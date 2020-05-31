{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

Direct interface to rule application (step-wise execution).
See "Kore.Step" for the high-level strategy-based interface.
 -}

module Kore.Step.RewriteStep
    ( applyRewriteRulesParallel
    , withoutUnification
    , applyRewriteRulesSequence
    ) where

import Prelude.Kore

import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq

import qualified Branch
import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    )
import qualified Kore.Internal.Condition as Condition
import Kore.Internal.Conditional
    ( Conditional (Conditional)
    )
import qualified Kore.Internal.Conditional as Conditional
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrCondition
    ( OrCondition
    )
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import qualified Kore.Internal.SideCondition as SideCondition
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike as TermLike
import Kore.Log.DebugAppliedRewriteRules
    ( debugAppliedRewriteRules
    )
import Kore.Log.ErrorRewritesInstantiation
    ( checkSubstitutionCoverage
    )
import Kore.Rewriting.RewritingVariable
import qualified Kore.Step.Remainder as Remainder
import qualified Kore.Step.Result as Result
import qualified Kore.Step.Result as Step
import Kore.Step.RulePattern
    ( RewriteRule (..)
    , RulePattern
    )
import qualified Kore.Step.RulePattern as Rule
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    , simplifyCondition
    )
import Kore.Step.Step
    ( Result
    , Results
    , UnificationProcedure (..)
    , UnifiedRule
    , applyInitialConditions
    , applyRemainder
    , assertFunctionLikeResults
    , mkRewritingPattern
    , unifyRules
    )

withoutUnification :: UnifiedRule rule variable -> rule variable
withoutUnification = Conditional.term

{- | Produce the final configurations of an applied rule.

The rule's 'ensures' clause is applied to the conditions and normalized. The
substitution is applied to the right-hand side of the rule to produce the final
configurations.

Because the rule is known to apply, @finalizeAppliedRule@ always returns exactly
one branch.

See also: 'applyInitialConditions'

 -}
finalizeAppliedRule
    :: forall simplifier
    .  MonadSimplify simplifier
    => RulePattern RewritingVariableName
    -- ^ Applied rule
    -> OrCondition RewritingVariableName
    -- ^ Conditions of applied rule
    -> simplifier (OrPattern RewritingVariableName)
finalizeAppliedRule renamedRule appliedConditions =
    MultiOr.gather
    $ finalizeAppliedRuleWorker =<< Branch.scatter appliedConditions
  where
    ruleRHS = Rule.rhs renamedRule
    finalizeAppliedRuleWorker appliedCondition = do
        -- Combine the initial conditions, the unification conditions, and the
        -- axiom ensures clause. The axiom requires clause is included by
        -- unifyRule.
        let
            avoidVars = freeVariables appliedCondition <> freeVariables ruleRHS
            finalPattern =
                Rule.topExistsToImplicitForall avoidVars ruleRHS
            Conditional { predicate = ensures } = finalPattern
            ensuresCondition = Condition.fromPredicate ensures
        finalCondition <-
            do
                partial <-
                    simplifyCondition
                        (SideCondition.fromCondition appliedCondition)
                        ensuresCondition
                simplifyCondition SideCondition.top
                    (appliedCondition <> partial)
            & Branch.alternate
        -- Apply the normalized substitution to the right-hand side of the
        -- axiom.
        let
            Conditional { substitution } = finalCondition
            substitution' = Substitution.toMap substitution
            Conditional { term = finalTerm} = finalPattern
            finalTerm' = TermLike.substitute substitution' finalTerm
        return (finalTerm' `Pattern.withCondition` finalCondition)

finalizeRule
    :: MonadSimplify simplifier
    => FreeVariables RewritingVariableName
    -> Pattern RewritingVariableName
    -- ^ Initial conditions
    -> UnifiedRule RulePattern RewritingVariableName
    -- ^ Rewriting axiom
    -> simplifier [Result RulePattern VariableName]
finalizeRule initialVariables initial unifiedRule =
    Branch.gather $ do
        let initialCondition = Conditional.withoutTerm initial
        let unificationCondition = Conditional.withoutTerm unifiedRule
        applied <- applyInitialConditions initialCondition unificationCondition
        checkSubstitutionCoverage initial (RewriteRule <$> unifiedRule)
        let renamedRule = Conditional.term unifiedRule
        final <- finalizeAppliedRule renamedRule applied
        let result = getResultPattern initialVariables <$> final
        return Step.Result { appliedRule = unifiedRule, result }

-- | Finalizes a list of applied rules into 'Results'.
type Finalizer simplifier =
        MonadSimplify simplifier
    =>  FreeVariables RewritingVariableName
    ->  Pattern RewritingVariableName
    ->  [UnifiedRule RulePattern RewritingVariableName]
    ->  simplifier (Results RulePattern VariableName)

finalizeRulesParallel :: forall simplifier. Finalizer simplifier
finalizeRulesParallel initialVariables initial unifiedRules = do
    results <-
        traverse (finalizeRule initialVariables initial) unifiedRules
        & fmap Foldable.fold
    let unifications = MultiOr.make (Conditional.withoutTerm <$> unifiedRules)
        remainder = Condition.fromPredicate (Remainder.remainder' unifications)
    remainders' <- applyRemainder initial remainder & Branch.gather
    return Step.Results
        { results = Seq.fromList results
        , remainders =
            OrPattern.fromPatterns $ getRemainderPattern <$> remainders'
        }

finalizeRulesSequence :: forall simplifier. Finalizer simplifier
finalizeRulesSequence initialVariables initial unifiedRules = do
    (results, remainder) <-
        State.runStateT
            (traverse finalizeRuleSequence' unifiedRules)
            (Conditional.withoutTerm initial)
    remainders' <- applyRemainder initial remainder & Branch.gather
    return Step.Results
        { results = Seq.fromList $ Foldable.fold results
        , remainders =
            OrPattern.fromPatterns $ getRemainderPattern <$> remainders'
        }
  where
    initialTerm = Conditional.term initial
    finalizeRuleSequence' unifiedRule = do
        remainder <- State.get
        let remainderPattern = Conditional.withCondition initialTerm remainder
        results <-
            finalizeRule initialVariables remainderPattern unifiedRule
            & Monad.Trans.lift
        let unification = Conditional.withoutTerm unifiedRule
            remainder' =
                Condition.fromPredicate
                $ Remainder.remainder'
                $ MultiOr.singleton unification
        State.put (remainder `Conditional.andCondition` remainder')
        return results

applyRulesWithFinalizer
    :: forall simplifier
    .  MonadSimplify simplifier
    => Finalizer simplifier
    -> UnificationProcedure simplifier
    -> [RulePattern RewritingVariableName]
    -- ^ Rewrite rules
    -> Pattern RewritingVariableName
    -- ^ Configuration being rewritten
    -> simplifier (Results RulePattern VariableName)
applyRulesWithFinalizer finalize unificationProcedure rules initial = do
    results <- unifyRules unificationProcedure initial rules
    debugAppliedRewriteRules initial results
    let initialVariables = freeVariables initial
    finalize initialVariables initial results
{-# INLINE applyRulesWithFinalizer #-}

{- | Apply the given rules to the initial configuration in parallel.

See also: 'applyRewriteRule'

 -}
applyRulesParallel
    :: forall simplifier
    .  MonadSimplify simplifier
    => UnificationProcedure simplifier
    -> [RulePattern RewritingVariableName]
    -- ^ Rewrite rules
    -> Pattern RewritingVariableName
    -- ^ Configuration being rewritten
    -> simplifier (Results RulePattern VariableName)
applyRulesParallel = applyRulesWithFinalizer finalizeRulesParallel

{- | Apply the given rewrite rules to the initial configuration in parallel.

See also: 'applyRewriteRule'

 -}
applyRewriteRulesParallel
    :: forall simplifier
    .  MonadSimplify simplifier
    => UnificationProcedure simplifier
    -> [RewriteRule RewritingVariableName]
    -- ^ Rewrite rules
    -> Pattern VariableName
    -- ^ Configuration being rewritten
    -> simplifier (Results RulePattern VariableName)
applyRewriteRulesParallel
    unificationProcedure
    (map getRewriteRule -> rules)
    (mkRewritingPattern -> initial)
  = do
    results <- applyRulesParallel unificationProcedure rules initial
    assertFunctionLikeResults (term initial) results
    return results


{- | Apply the given rewrite rules to the initial configuration in sequence.

See also: 'applyRewriteRule'

 -}
applyRulesSequence
    :: forall simplifier
    .  MonadSimplify simplifier
    => UnificationProcedure simplifier
    -> [RulePattern RewritingVariableName]
    -- ^ Rewrite rules
    -> Pattern RewritingVariableName
    -- ^ Configuration being rewritten
    -> simplifier (Results RulePattern VariableName)
applyRulesSequence = applyRulesWithFinalizer finalizeRulesSequence

{- | Apply the given rewrite rules to the initial configuration in sequence.

See also: 'applyRewriteRulesParallel'

 -}
applyRewriteRulesSequence
    :: forall simplifier
    .  MonadSimplify simplifier
    => UnificationProcedure simplifier
    -> Pattern VariableName
    -- ^ Configuration being rewritten
    -> [RewriteRule RewritingVariableName]
    -- ^ Rewrite rules
    -> simplifier (Results RulePattern VariableName)
applyRewriteRulesSequence
    unificationProcedure
    (mkRewritingPattern -> initialConfig)
    (map getRewriteRule -> rules)
  = do
    results <- applyRulesSequence unificationProcedure rules initialConfig
    assertFunctionLikeResults (term initialConfig) results
    return results
