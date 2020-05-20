{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

Unification of rules (used for stepping with rules or equations)

 -}
module Kore.Step.Step
    ( UnifiedRule
    , Result
    , Results
    , Renaming
    , UnifyingRule (..)
    , InstantiationFailure (..)
    , unifyRules
    , unifyRule
    , applyInitialConditions
    , applyRemainder
    , simplifyPredicate
    , toConfigurationVariablesCondition
    , assertFunctionLikeResults
    , checkFunctionLike
    , wouldNarrowWith
    -- * Re-exports
    , UnificationProcedure (..)
    , unRewritingRule
    , mkRewritingPattern
    -- Below exports are just for tests
    , Step.gatherResults
    , Step.remainders
    , Step.result
    , Step.results
    ) where

import Prelude.Kore

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Data.Text.Prettyprint.Doc as Pretty

import Branch
    ( BranchT
    )
import qualified Branch
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import Kore.Internal.Condition
    ( Condition
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
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( andCondition
    , mapVariables
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
    ( InternalVariable
    , TermLike
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Rewriting.RewritingVariable
import Kore.Rewriting.UnifyingRule
import qualified Kore.Step.Result as Result
import qualified Kore.Step.Result as Results
import qualified Kore.Step.Result as Step
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    )
import qualified Kore.Step.Simplification.Simplify as Simplifier
import qualified Kore.Step.SMT.Evaluator as SMT.Evaluator
import qualified Kore.TopBottom as TopBottom
import Kore.Unification.UnificationProcedure
import Kore.Unparser
import Kore.Variables.Target
    ( Target
    )
import qualified Kore.Variables.Target as Target
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable
    )

type UnifiedRule rule variable = Conditional variable (rule variable)

type Result rule variable =
    Step.Result
        (UnifiedRule rule RewritingVariable)
        (Pattern variable)

type Results rule variable =
    Step.Results
        (UnifiedRule rule RewritingVariable)
        (Pattern variable)

-- |Unifies/matches a list a rules against a configuration. See 'unifyRule'.
unifyRules
    :: MonadSimplify simplifier
    => UnifyingRule rule
    => UnificationProcedure simplifier
    -> SideCondition RewritingVariable
    -> Pattern RewritingVariable
    -- ^ Initial configuration
    -> [rule RewritingVariable]
    -- ^ Rule
    -> simplifier [UnifiedRule rule RewritingVariable]
unifyRules unificationProcedure sideCondition initial rules =
    Branch.gather $ do
        rule <- Branch.scatter rules
        unifyRule unificationProcedure sideCondition initial rule

{- | Attempt to unify a rule with the initial configuration.

The rule variables are renamed to avoid collision with the configuration. The
rule's 'RulePattern.requires' clause is combined with the unification
solution. The combined condition is simplified and checked for
satisfiability.

If any of these steps produces an error, then @unifyRule@ returns that error.

@unifyRule@ returns the renamed rule wrapped with the combined conditions on
unification. The substitution is not applied to the renamed rule.

 -}
unifyRule
    :: InternalVariable variable
    => MonadSimplify simplifier
    => UnifyingRule rule
    => UnificationProcedure simplifier
    -> SideCondition variable
    -- ^ Top level condition.
    -> Pattern variable
    -- ^ Initial configuration
    -> rule variable
    -- ^ Rule
    -> BranchT simplifier (UnifiedRule rule variable)
unifyRule unificationProcedure sideCondition initial rule = do
    let (initialTerm, initialCondition) = Pattern.splitTerm initial
        mergedSideCondition =
            sideCondition `SideCondition.andCondition` initialCondition
    -- Unify the left-hand side of the rule with the term of the initial
    -- configuration.
    let ruleLeft = matchingPattern rule
    unification <-
        unifyTermLikes mergedSideCondition initialTerm ruleLeft
    -- Combine the unification solution with the rule's requirement clause,
    let ruleRequires = precondition rule
        requires' = Condition.fromPredicate ruleRequires
    unification' <-
        simplifyPredicate mergedSideCondition Nothing (unification <> requires')
    return (rule `Conditional.withCondition` unification')
  where
    unifyTermLikes = runUnificationProcedure unificationProcedure

{- | The 'Set' of variables that would be introduced by narrowing.
 -}
-- TODO (thomas.tuegel): Unit tests
wouldNarrowWith
    :: forall rule variable
    .  Ord variable
    => UnifyingRule rule
    => UnifiedRule rule variable
    -> Set (UnifiedVariable variable)
wouldNarrowWith unified =
    Set.difference leftAxiomVariables substitutionVariables
  where
    leftAxiomVariables = TermLike.freeVariables leftAxiom & FreeVariables.toSet
      where
        Conditional { term = axiom } = unified
        leftAxiom = matchingPattern axiom
    Conditional { substitution } = unified
    substitutionVariables = Map.keysSet (Substitution.toMap substitution)

-- |Errors if configuration or matching pattern are not function-like
assertFunctionLikeResults
    :: InternalVariable variable
    => Monad m
    => UnifyingRule rule
    => Eq (rule RewritingVariable)
    => TermLike variable
    -> Results rule variable'
    -> m ()
assertFunctionLikeResults termLike results =
    let appliedRules = Result.appliedRule <$> Results.results results
    in case checkFunctionLike appliedRules termLike of
        Left err -> error err
        _        -> return ()

-- |Checks whether configuration and matching pattern are function-like
checkFunctionLike
    :: InternalVariable variable
    => InternalVariable variable'
    => Foldable f
    => UnifyingRule rule
    => Eq (f (UnifiedRule rule variable'))
    => Monoid (f (UnifiedRule rule variable'))
    => f (UnifiedRule rule variable')
    -> TermLike variable
    -> Either String ()
checkFunctionLike unifiedRules pat
  | unifiedRules == mempty = pure ()
  | TermLike.isFunctionPattern pat =
    Foldable.traverse_ checkFunctionLikeRule unifiedRules
  | otherwise = Left . show . Pretty.vsep $
    [ "Expected function-like term, but found:"
    , Pretty.indent 4 (unparse pat)
    ]
  where
    checkFunctionLikeRule Conditional { term }
      | TermLike.isFunctionPattern left = return ()
      | otherwise = Left . show . Pretty.vsep $
        [ "Expected function-like left-hand side of rule, but found:"
        , Pretty.indent 4 (unparse left)
        ]
      where
        left = matchingPattern term

{- | Apply the initial conditions to the results of rule unification.

The rule is considered to apply if the result is not @\\bottom@.

 -}
applyInitialConditions
    :: forall simplifier variable
    .  InternalVariable variable
    => MonadSimplify simplifier
    => SideCondition variable
    -- ^ Top-level conditions
    -> Maybe (Condition variable)
    -- ^ Initial conditions
    -> Condition variable
    -- ^ Unification conditions
    -> BranchT simplifier (OrCondition variable)
    -- TODO(virgil): This should take advantage of the BranchT and not return
    -- an OrCondition.
applyInitialConditions sideCondition initial unification = do
    -- Combine the initial conditions and the unification conditions.
    -- The axiom requires clause is included in the unification conditions.
    applied <-
        simplifyPredicate sideCondition initial unification
        & MultiOr.gather
    evaluated <- SMT.Evaluator.filterMultiOr applied
    -- If 'evaluated' is \bottom, the rule is considered to not apply and
    -- no result is returned. If the result is \bottom after this check,
    -- then the rule is considered to apply with a \bottom result.
    TopBottom.guardAgainstBottom evaluated
    return evaluated

-- |Renames configuration variables to distinguish them from those in the rule.
toConfigurationVariablesCondition
    :: InternalVariable variable
    => SideCondition variable
    -> SideCondition (Target variable)
toConfigurationVariablesCondition =
    SideCondition.mapVariables Target.mkElementNonTarget Target.mkSetNonTarget

{- | Apply the remainder predicate to the given initial configuration.

 -}
applyRemainder
    :: forall simplifier variable
    .  InternalVariable variable
    => MonadSimplify simplifier
    => SideCondition variable
    -- ^ Top level condition
    -> Pattern variable
    -- ^ Initial configuration
    -> Condition variable
    -- ^ Remainder
    -> BranchT simplifier (Pattern variable)
applyRemainder sideCondition initial remainder = do
    let (initialTerm, initialCondition) = Pattern.splitTerm initial
    normalizedCondition <-
        simplifyPredicate sideCondition (Just initialCondition) remainder
    return normalizedCondition { Conditional.term = initialTerm }

-- | Simplifies the predicate obtained upon matching/unification.
simplifyPredicate
    :: forall simplifier variable term
    .  InternalVariable variable
    => MonadSimplify simplifier
    => SideCondition variable
    -> Maybe (Condition variable)
    -> Conditional variable term
    -> BranchT simplifier (Conditional variable term)
simplifyPredicate sideCondition (Just initialCondition) conditional = do
    partialResult <-
        Simplifier.simplifyCondition
            (sideCondition `SideCondition.andCondition` initialCondition)
            conditional
    -- TODO (virgil): Consider using different simplifyPredicate implementations
    -- for rewrite rules and equational rules.
    -- Right now this double simplification both allows using the same code for
    -- both kinds of rules, and allows using the strongest background condition
    -- for simplifying the `conditional`. However, it's not obvious that
    -- using the strongest background condition actually helps in our
    -- use cases, so we may be able to do something better for equations.
    Simplifier.simplifyCondition
        sideCondition
        ( partialResult
        `Pattern.andCondition` initialCondition
        )
simplifyPredicate sideCondition Nothing conditional =
    Simplifier.simplifyCondition
        sideCondition
        conditional
