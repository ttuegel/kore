{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Step.Rule.Simplify
    ( SimplifyRuleLHS (..)
    ) where

import Prelude.Kore

import qualified Control.Lens as Lens
import Control.Monad
    ( (>=>)
    )

import Branch
    ( BranchT
    )
import qualified Branch
import Kore.Internal.Conditional
    ( Conditional (Conditional)
    )
import qualified Kore.Internal.Conditional as Conditional.DoNotUse
    ( Conditional (..)
    )
import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import qualified Kore.Internal.MultiAnd as MultiAnd
    ( make
    )
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
    ( coerceSort
    )
import Kore.Internal.TermLike
    ( termLikeSort
    )
import Kore.Step.RulePattern
    ( AllPathRule (..)
    , OnePathRule (..)
    , ReachabilityRule (..)
    , RewriteRule (..)
    , RulePattern (RulePattern)
    )
import qualified Kore.Step.RulePattern as RulePattern
    ( RulePattern (..)
    , applySubstitution
    , leftPattern
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
import Kore.Step.Simplification.Simplify
    ( InternalVariable
    , MonadSimplify
    )
import qualified Kore.Step.SMT.Evaluator as SMT.Evaluator
import Kore.Syntax.Variable
    ( VariableName
    )

-- | Simplifies the left-hand-side of a rule/claim
class SimplifyRuleLHS rule where
    simplifyRuleLhs
        :: forall simplifier
        .  MonadSimplify simplifier
        => rule
        -> simplifier (MultiAnd rule)

instance InternalVariable variable => SimplifyRuleLHS (RulePattern variable)
  where
    simplifyRuleLhs rule@(RulePattern _ _ _ _ _) = do
        let lhsWithPredicate = Pattern.fromTermLike left
        simplifiedTerms <-
            Pattern.simplifyTopConfiguration lhsWithPredicate
        fullySimplified <- SMT.Evaluator.filterMultiOr simplifiedTerms
        let rules =
                map (setRuleLeft rule) (MultiOr.extractPatterns fullySimplified)
        return (MultiAnd.make rules)
      where
        RulePattern {left} = rule

        setRuleLeft
            :: RulePattern variable
            -> Pattern variable
            -> RulePattern variable
        setRuleLeft
            rulePattern@RulePattern {requires = requires'}
            Conditional {term, predicate, substitution}
          =
            RulePattern.applySubstitution
                substitution
                rulePattern
                    { RulePattern.left = term
                    , RulePattern.requires =
                        Predicate.coerceSort (termLikeSort term)
                        $ makeAndPredicate predicate requires'
                    }

instance SimplifyRuleLHS (RewriteRule VariableName) where
    simplifyRuleLhs =
        fmap (fmap RewriteRule) . simplifyRuleLhs . getRewriteRule

instance SimplifyRuleLHS OnePathRule where
    simplifyRuleLhs =
        fmap (fmap OnePathRule) . simplifyClaimRule . getOnePathRule

instance SimplifyRuleLHS AllPathRule where
    simplifyRuleLhs =
        fmap (fmap AllPathRule) . simplifyClaimRule . getAllPathRule

instance SimplifyRuleLHS ReachabilityRule where
    simplifyRuleLhs (OnePath rule) =
        (fmap . fmap) OnePath $ simplifyRuleLhs rule
    simplifyRuleLhs (AllPath rule) =
        (fmap . fmap) AllPath $ simplifyRuleLhs rule

simplifyClaimRule
    :: forall simplifier variable
    .  MonadSimplify simplifier
    => InternalVariable variable
    => RulePattern variable
    -> simplifier (MultiAnd (RulePattern variable))
simplifyClaimRule =
    fmap MultiAnd.make . Branch.gather . worker
  where
    simplify, filterWithSolver
        :: Pattern variable
        -> BranchT simplifier (Pattern variable)
    simplify =
        (return . Pattern.requireDefined)
        >=> Pattern.simplifyTopConfiguration
        >=> Branch.scatter
        >=> filterWithSolver
    filterWithSolver = SMT.Evaluator.filterBranch

    worker :: RulePattern variable -> BranchT simplifier (RulePattern variable)
    worker rulePattern = do
        let lhs = Lens.view RulePattern.leftPattern rulePattern
        simplified <- simplify lhs
        let substitution = Pattern.substitution simplified
            lhs' = simplified { Pattern.substitution = mempty }
        rulePattern
            & Lens.set RulePattern.leftPattern lhs'
            & RulePattern.applySubstitution substitution
            & return
