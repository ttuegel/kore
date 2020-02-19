{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Step.Rule.Simplify
    ( SimplifyRuleLHS (..)
    ) where

import Prelude.Kore

import qualified Branch
import Kore.Internal.Condition
    ( Condition
    )
import qualified Kore.Internal.Condition as Condition
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
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , makeAndPredicate
    , makeCeilPredicate_
    , makeTruePredicate_
    )
import qualified Kore.Internal.Predicate as Predicate
    ( coerceSort
    , unwrapPredicate
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.TermLike
    ( mkAnd
    , mkCeil_
    , termLikeSort
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
    )
import Kore.Step.Simplification.OrPattern
    ( simplifyConditionsWithSmt
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
import Kore.Step.Simplification.Simplify
    ( InternalVariable
    , MonadSimplify
    , simplifyCondition
    )
import Kore.Syntax.Variable
    ( Variable
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
        fullySimplified <-
            simplifyConditionsWithSmt
                SideCondition.top
                simplifiedTerms
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

instance SimplifyRuleLHS (RewriteRule Variable) where
    simplifyRuleLhs =
        fmap (fmap RewriteRule) . simplifyRuleLhs . getRewriteRule

instance SimplifyRuleLHS (OnePathRule Variable) where
    simplifyRuleLhs =
        fmap (fmap OnePathRule) . simplifyClaimRule . getOnePathRule

instance SimplifyRuleLHS (AllPathRule Variable) where
    simplifyRuleLhs =
        fmap (fmap AllPathRule) . simplifyClaimRule . getAllPathRule

instance SimplifyRuleLHS (ReachabilityRule Variable) where
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
simplifyClaimRule rule@(RulePattern _ _ _ _ _) =
    fmap MultiAnd.make . Branch.gather $ do
        let requires1 = makeAndPredicate requires (makeCeilPredicate_ left)
        let left0 = Pattern.withCondition left $ from @(Predicate _) requires1
        left1 <- Pattern.simplifyTopConfiguration left0 >>= Branch.scatter
        let left2 = OrPattern.fromPattern left1
        left3 <-
            simplifyConditionsWithSmt SideCondition.top left2
            >>= Branch.scatter
        return (setRuleLeft rule left3)
  where
    RulePattern { left, requires } = rule

    setRuleLeft
        :: RulePattern variable
        -> Pattern variable
        -> RulePattern variable
    setRuleLeft
        rulePattern
        Conditional { term, predicate, substitution }
      =
        RulePattern.applySubstitution
            substitution
            rulePattern
                { RulePattern.left = term
                , RulePattern.requires =
                    Predicate.coerceSort (termLikeSort term) predicate
                }
