{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}
module Kore.Reachability.Claim
    ( Claim (..)
    , AppliedRule (..)
    , strategy
    , TransitionRule
    , Prim
    , ClaimExtractor (..)
    , WithConfiguration (..)
    , CheckImplicationResult (..)
    , extractClaims
    , reachabilityFirstStep
    , reachabilityNextStep
    , transitionRule
    , isTrusted
    -- * Re-exports
    , ReachabilityClaim (..)
    , AllPathClaim (..)
    , OnePathClaim (..)
    , Rule (..)
    , RewriteRule (..)
    , module Kore.Log.InfoReachability
    , getConfiguration
    , getDestination
    -- * For testing
    , checkImplicationWorker
    ) where

import Prelude.Kore

import Control.Lens
    ( Lens'
    )
import qualified Control.Lens as Lens
import Control.Monad
    ( foldM
    )
import Control.Monad.Catch
    ( Exception (..)
    , SomeException (..)
    )
import Control.Monad.State.Strict
    ( MonadState
    , StateT
    , runStateT
    )
import qualified Control.Monad.State.Strict as State
import Data.Coerce
    ( coerce
    )
import qualified Data.Foldable as Foldable
import Data.Functor.Compose
import Data.Generics.Product
    ( field
    )
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import qualified Data.Monoid as Monoid
import Data.Stream.Infinite
    ( Stream (..)
    )
import qualified Data.Stream.Infinite as Stream
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Debug
import qualified Kore.Attribute.Axiom as Attribute.Axiom
import qualified Kore.Attribute.Label as Attribute
    ( Label
    )
import qualified Kore.Attribute.RuleIndex as Attribute
    ( RuleIndex
    )
import Kore.Reachability.Prim
import qualified Kore.Attribute.SourceLocation as Attribute
    ( SourceLocation
    )
import qualified Kore.Attribute.Trusted as Attribute.Trusted
import Kore.IndexedModule.IndexedModule
    ( IndexedModule (indexedModuleClaims)
    , VerifiedModule
    )
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.Conditional as Conditional
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeCeilPredicate_
    )
import qualified Kore.Internal.SideCondition as SideCondition
import Kore.Internal.Symbol
    ( Symbol
    )
import Kore.Internal.TermLike
    ( isFunctionPattern
    , mkDefined
    , mkIn
    , termLikeSort
    )
import Kore.Log.InfoReachability
import Kore.Log.WarnStuckClaimState
    ( warnStuckClaimStateTermsNotUnifiable
    , warnStuckClaimStateTermsUnifiable
    )
import Kore.Rewriting.RewritingVariable
import Kore.Step.AxiomPattern
    ( AxiomPattern (..)
    )
import Kore.Step.ClaimPattern
    ( AllPathClaim (..)
    , ClaimPattern (..)
    , OnePathClaim (..)
    , ReachabilityClaim (..)
    , getConfiguration
    , getDestination
    )
import qualified Kore.Step.ClaimPattern as ClaimPattern
import Kore.Step.Result
    ( Result (..)
    , Results (..)
    )
import qualified Kore.Step.RewriteStep as Step
import Kore.Step.Rule
    ( QualifiedAxiomPattern (..)
    , fromSentenceAxiom
    )
import Kore.Step.RulePattern
    ( RulePattern (..)
    , RewriteRule (..)
    )
import Kore.Step.Simplification.Data
    ( MonadSimplify
    )
import qualified Kore.Step.Simplification.Exists as Exists
import qualified Kore.Step.Simplification.Not as Not
import Kore.Step.Simplification.OrPattern
    ( simplifyConditionsWithSmt
    )
import Kore.Step.Simplification.Pattern
    ( simplifyTopConfiguration
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
import qualified Kore.Step.SMT.Evaluator as SMT.Evaluator
import qualified Kore.Step.Step as Step
import Kore.Step.Strategy
    ( Strategy
    )
import qualified Kore.Step.Strategy as Strategy
import qualified Kore.Step.Transition as Transition
import Kore.Reachability.ClaimState hiding
    ( claimState
    )
import qualified Kore.Reachability.ClaimState as ClaimState
import qualified Kore.Syntax.Sentence as Syntax
import Kore.Syntax.Variable
import Kore.TopBottom
    ( isBottom
    )
import qualified Kore.Unification.Procedure as Unification
import Kore.Unparser
    ( Unparse (..)
    )
import qualified Kore.Verified as Verified
import Logic
    ( LogicT
    , MonadLogic
    )
import qualified Logic
import qualified Pretty
import qualified SMT

class Claim claim where
    {- | @Rule claim@ is the type of rule to take a single step toward @claim@.
    -}
    data family Rule claim

    checkImplication
        :: MonadSimplify m
        => claim
        -> LogicT m (CheckImplicationResult claim)

    simplify
        :: MonadSimplify m
        => claim
        -> Strategy.TransitionT (AppliedRule claim) m claim

    {- TODO (thomas.tuegel): applyClaims and applyAxioms should return:

    > data ApplyResult claim
    >     = ApplyRewritten !claim
    >     | ApplyRemainder !claim

    Rationale: ClaimState is part of the implementation of transitionRule, that
    is: these functions have hidden knowledge of how transitionRule works
    because they tell it what to do next. Instead, they should report their
    result and leave the decision up to transitionRule.

    -}
    applyClaims
        :: MonadSimplify m
        => [claim]
        -> claim
        -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)

    applyAxioms
        :: MonadSimplify m
        => [[Rule claim]]
        -> claim
        -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)

data AppliedRule claim
    = AppliedAxiom (Rule claim)
    | AppliedClaim claim
    deriving (GHC.Generic)

instance SOP.Generic claim => SOP.Generic (AppliedRule claim)

instance SOP.HasDatatypeInfo claim => SOP.HasDatatypeInfo (AppliedRule claim)

instance
    ( Debug claim
    , SOP.HasDatatypeInfo claim
    , Debug (Rule claim)
    , SOP.HasDatatypeInfo (Rule claim)
    ) => Debug (AppliedRule claim)

instance
    ( Diff claim
    , Debug claim
    , SOP.HasDatatypeInfo claim
    , Diff (Rule claim)
    , Debug (Rule claim)
    , SOP.HasDatatypeInfo (Rule claim)
    ) => Diff (AppliedRule claim)

instance (From claim Attribute.Label, From (Rule claim) Attribute.Label)
  => From (AppliedRule claim) Attribute.Label
  where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance (From claim Attribute.RuleIndex, From (Rule claim) Attribute.RuleIndex)
  => From (AppliedRule claim) Attribute.RuleIndex
  where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance
    ( From claim Attribute.SourceLocation
    , From (Rule claim) Attribute.SourceLocation
    )
    => From (AppliedRule claim) Attribute.SourceLocation
  where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance (Unparse claim, Unparse (Rule claim)) => Unparse (AppliedRule claim)
  where
    unparse (AppliedAxiom rule) = unparse rule
    unparse (AppliedClaim claim) = unparse claim

    unparse2 (AppliedAxiom rule) = unparse2 rule
    unparse2 (AppliedClaim claim) = unparse2 claim

type AxiomAttributes = Attribute.Axiom.Axiom Symbol VariableName

class ClaimExtractor claim where
    extractClaim :: (AxiomAttributes, Verified.SentenceClaim) -> Maybe claim

-- | Extracts all One-Path claims from a verified module.
extractClaims
    :: ClaimExtractor claim
    => VerifiedModule declAtts
    -- ^ 'IndexedModule' containing the definition
    -> [claim]
extractClaims = mapMaybe extractClaim . indexedModuleClaims

{- NOTE: Non-deterministic semantics

The current implementation of one-path verification assumes that the proof claim
is deterministic, that is: the proof claim would not be discharged during at a
non-confluent state in the execution of a non-deterministic semantics. (Often
this means that the definition is simply deterministic.) As a result, given the
non-deterministic definition

> module ABC
>   import DOMAINS
>   syntax S ::= "a" | "b" | "c"
>   rule [ab]: a => b
>   rule [ac]: a => c
> endmodule

this claim would be provable,

> rule a => b [claim]

but this claim would **not** be provable,

> rule a => c [claim]

because the algorithm would first apply semantic rule [ab], which prevents rule
[ac] from being used.

We decided to assume that the definition is deterministic because one-path
verification is mainly used only for deterministic semantics and the assumption
simplifies the implementation. However, this assumption is not an essential
feature of the algorithm. You should not rely on this assumption elsewhere. This
decision is subject to change without notice.

This instance contains the default implementation for a one-path strategy. You
can apply it to the first two arguments and pass the resulting function to
'Kore.Strategies.Verification.verify'.

Things to note when implementing your own:

1. The first step does not use the reachability claims

2. You can return an infinite list.
-}

instance Claim OnePathClaim where

    newtype instance Rule OnePathClaim =
        OnePathRewriteRule
        { unRuleOnePath :: RewriteRule RewritingVariableName }
        deriving (GHC.Generic, Show, Unparse)

    simplify = simplify' _Unwrapped

    checkImplication = checkImplication' _Unwrapped

    applyClaims claims = deriveSeqClaim _Unwrapped OnePathClaim claims

    applyAxioms axioms = deriveSeqAxiomOnePath (concat axioms)

instance SOP.Generic (Rule OnePathClaim)

instance SOP.HasDatatypeInfo (Rule OnePathClaim)

instance Debug (Rule OnePathClaim)

instance Diff (Rule OnePathClaim)

instance From (Rule OnePathClaim) Attribute.Axiom.PriorityAttributes where
    from = from @(RewriteRule _) . unRuleOnePath

deriveSeqClaim
    :: MonadSimplify m
    => Step.UnifyingRule claim
    => Step.UnifyingRuleVariable claim ~ RewritingVariableName
    => From claim (AxiomPattern RewritingVariableName)
    => From claim Attribute.SourceLocation
    => Lens' claim ClaimPattern
    -> (ClaimPattern -> claim)
    -> [claim]
    -> claim
    -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)
deriveSeqClaim lensClaimPattern mkClaim claims claim =
    getCompose
    $ Lens.forOf lensClaimPattern claim
    $ \claimPattern ->
        fmap (snd . Step.refreshRule mempty)
        $ Lens.forOf (field @"left") claimPattern
        $ \config -> Compose $ do
            results <-
                Step.applyClaimsSequence
                    mkClaim
                    Unification.unificationProcedure
                    config
                    (Lens.view lensClaimPattern <$> claims)
                    & lift
            deriveResults fromAppliedRule results
  where
    fromAppliedRule =
        AppliedClaim
        . mkClaim
        . Step.withoutUnification

deriveSeqAxiomOnePath
    ::  MonadSimplify simplifier
    =>  [Rule OnePathClaim]
    ->  OnePathClaim
    ->  Strategy.TransitionT (AppliedRule OnePathClaim) simplifier
            (ClaimState OnePathClaim)
deriveSeqAxiomOnePath rules =
    deriveSeq' _Unwrapped OnePathRewriteRule rewrites
  where
    rewrites = unRuleOnePath <$> rules

instance ClaimExtractor OnePathClaim where
    extractClaim (attrs, sentence) =
        case fromSentenceAxiom (attrs, Syntax.getSentenceClaim sentence) of
            Right (OnePathClaimPattern claim) -> Just claim
            _ -> Nothing

instance Claim AllPathClaim where

    newtype instance Rule AllPathClaim =
        AllPathRewriteRule
        { unRuleAllPath :: RewriteRule RewritingVariableName }
        deriving (GHC.Generic, Show, Unparse)

    simplify = simplify' _Unwrapped
    checkImplication = checkImplication' _Unwrapped
    applyClaims claims = deriveSeqClaim _Unwrapped AllPathClaim claims

    applyAxioms axiomss = \claim ->
        foldM applyAxioms1 (Remaining claim) axiomss
      where
        applyAxioms1 claimState axioms
          | Just claim <- retractRewritable claimState =
            deriveParAxiomAllPath axioms claim
            >>= simplifyRemainder
          | otherwise =
            pure claimState

        simplifyRemainder claimState =
            case claimState of
                Remaining claim -> Remaining <$> simplify claim
                _ -> return claimState

instance SOP.Generic (Rule AllPathClaim)

instance SOP.HasDatatypeInfo (Rule AllPathClaim)

instance Debug (Rule AllPathClaim)

instance Diff (Rule AllPathClaim)

instance From (Rule AllPathClaim) Attribute.Axiom.PriorityAttributes where
    from = from @(RewriteRule _) . unRuleAllPath

deriveParAxiomAllPath
    ::  MonadSimplify simplifier
    =>  [Rule AllPathClaim]
    ->  AllPathClaim
    ->  Strategy.TransitionT (AppliedRule AllPathClaim) simplifier
            (ClaimState AllPathClaim)
deriveParAxiomAllPath rules =
    derivePar' _Unwrapped AllPathRewriteRule rewrites
  where
    rewrites = unRuleAllPath <$> rules

instance ClaimExtractor AllPathClaim where
    extractClaim (attrs, sentence) =
        case fromSentenceAxiom (attrs, Syntax.getSentenceClaim sentence) of
            Right (AllPathClaimPattern claim) -> Just claim
            _ -> Nothing

instance Claim ReachabilityClaim where

    newtype instance Rule ReachabilityClaim =
        ReachabilityRewriteRule
            { unReachabilityRewriteRule :: RewriteRule RewritingVariableName }
        deriving (GHC.Generic, Show, Unparse)

    simplify (AllPath claim) = allPathTransition $ AllPath <$> simplify claim
    simplify (OnePath claim) = onePathTransition $ OnePath <$> simplify claim

    checkImplication (AllPath claim) = fmap AllPath <$> checkImplication claim
    checkImplication (OnePath claim) = fmap OnePath <$> checkImplication claim

    applyClaims claims (AllPath claim) =
        applyClaims (mapMaybe maybeAllPath claims) claim
        & fmap (fmap AllPath)
        & allPathTransition
    applyClaims claims (OnePath claim) =
        applyClaims (mapMaybe maybeOnePath claims) claim
        & fmap (fmap OnePath)
        & onePathTransition

    applyAxioms axiomGroups (AllPath claim) =
        applyAxioms (coerce axiomGroups) claim
        & fmap (fmap AllPath)
        & allPathTransition
    applyAxioms axiomGroups (OnePath claim) =
        applyAxioms (coerce axiomGroups) claim
        & fmap (fmap OnePath)
        & onePathTransition

instance SOP.Generic (Rule ReachabilityClaim)

instance SOP.HasDatatypeInfo (Rule ReachabilityClaim)

instance Debug (Rule ReachabilityClaim)

instance Diff (Rule ReachabilityClaim)

instance From (Rule ReachabilityClaim) Attribute.Axiom.PriorityAttributes where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule ReachabilityClaim) Attribute.SourceLocation where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule ReachabilityClaim) Attribute.Label where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule ReachabilityClaim) Attribute.RuleIndex where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance ClaimExtractor ReachabilityClaim where
    extractClaim (attrs, sentence) =
        case fromSentenceAxiom (attrs, Syntax.getSentenceClaim sentence) of
            Right (OnePathClaimPattern claim) -> Just (OnePath claim)
            Right (AllPathClaimPattern claim) -> Just (AllPath claim)
            _ -> Nothing

allPathTransition
    :: Monad m
    => Strategy.TransitionT (AppliedRule AllPathClaim) m a
    -> Strategy.TransitionT (AppliedRule ReachabilityClaim) m a
allPathTransition = Transition.mapRules ruleAllPathToRuleReachability

onePathTransition
    :: Monad m
    => Strategy.TransitionT (AppliedRule OnePathClaim) m a
    -> Strategy.TransitionT (AppliedRule ReachabilityClaim) m a
onePathTransition = Transition.mapRules ruleOnePathToRuleReachability

maybeOnePath :: ReachabilityClaim -> Maybe OnePathClaim
maybeOnePath (OnePath rule) = Just rule
maybeOnePath _ = Nothing

maybeAllPath :: ReachabilityClaim -> Maybe AllPathClaim
maybeAllPath (AllPath rule) = Just rule
maybeAllPath _ = Nothing

ruleAllPathToRuleReachability
    :: AppliedRule AllPathClaim
    -> AppliedRule ReachabilityClaim
ruleAllPathToRuleReachability (AppliedAxiom (AllPathRewriteRule rewriteRule)) =
    AppliedAxiom (ReachabilityRewriteRule rewriteRule)
ruleAllPathToRuleReachability (AppliedClaim allPathRule) =
    AppliedClaim (AllPath allPathRule)

ruleOnePathToRuleReachability
    :: AppliedRule OnePathClaim
    -> AppliedRule ReachabilityClaim
ruleOnePathToRuleReachability (AppliedAxiom (OnePathRewriteRule rewriteRule)) =
    AppliedAxiom (ReachabilityRewriteRule rewriteRule)
ruleOnePathToRuleReachability (AppliedClaim onePathRule) =
    AppliedClaim (OnePath onePathRule)

type TransitionRule m rule state =
    Prim -> state -> Strategy.TransitionT rule m state

transitionRule
    :: forall m claim
    .  MonadSimplify m
    => Claim claim
    => [claim]
    -> [[Rule claim]]
    -> TransitionRule m (AppliedRule claim) (ClaimState claim)
transitionRule claims axiomGroups = transitionRuleWorker
  where
    transitionRuleWorker
        :: Prim
        -> ClaimState claim
        -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)

    transitionRuleWorker Begin Proven = empty
    transitionRuleWorker Begin (Stuck _) = empty
    transitionRuleWorker Begin (Rewritten claim) =
        SMT.reinit >> pure (Claimed claim)
    transitionRuleWorker Begin claimState =
        SMT.reinit >> pure claimState

    transitionRuleWorker Simplify claimState
      | Just claim <- retractSimplifiable claimState =
        Transition.ifte (simplify claim) (pure . ($>) claimState) (pure Proven)
      | otherwise =
        pure claimState

    transitionRuleWorker CheckImplication claimState
      | Just claim <- retractRewritable claimState = do
        result <- checkImplication claim & Logic.lowerLogicT
        case result of
            Implied -> pure Proven
            NotImpliedStuck a -> do
                warnStuckClaimStateTermsUnifiable
                pure (Stuck a)
            NotImplied a
              | isRemainder claimState -> do
                warnStuckClaimStateTermsNotUnifiable
                pure (Stuck a)
              | otherwise -> pure (Claimed a)
      | otherwise = pure claimState

    -- TODO (virgil): Wrap the results in GoalRemainder/GoalRewritten here.
    --
    -- thomas.tuegel: "Here" is in ApplyClaims and ApplyAxioms.
    --
    -- Note that in most transitions it is obvious what is being transformed
    -- into what, e.g. that a `ResetGoal` transition transforms
    -- `GoalRewritten` into `Goal`. However, here we're taking a `Goal`
    -- and transforming it into `GoalRewritten` and `GoalRemainder` in an
    -- opaque way. I think that there's no good reason for wrapping the
    -- results in `derivePar` as opposed to here.

    transitionRuleWorker ApplyClaims (Claimed claim) =
        applyClaims claims claim
    transitionRuleWorker ApplyClaims claimState = pure claimState

    transitionRuleWorker ApplyAxioms claimState
      | Just claim <- retractRewritable claimState =
        applyAxioms axiomGroups claim
      | otherwise = pure claimState

retractSimplifiable :: ClaimState a -> Maybe a
retractSimplifiable (Claimed a) = Just a
retractSimplifiable (Rewritten a) = Just a
retractSimplifiable (Remaining a) = Just a
retractSimplifiable _ = Nothing

isRemainder :: ClaimState a -> Bool
isRemainder (Remaining _) = True
isRemainder _ = False

reachabilityFirstStep :: Strategy Prim
reachabilityFirstStep =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , CheckImplication
        , ApplyAxioms
        , Simplify
        ]

reachabilityNextStep :: Strategy Prim
reachabilityNextStep =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , CheckImplication
        , ApplyClaims
        , ApplyAxioms
        , Simplify
        ]

strategy :: Stream (Strategy Prim)
strategy =
    reachabilityFirstStep :> Stream.iterate id reachabilityNextStep

{- | The result of checking the direct implication of a proof claim.

As an optimization, 'checkImplication' returns 'NotImpliedStuck' when the
implication between /terms/ is valid, but the implication between side
conditions does not hold.

 -}
data CheckImplicationResult a
    = Implied
    -- ^ The implication is valid.
    | NotImplied !a
    -- ^ The implication is not valid.
    | NotImpliedStuck !a
    -- ^ The implication between /terms/ is valid, but the implication between
    -- side-conditions is not valid.
    deriving (Show, Eq, Functor, GHC.Generic)

instance SOP.Generic claim =>
    SOP.Generic (CheckImplicationResult claim)

instance SOP.HasDatatypeInfo claim =>
    SOP.HasDatatypeInfo (CheckImplicationResult claim)

instance (Debug claim, SOP.HasDatatypeInfo claim) =>
    Debug (CheckImplicationResult claim)

instance (Diff claim, Debug claim, SOP.HasDatatypeInfo claim) =>
    Diff (CheckImplicationResult claim)

-- | Remove the destination of the claim.
checkImplication'
    :: forall claim m
    .  (MonadLogic m, MonadSimplify m)
    => Lens' claim ClaimPattern
    -> claim
    -> m (CheckImplicationResult claim)
checkImplication' lensRulePattern claim =
    claim
    & Lens.traverseOf lensRulePattern (Compose . checkImplicationWorker)
    & getCompose

assertFunctionLikeConfiguration
    :: forall m
    .  Monad m
    => HasCallStack
    => ClaimPattern
    -> m ()
assertFunctionLikeConfiguration claimPattern
  | (not . isFunctionPattern) leftTerm =
    error . show . Pretty.vsep $
        [ "The check implication step expects\
        \ the configuration term to be function-like."
        , Pretty.indent 2 "Configuration term:"
        , Pretty.indent 4 (unparse leftTerm)
        ]
  | otherwise = pure ()
  where
    ClaimPattern { left } = claimPattern
    leftTerm = Pattern.term left

newtype AnyUnified = AnyUnified { didAnyUnify :: Bool }
    deriving stock (Eq, Ord, Read, Show)
    deriving (Semigroup, Monoid) via Monoid.Any

{- | Check the claim by direct implication.

The claim has the form

@
φ(X) → ∘ ∃ Y. ⋁ᵢ ψᵢ(X, Y)
@

where @∘ _@ is a modality in reachability logic. @φ@ and the @ψᵢ@ are assumed to
be function-like patterns. @X@ and @Y@ are disjoint families of
variables. @checkImplicationWorker@ checks the validity of the formula

@
⌊ φ(X) → ∃ Y. ⋁ᵢ ψᵢ(X, Y) ⌋
@

Let @φ(X) = t(X) ∧ P(X)@ and @ψᵢ(X, Y) = tᵢ(X, Y) ∧ Pᵢ(X, Y)@; then the
implication formula above is valid when

@
(⋀ᵢ ¬ ∃ Y. ⌈t(X) ∧ tᵢ(X, Y)⌉ ∧ Pᵢ(X, Y)) ∧ ⌈t(X)⌉ ∧ P(X)
@

is unsatisfiable. This predicate basically consists of two parts: a single positive
conjunct asserting that the left-hand side of the claim is satisfiable:

@
⌈t(X)⌉ ∧ P(X)
@

and many negative conjuncts arising from the unification of the left- and
right-hand sides:

@
⋀ᵢ ¬ ∃ Y. ⌈t(X) ∧ tᵢ(X, Y)⌉ ∧ Pᵢ(X, Y)
@

When the implication formula is valid, @checkImplicationWorker@ returns
'Implied'. When the implication formula is not valid, we apply the following
heuristic:

* If any of the unification problems @⌈t(X) ∧ tᵢ(X, Y)⌉@ succeeded,
  @checkImplicationWorker@ returns 'NotImpliedStuck',
* otherwise, it returns 'NotImplied'.

Returing 'NotImpliedStuck' has the effect of terminating the proof. This
heuristic prevents the prover from executing beyond the intended final program
state ("inventing" programs), but at the cost that it does prevent the prover
from visiting the final program state twice. In practice, we find that deductive
proofs should not require the prover to visit the final program state twice,
anyway.

 -}
checkImplicationWorker
    :: forall m
    .  (MonadLogic m, MonadSimplify m)
    => ClaimPattern
    -> m (CheckImplicationResult ClaimPattern)
checkImplicationWorker (ClaimPattern.refreshExistentials -> claimPattern) =
    do
        (anyUnified, removal) <- getNegativeConjuncts
        let definedConfig =
                Pattern.andCondition left
                $ from $ makeCeilPredicate_ leftTerm
        let configs' = MultiOr.map (definedConfig <*) removal
        stuck <-
            simplifyConditionsWithSmt sideCondition configs'
            >>= Logic.scatter
        pure (examine anyUnified stuck)
    & elseImplied
  where
    ClaimPattern { right, left, existentials } = claimPattern
    leftTerm = Pattern.term left
    sort = termLikeSort leftTerm
    leftCondition = Pattern.withoutTerm left

    -- TODO (#1278): Do not combine the predicate and the substitution.
    -- This is held over from the old representation of claims, which did not
    -- distinguish the predicate and substitution in the first place. We can't
    -- use the substitution directly yet, because it isn't kept normalized. Once
    -- the claim is fully simplified at every step, that should not be a
    -- problem.
    sideCondition =
        SideCondition.assumeTrueCondition
            (Condition.fromPredicate . Condition.toPredicate $ leftCondition)

    getNegativeConjuncts :: m (AnyUnified, OrPattern RewritingVariableName)
    getNegativeConjuncts =
        do
            assertFunctionLikeConfiguration claimPattern
            right' <- Logic.scatter right
            let (rightTerm, rightCondition) = Pattern.splitTerm right'
            unified <-
                mkIn sort leftTerm rightTerm
                & Pattern.fromTermLike
                & Pattern.simplify sideCondition
                & (>>= Logic.scatter)
            didUnify
            removed <-
                Pattern.andCondition unified rightCondition
                & Pattern.simplify sideCondition
                & (>>= Logic.scatter)
            Exists.makeEvaluate sideCondition existentials removed
                >>= Logic.scatter
        & OrPattern.observeAllT
        & (>>= Not.simplifyEvaluated sideCondition)
        & wereAnyUnified

    wereAnyUnified :: StateT AnyUnified m a -> m (AnyUnified, a)
    wereAnyUnified act = swap <$> runStateT act mempty

    didUnify :: MonadState AnyUnified state => state ()
    didUnify = State.put (AnyUnified True)

    elseImplied acts = Logic.ifte acts pure (pure Implied)

    examine
        :: AnyUnified
        -> Pattern RewritingVariableName
        -> CheckImplicationResult ClaimPattern
    examine AnyUnified { didAnyUnify } stuck
      | not didAnyUnify = NotImplied claimPattern
      | isBottom condition = Implied
      | otherwise =
        Lens.set (field @"left") stuck claimPattern
        & NotImpliedStuck
      where
        (_, condition) = Pattern.splitTerm stuck

simplify'
    :: MonadSimplify m
    => Lens' claim ClaimPattern
    -> claim
    -> Strategy.TransitionT (AppliedRule claim) m claim
simplify' lensClaimPattern claim = do
    claim' <- simplifyLeftHandSide claim
    let sideCondition = extractSideCondition claim'
    simplifyRightHandSide sideCondition claim'
  where
    extractSideCondition =
        SideCondition.assumeTrueCondition
        . Pattern.withoutTerm
        . Lens.view (lensClaimPattern . field @"left")

    simplifyLeftHandSide =
        Lens.traverseOf (lensClaimPattern . field @"left") $ \config -> do
            let definedConfig =
                    Pattern.andCondition (mkDefined <$> config)
                    $ from $ makeCeilPredicate_ (Conditional.term config)
            configs <-
                simplifyTopConfiguration definedConfig
                >>= SMT.Evaluator.filterMultiOr
                & lift
            Foldable.asum (pure <$> configs)

    simplifyRightHandSide sideCondition =
        Lens.traverseOf (lensClaimPattern . field @"right") $ \dest ->
            OrPattern.observeAllT
            $ Logic.scatter dest
            >>= Pattern.simplify sideCondition
            >>= Logic.scatter

isTrusted :: From claim Attribute.Axiom.Trusted => claim -> Bool
isTrusted = Attribute.Trusted.isTrusted . from @_ @Attribute.Axiom.Trusted

-- | Exception that contains the last configuration before the error.
data WithConfiguration =
    WithConfiguration (Pattern VariableName) SomeException
    deriving (Show, Typeable)

instance Exception WithConfiguration

-- | Apply 'Rule's to the claim in parallel.
derivePar'
    :: forall m claim
    .  MonadSimplify m
    => Lens' claim ClaimPattern
    -> (RewriteRule RewritingVariableName -> Rule claim)
    -> [RewriteRule RewritingVariableName]
    -> claim
    -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)
derivePar' lensRulePattern mkRule =
    deriveWith lensRulePattern mkRule
    $ Step.applyRewriteRulesParallel Unification.unificationProcedure

type Deriver monad =
        [RewriteRule RewritingVariableName]
    ->  Pattern RewritingVariableName
    ->  monad (Step.Results (RulePattern RewritingVariableName))

-- | Apply 'Rule's to the claim in parallel.
deriveWith
    :: forall m claim
    .  Monad m
    => Lens' claim ClaimPattern
    -> (RewriteRule RewritingVariableName -> Rule claim)
    -> Deriver m
    -> [RewriteRule RewritingVariableName]
    -> claim
    -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)
deriveWith lensClaimPattern mkRule takeStep rewrites claim =
    getCompose
    $ Lens.forOf lensClaimPattern claim
    $ \claimPattern ->
        fmap (snd . Step.refreshRule mempty)
        $ Lens.forOf (field @"left") claimPattern
        $ \config -> Compose $ do
            results <- takeStep rewrites config & lift
            deriveResults fromAppliedRule results
  where
    fromAppliedRule =
        AppliedAxiom
        . mkRule
        . RewriteRule
        . Step.withoutUnification

-- | Apply 'Rule's to the claim in sequence.
deriveSeq'
    :: forall m claim
    .  MonadSimplify m
    => Lens' claim ClaimPattern
    -> (RewriteRule RewritingVariableName -> Rule claim)
    -> [RewriteRule RewritingVariableName]
    -> claim
    -> Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)
deriveSeq' lensRulePattern mkRule =
    deriveWith lensRulePattern mkRule . flip
    $ Step.applyRewriteRulesSequence Unification.unificationProcedure

deriveResults
    :: Step.UnifyingRuleVariable representation ~ RewritingVariableName
    => (Step.UnifiedRule representation -> AppliedRule claim)
    -> Step.Results representation
    -> Strategy.TransitionT (AppliedRule claim) simplifier
        (ClaimState.ClaimState (Pattern RewritingVariableName))
-- TODO (thomas.tuegel): Remove claim argument.
deriveResults fromAppliedRule Results { results, remainders } =
    addResults <|> addRemainders
  where
    addResults = Foldable.asum (addResult <$> results)
    addRemainders = Foldable.asum (addRemainder <$> Foldable.toList remainders)

    addResult Result { appliedRule, result } = do
        addRule appliedRule
        case Foldable.toList result of
            []      ->
                -- If the rule returns \bottom, the claim is proven on the
                -- current branch.
                pure Proven
            configs -> Foldable.asum (addRewritten <$> configs)

    addRewritten = pure . Rewritten
    addRemainder = pure . Remaining

    addRule = Transition.addRule . fromAppliedRule
