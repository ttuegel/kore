{-|
Module      : Kore.Step.Step
Description : Single and multiple step execution
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Step
    ( step
    , pickFirstStepper
    , Limit (..)
    , Natural
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import qualified Control.Monad.State.Class as Monad.State
import qualified Control.Monad.Trans as Monad.Trans
import           Control.Monad.Trans.Maybe
                 ( MaybeT (..), runMaybeT )
import           Data.Either
                 ( rights )
import qualified Data.Map as Map
import           Data.Semigroup
                 ( (<>) )

import           Kore.AST.Common
                 ( Id )
import           Kore.AST.MetaOrObject
                 ( MetaOrObject )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.BaseStep
                 ( AxiomPattern, StepProof (..), StepProofAtom (..), stepProof,
                 stepWithAxiom )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern )
import           Kore.Step.Function.Data
                 ( CommonApplicationFunctionEvaluator )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( extractPatterns, make, traverseFlattenWithPairs )
import           Kore.Step.Simplification.Data
                 ( Simplifier, liftCounter )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
                 ( simplify )
import           Kore.Step.Stepper
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Variables.Fresh

{-| 'step' executes a single rewriting step using the provided axioms.

Does not handle properly various cases, among which:
sigma(x, y) => y    vs    a
-}
step
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> Map.Map (Id level) [CommonApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> CommonOrOfExpandedPattern level
    -- ^ Configuration being rewritten.
    -> Simplifier
        (CommonOrOfExpandedPattern level, StepProof level)
step tools symbolIdToEvaluator axioms configuration = do
    (stepPattern, stepProofs) <- liftCounter
        (OrOfExpandedPattern.traverseFlattenWithPairs
            (baseStepWithPattern tools axioms)
            configuration
        )
    (simplifiedPattern, simplificationProofs) <-
        OrOfExpandedPattern.traverseFlattenWithPairs
            (ExpandedPattern.simplify tools symbolIdToEvaluator)
            stepPattern
    return
        ( simplifiedPattern
        , mconcat (stepProof . StepProofSimplification <$> simplificationProofs)
            <> mconcat stepProofs
        )

baseStepWithPattern
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> CommonExpandedPattern level
    -- ^ Configuration being rewritten.
    -> Counter (CommonOrOfExpandedPattern level, StepProof level)
baseStepWithPattern tools axioms configuration = do
    stepResultsWithProofs <- sequence (stepToList tools configuration axioms)
    let (results, proofs) = unzip stepResultsWithProofs
    return
        ( OrOfExpandedPattern.make results
        , mconcat proofs
        )

stepToList
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> CommonExpandedPattern level
    -- ^ Configuration being rewritten.
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    ->  [ Counter
            (CommonExpandedPattern level, StepProof level)
        ]
stepToList tools configuration axioms =
    -- TODO: Stop ignoring Left results. Also, how would a result
    -- to which I can't apply an axiom look like?
    rights $ map (stepWithAxiom tools configuration) axioms

{-| 'pickFirstStepper' rewrites a configuration using the provided axioms
until it cannot be rewritten anymore or until the step limit has been reached.
Whenever multiple axioms can be applied, it picks the first one whose
'Predicate' is not false and continues with that.

Does not handle properly various cases, among which:
sigma(x, y) => y    vs    a
-}
pickFirstStepper
    ::  ( MetaOrObject level)
    => Limit Natural
    -> MetadataTools level StepperAttributes
    -> Map.Map (Id level) [CommonApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> (CommonExpandedPattern level, StepProof level)
    -- ^ Configuration being rewritten and its accompanying proof
    -> Stepper (CommonExpandedPattern level, StepProof level)
pickFirstStepper
    stepLimit tools symbolIdToEvaluator axioms
  =
    limitSteps stepLimit
        (pickFirstStepperSkipMaxCheck
            stepLimit
            tools
            symbolIdToEvaluator
            axioms
        )

pickFirstStepperSkipMaxCheck
    ::  ( MetaOrObject level)
    => Limit Natural
    -> MetadataTools level StepperAttributes
    -> Map.Map (Id level) [CommonApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> (CommonExpandedPattern level, StepProof level)
    -- ^ Configuration being rewritten and its accompanying proof
    -> Stepper (CommonExpandedPattern level, StepProof level)
pickFirstStepperSkipMaxCheck
    stepLimit tools symbolIdToEvaluator axioms
    (stepperConfiguration, prevProof)
  = do
    simplified <-
        runMaybeT (normalStep <|> heatingStep <|> coolingStep)
    case simplified of
        Nothing -> return (stepperConfiguration, prevProof)
        Just (nextConfig, thisProof) ->
            pickFirstStepper stepLimit tools symbolIdToEvaluator axioms
                (nextConfig, prevProof <> thisProof)
  where
    stepWithFirst axioms' = do
        (patterns, thisProof) <-
            Monad.Trans.lift
                $ liftSimplifier
                $ step tools symbolIdToEvaluator axioms'
                $ OrOfExpandedPattern.make [stepperConfiguration]
        case OrOfExpandedPattern.extractPatterns patterns of
            [] -> empty
            (nextConfig : _) -> return (nextConfig, thisProof)

    normalAxioms = filter isNormalRule axioms
    heatingAxioms = filter isHeatingRule axioms
    coolingAxioms = filter isCoolingRule axioms

    normalStep = do
        applied <- stepWithFirst normalAxioms
        appliedRule Nothing
        return applied

    heatingStep = do
        StepperState { stepperLastApplied } <- Monad.State.get
        case stepperLastApplied of
            Just Cool ->
                -- The last applied rule was a cooling rule, so we must not
                -- attempt a heating rule right now: it would always succeed
                -- and evaluation would loop forever.
                empty
            _ -> do
                applied <- stepWithFirst heatingAxioms
                appliedRule (Just Heat)
                return applied

    coolingStep = do
        StepperState { stepperLastApplied } <- Monad.State.get
        case stepperLastApplied of
            Just Heat ->
                -- The last applied rule was a heating rule, so we must not
                -- attempt a cooling rule right now: it would always succeed
                -- and evaluation would loop forever.
                empty
            _ -> do
                applied <- stepWithFirst coolingAxioms
                appliedRule (Just Cool)
                return applied
