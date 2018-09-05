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
                 ( AxiomPattern, StepProof (..), StepProofAtom (..),
                 simplificationProof, stepProof, stepWithAxiom )
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
    -> CommonExpandedPattern level
    -- ^ Configuration being rewritten.
    -> Stepper
        (CommonExpandedPattern level, StepProof level)
step tools symbolIdToEvaluator axioms configuration = do
    (stepPattern, stepProofs) <-
        baseStepWithPattern tools axioms configuration
    (simplifiedPatterns, simplificationProofs) <-
        liftSimplifier
            $ ExpandedPattern.simplify tools symbolIdToEvaluator stepPattern
    simplifiedPattern <- parallel (pure <$> OrOfExpandedPattern.extractPatterns simplifiedPatterns)
    return
        ( simplifiedPattern
        , simplificationProof simplificationProofs <> stepProofs
        )

baseStepWithPattern
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> CommonExpandedPattern level
    -- ^ Configuration being rewritten.
    -> Stepper (CommonExpandedPattern level, StepProof level)
baseStepWithPattern tools axioms configuration =
    (parallel . map liftSimplifier)
    (stepToList tools configuration axioms)

stepToList
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> CommonExpandedPattern level
    -- ^ Configuration being rewritten.
    -> [AxiomPattern level]
    -- ^ Rewriting axioms
    -> [ Simplifier (CommonExpandedPattern level, StepProof level) ]
stepToList tools configuration axioms =
    -- TODO: Stop ignoring Left results. Also, how would a result
    -- to which I can't apply an axiom look like?
    map liftCounter $ rights $ map (stepWithAxiom tools configuration) axioms

{-| 'pickFirstStepper' rewrites a configuration using the provided axioms
until it cannot be rewritten anymore or until the step limit has been reached.
Whenever multiple axioms can be applied, it picks the first one whose
'Predicate' is not false and continues with that.

Does not handle properly various cases, among which:
sigma(x, y) => y    vs    a
-}
pickFirstStepper
    :: (MetaOrObject level)
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
    manySteps stepWithFirst
  where
    stepWithFirst (config0, proof0) =
        limitSteps stepLimit $ do
            (config1, proof1) <- step tools symbolIdToEvaluator axioms config0
            return (config1, proof0 <> proof1)
