{-|
Module      : Kore.Step.Function.UserDefined
Description : Evaluates user-defined functions in a pattern.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Function.UserDefined
    ( PureMLPatternSimplifier
    , axiomFunctionEvaluator
    ) where

import Control.Monad.Except
       ( runExceptT )
import Data.Reflection

import           Kore.AST.Common
                 ( Application (..), CommonPurePattern, Pattern (..),
                 PureMLPattern, SortedVariable )
import           Kore.AST.MetaOrObject
                 ( Meta, MetaOrObject, Object )
import           Kore.AST.PureML
                 ( asPurePattern )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Predicate.Predicate
                 ( pattern PredicateFalse, makeTruePredicate )
import           Kore.Step.BaseStep
                 ( AxiomPattern, stepWithAxiom )
import qualified Kore.Step.Condition.Evaluator as Predicate
                 ( evaluate )
import           Kore.Step.ExpandedPattern as ExpandedPattern
                 ( ExpandedPattern, Predicated(..))
import           Kore.Step.ExpandedPattern as PredicateSubstitution
                 ( PredicateSubstitution (..) )
import           Kore.Step.ExpandedPattern
                 ( PredicateSubstitution (PredicateSubstitution) )
import           Kore.Step.Function.Data as AttemptedFunction
                 ( AttemptedFunction (..) )
import           Kore.Step.Function.Data
                 ( CommonAttemptedFunction )
import qualified Kore.Step.Merging.OrOfExpandedPattern as OrOfExpandedPattern
                 ( mergeWithPredicateSubstitution )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make, traverseWithPairs )
import           Kore.Step.Simplification.Data
                 ( CommonPureMLPatternSimplifier, PureMLPatternSimplifier (..),
                 SimplificationProof (..), Simplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutions )
import           Kore.Substitution.Class
                 ( Hashable )
import           Kore.Variables.Fresh

{-| 'axiomFunctionEvaluator' evaluates a user-defined function. After
evaluating the function, it tries to re-evaluate all functions on the result.

The function is assumed to be defined through an axiom.
-}
axiomFunctionEvaluator
    ::  ( MetaOrObject level)
    => AxiomPattern level
    -- ^ Axiom defining the current function.
    -> MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> CommonPureMLPatternSimplifier level
    -- ^ Evaluates functions in patterns
    -> Application level (CommonPurePattern level)
    -- ^ The function on which to evaluate the current function.
    -> Simplifier (CommonAttemptedFunction level, SimplificationProof level)
axiomFunctionEvaluator
    axiom
    tools
    simplifier
    app
  = do
    result <- runExceptT stepResult
    case result of
        Left _ ->
            return (AttemptedFunction.NotApplicable, SimplificationProof)
        Right (stepPattern, _) ->
            do
                (   rewrittenPattern@Predicated
                        { predicate = rewritingCondition }
                    , _
                    ) <- evaluatePredicate tools simplifier stepPattern
                case rewritingCondition of
                    PredicateFalse ->
                        return
                            ( AttemptedFunction.Applied
                                (OrOfExpandedPattern.make [])
                            , SimplificationProof
                            )
                    _ ->
                        reevaluateFunctions
                            tools
                            simplifier
                            rewrittenPattern
  where
    stepResult =
        stepWithAxiom
            tools
            (stepperConfiguration app)
            axiom
    stepperConfiguration
        :: MetaOrObject level
        => Application level (PureMLPattern level variable)
        -> ExpandedPattern level variable
    stepperConfiguration app' =
        Predicated
            { term = asPurePattern $ ApplicationPattern app'
            , predicate = makeTruePredicate
            , substitution = []
            }

{-| 'reevaluateFunctions' re-evaluates functions after a user-defined function
was evaluated.
-}
reevaluateFunctions
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions in patterns.
    -> ExpandedPattern level variable
    -- ^ Function evaluation result.
    -> Simplifier (AttemptedFunction level variable, SimplificationProof level)
reevaluateFunctions
    tools
    wrappedSimplifier@(PureMLPatternSimplifier simplifier)
    Predicated
        { term   = rewrittenPattern
        , predicate = rewritingCondition
        , substitution = rewrittenSubstitution
        }
  = do
    (pattOr , _proof) <-
        -- TODO(virgil): This call should be done in Evaluator.hs, but,
        -- for optimization purposes, it's done here. Make sure that
        -- this still makes sense after the evaluation code is fully
        -- optimized.
        simplifier rewrittenPattern
    (mergedPatt, _proof) <-
        OrOfExpandedPattern.mergeWithPredicateSubstitution
            tools
            wrappedSimplifier
            PredicateSubstitution
                { predicate = rewritingCondition
                , substitution = rewrittenSubstitution
                }
            pattOr
    (evaluatedPatt, _) <-
        OrOfExpandedPattern.traverseWithPairs
            (evaluatePredicate tools wrappedSimplifier)
            mergedPatt
    return
        ( AttemptedFunction.Applied evaluatedPatt
        , SimplificationProof
        )

evaluatePredicate
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions in a pattern.
    -> ExpandedPattern level variable
    -- ^ The condition to be evaluated.
    -> Simplifier (ExpandedPattern level variable, SimplificationProof level)
evaluatePredicate
    tools
    simplifier
    Predicated {term, predicate, substitution}
  = do
    (   PredicateSubstitution
            { predicate = evaluatedPredicate
            , substitution = evaluatedSubstitution
            }
        , _proof
        ) <- give (symbolOrAliasSorts tools) $
             give tools $
                 Predicate.evaluate simplifier predicate
    (   PredicateSubstitution
            { predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , _proof
        ) <-
            mergePredicatesAndSubstitutions
                tools
                [evaluatedPredicate]
                [substitution, evaluatedSubstitution]
    -- TODO(virgil): Do I need to re-evaluate the predicate?
    return
        ( Predicated
            { term = term
            , predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , SimplificationProof
        )
