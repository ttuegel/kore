{-|
Module      : Kore.Step.Merging.ExpandedPattern
Description : Tools for merging ExpandedPatterns with various stuff.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Merging.ExpandedPattern
    ( mergeWithPredicateSubstitution
    ) where

import Data.Reflection

import           Kore.AST.Common
                 ( SortedVariable )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import qualified Kore.Step.Condition.Evaluator as Predicate
                 ( evaluate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, PredicateSubstitution, Predicated (..) )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier,
                 SimplificationProof (SimplificationProof), Simplifier,
                 StepPatternSimplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutions )
import           Kore.Unparser
import           Kore.Variables.Fresh

{-| 'mergeWithPredicateSubstitution' ands the given predicate-substitution
with the given pattern.
-}
mergeWithPredicateSubstitution
    ::  ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , SortedVariable variable
        , Given (MetadataTools level StepperAttributes)
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -- ^ Evaluates functions in a pattern.
    -> PredicateSubstitution level variable
    -- ^ Condition and substitution to add.
    -> ExpandedPattern level variable
    -- ^ pattern to which the above should be added.
    -> Simplifier (ExpandedPattern level variable, SimplificationProof level)
mergeWithPredicateSubstitution
    tools
    substitutionSimplifier
    simplifier
    Predicated
        { predicate = conditionToMerge
        , substitution = substitutionToMerge
        }
    patt@Predicated
        { predicate = pattPredicate
        , substitution = pattSubstitution
        }
  = do
    (   Predicated
            { predicate = mergedCondition
            , substitution = mergedSubstitution
            }
        , _proof ) <-
            mergePredicatesAndSubstitutions
                tools
                substitutionSimplifier
                [pattPredicate, conditionToMerge]
                [pattSubstitution, substitutionToMerge]
    (evaluatedCondition, _) <-
        Predicate.evaluate
            substitutionSimplifier simplifier mergedCondition
    mergeWithEvaluatedCondition
        tools
        substitutionSimplifier
        patt {substitution = mergedSubstitution}
        evaluatedCondition

mergeWithEvaluatedCondition
    ::  ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , SortedVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> ExpandedPattern level variable
    -> PredicateSubstitution level variable
    -> Simplifier (ExpandedPattern level variable, SimplificationProof level)
mergeWithEvaluatedCondition
    tools
    substitutionSimplifier
    Predicated
        { term = pattTerm
        , substitution = pattSubstitution
        }  -- The predicate was already included in the PredicateSubstitution
    Predicated
        { predicate = predPredicate, substitution = predSubstitution }
  = do
    (   Predicated
            { predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , _proof
        ) <- mergePredicatesAndSubstitutions
            tools
            substitutionSimplifier
            [predPredicate]
            [pattSubstitution, predSubstitution]
    return
        ( Predicated
            { term = pattTerm
            , predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , SimplificationProof
        )
