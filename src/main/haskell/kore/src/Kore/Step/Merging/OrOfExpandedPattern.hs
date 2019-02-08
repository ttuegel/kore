{-|
Module      : Kore.Step.Merging.OrOfExpandedPattern
Description : Tools for merging OrOfExpandedPatterns with various stuff.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Merging.OrOfExpandedPattern
    ( mergeWithPredicateSubstitution
    ) where

import Data.Reflection

import           Kore.AST.Common
                 ( SortedVariable )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.ExpandedPattern
                 ( PredicateSubstitution )
import qualified Kore.Step.Merging.ExpandedPattern as ExpandedPattern
                 ( mergeWithPredicateSubstitution )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( traverseWithPairs )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, SimplificationProof (..),
                 Simplifier, StepPatternSimplifier (..) )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Unparser
import           Kore.Variables.Fresh

{-| 'mergeWithPredicateSubstitution' ands the given predicate/substitution
to the given Or.
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
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -- ^ Evaluates functions in a pattern.
    -> PredicateSubstitution level variable
    -- ^ PredicateSubstitution to add.
    -> OrOfExpandedPattern level variable
    -- ^ Pattern to which the condition should be added.
    -> Simplifier (OrOfExpandedPattern level variable, SimplificationProof level)
mergeWithPredicateSubstitution
    tools
    substitutionSimplifier
    simplifier
    toMerge
    patt
  = do
    (evaluated, _proofs) <-
        OrOfExpandedPattern.traverseWithPairs
            (give tools $ ExpandedPattern.mergeWithPredicateSubstitution
                tools
                substitutionSimplifier
                simplifier
                toMerge
            )
            patt
    return (evaluated, SimplificationProof)
