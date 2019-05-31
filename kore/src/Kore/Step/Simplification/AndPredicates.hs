{-|
Module      : Kore.Step.Simplification.AndPredicates
Description : Tools for And Predicate simplification.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.AndPredicates
    ( simplifyEvaluatedMultiPredicate
    ) where

import qualified Data.Foldable as Foldable

import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Internal.MultiAnd
                 ( MultiAnd )
import qualified Kore.Internal.MultiAnd as MultiAnd
                 ( extractPatterns )
import           Kore.Internal.MultiOr
                 ( MultiOr )
import qualified Kore.Internal.MultiOr as MultiOr
                 ( fullCrossProduct, mergeAll )
import           Kore.Internal.OrPredicate
                 ( OrPredicate )
import           Kore.Internal.Pattern
                 ( Predicate )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Simplification.Data
                 ( BranchT, PredicateSimplifier, Simplifier,
                 TermLikeSimplifier )
import qualified Kore.Step.Simplification.Data as BranchT
                 ( gather )
import qualified Kore.Step.Substitution as Substitution
import           Kore.Unparser
import           Kore.Variables.Fresh

simplifyEvaluatedMultiPredicate
    :: forall variable .
        ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        , FreshVariable variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> MultiAnd (OrPredicate variable)
    -> Simplifier (OrPredicate variable)
simplifyEvaluatedMultiPredicate
    _tools
    substitutionSimplifier
    simplifier
    axiomIdToSubstitution
    predicates
  = do
    let
        crossProduct :: MultiOr [Predicate variable]
        crossProduct =
            MultiOr.fullCrossProduct
                (MultiAnd.extractPatterns predicates)
    orResults <- BranchT.gather (traverse andPredicates crossProduct)
    return (MultiOr.mergeAll orResults)
  where
    andPredicates
        :: [Predicate variable]
        -> BranchT Simplifier (Predicate variable)
    andPredicates predicates' =
        normalize (Foldable.fold predicates')

    normalize =
        Substitution.normalize
            substitutionSimplifier
            simplifier
            axiomIdToSubstitution
