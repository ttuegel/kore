module Kore.Step.Substitution where

import GHC.Stack
       ( HasCallStack )

import           Kore.Internal.Pattern
                 ( Predicate )
import qualified Kore.Predicate.Predicate as Syntax
                 ( Predicate )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, TermLikeSimplifier )
import           Kore.Syntax.Variable
                 ( SortedVariable )
import           Kore.Unification.Substitution
                 ( Substitution )
import           Kore.Unification.Unify
                 ( MonadUnify )
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

mergePredicatesAndSubstitutionsExcept
    ::  ( Show variable
        , SortedVariable variable
        , Ord variable
        , Unparse variable
        , FreshVariable variable
        , HasCallStack
        , MonadUnify unifier
        )
    => PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> [Syntax.Predicate variable]
    -> [Substitution variable]
    -> unifier (Predicate variable)
