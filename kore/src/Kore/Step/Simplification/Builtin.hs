{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}
module Kore.Step.Simplification.Builtin
    ( simplify
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Compose
import           Data.Generics.Product
import           Data.Maybe

import qualified Kore.Builtin.AssociativeCommutative as Builtin
import           Kore.Domain.Builtin
                 ( InternalMap, InternalSet )
import qualified Kore.Domain.Builtin as Domain
import           Kore.Internal.Conditional
                 ( Conditional )
import qualified Kore.Internal.Conditional as Conditional
import           Kore.Internal.MultiOr as MultiOr
import           Kore.Internal.OrPattern
                 ( OrPattern )
import           Kore.Internal.TermLike
import           Kore.Predicate.Predicate
                 ( makeFalsePredicate )
import           Kore.Unparser

{-| 'simplify' simplifies a 'DomainValue' pattern, which means returning
an or containing a term made of that value.
-}
simplify
    :: ( Ord variable
       , Show variable
       , Unparse variable
       , SortedVariable variable
       )
    => Builtin (OrPattern variable)
    -> OrPattern variable
simplify builtin =
    MultiOr.filterOr $ do
        child <- simplifyBuiltin builtin
        return (mkBuiltin <$> child)

simplifyBuiltin
    :: ( Ord variable
       , Show variable
       , Unparse variable
       , SortedVariable variable
       )
    => Builtin (OrPattern variable)
    -> MultiOr (Conditional variable (Builtin (TermLike variable)))
simplifyBuiltin =
    \case
        Domain.BuiltinMap map' -> simplifyInternalMap map'
        Domain.BuiltinList _list -> do
            _list <- sequence _list
            -- MultiOr propagates \bottom children upward.
            return (Domain.BuiltinList <$> sequenceA _list)
        Domain.BuiltinSet set' -> simplifyInternalSet set'
        Domain.BuiltinInt int -> (return . pure) (Domain.BuiltinInt int)
        Domain.BuiltinBool bool -> (return . pure) (Domain.BuiltinBool bool)
        Domain.BuiltinString string ->
            (return . pure) (Domain.BuiltinString string)

simplifyInternalMap
    ::  ( Ord variable
        , Show variable
        , Unparse variable
        , SortedVariable variable
        )
    =>  Domain.InternalMap (TermLike Concrete) (OrPattern variable)
    ->  MultiOr
            (Conditional variable
                (Domain.Builtin (TermLike Concrete) (TermLike variable))
            )
simplifyInternalMap internalMapOrPattern = do
    conditionalInternalMap <- Lens.ala Compose traverse internalMapOrPattern
    let bottomInternalMap =
            conditionalInternalMap
            `Conditional.andPredicate` makeFalsePredicate
        normalizedInternalMap =
            fromMaybe bottomInternalMap
            $ traverse normalizeInternalMap conditionalInternalMap
    return (Domain.BuiltinMap <$> normalizedInternalMap)

normalizeInternalMap
    :: Ord variable
    => InternalMap (TermLike Concrete) (TermLike variable)
    -> Maybe (InternalMap (TermLike Concrete) (TermLike variable))
normalizeInternalMap =
    Lens.traverseOf (field @"builtinAcChild") Builtin.normalize

simplifyInternalSet
    ::  ( Ord variable
        , Show variable
        , Unparse variable
        , SortedVariable variable
        )
    =>  Domain.InternalSet (TermLike Concrete) (OrPattern variable)
    ->  MultiOr
            (Conditional variable
                (Domain.Builtin (TermLike Concrete) (TermLike variable))
            )
simplifyInternalSet internalSetOrPattern = do
    conditionalInternalSet <- Lens.ala Compose traverse internalSetOrPattern
    let bottomInternalSet =
            conditionalInternalSet
            `Conditional.andPredicate` makeFalsePredicate
        normalizedInternalSet =
            fromMaybe bottomInternalSet
            $ traverse normalizeInternalSet conditionalInternalSet
    return (Domain.BuiltinSet <$> normalizedInternalSet)

normalizeInternalSet
    :: Ord variable
    => InternalSet (TermLike Concrete) (TermLike variable)
    -> Maybe (InternalSet (TermLike Concrete) (TermLike variable))
normalizeInternalSet =
    Lens.traverseOf (field @"builtinAcChild") Builtin.normalize
