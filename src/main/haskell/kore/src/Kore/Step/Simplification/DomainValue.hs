{-|
Module      : Kore.Step.Simplification.DomainValue
Description : Tools for DomainValue pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.DomainValue
    ( simplify
    ) where

import           Data.Foldable
                 ( foldl' )
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Reflection
                 ( Given, give )
import           Data.Sequence
                 ( Seq )
import           Data.Sequence as Seq
import           Prelude hiding
                 ( pred )

import           Kore.AST.Common
                 ( BuiltinDomain (..), ConcretePurePattern, DomainValue (..),
                 Pattern (DomainValuePattern), PureMLPattern, Sort,
                 SortedVariable )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( asPurePattern )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SortTools )
import           Kore.Predicate.Predicate
                 ( Predicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern (ExpandedPattern) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.OrOfExpandedPattern
                 ( MultiOr, OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )
import           Kore.Unification.Data
                 ( UnificationSubstitution )

{-| 'simplify' simplifies a 'DomainValue' pattern, which means returning
an or containing a term made of that value.
-}
simplify
    :: ( Eq (variable Object), Show (variable Object)
       , SortedVariable variable
       )
    => MetadataTools Object attrs
    -> DomainValue Object (BuiltinDomain (OrOfExpandedPattern Object variable))
    -> ( OrOfExpandedPattern Object variable
       , SimplificationProof Object
       )
simplify MetadataTools { sortTools } DomainValue { domainValueSort, domainValueChild } =
    ( give sortTools simplifyBuiltinDomain domainValueSort domainValueChild
    , SimplificationProof
    )
{-
    ( OrOfExpandedPattern.make
        [ExpandedPattern
            { term = asPurePattern (DomainValuePattern dv)
            , predicate = makeTruePredicate
            , substitution = []
            }
        ]
    , SimplificationProof
    )
-}

simplifyBuiltinDomain
    :: ( Eq (variable Object), Show (variable Object)
       , Given (SortTools Object)
       , SortedVariable variable
       )
    => Sort Object
    -> BuiltinDomain (OrOfExpandedPattern Object variable)
    -> OrOfExpandedPattern Object variable
simplifyBuiltinDomain domainValueSort =
    \case
        BuiltinDomainPattern pat ->
            OrOfExpandedPattern.make
            [
                (ExpandedPattern.fromPurePattern . asPurePattern)
                (DomainValuePattern DomainValue
                    { domainValueSort
                    , domainValueChild = BuiltinDomainPattern pat
                    }
                )
            ]
        BuiltinDomainMap _map -> do
            (_map, predicate, substitution) <-
                Map.foldlWithKey' simplifyBuiltinDomainMapElement
                    (pure (Map.empty, Predicate.makeTruePredicate, mempty))
                    _map
            let term =
                    (asPurePattern . DomainValuePattern)
                    DomainValue
                        { domainValueSort
                        , domainValueChild = BuiltinDomainMap _map
                        }
            OrOfExpandedPattern.make
                [ ExpandedPattern { term, predicate, substitution } ]
        BuiltinDomainList _list -> do
            (_list, predicate, substitution) <-
                foldl' simplifyBuiltinDomainListElement
                    (pure (Seq.empty, Predicate.makeTruePredicate, mempty))
                    _list
            let term =
                    (asPurePattern . DomainValuePattern)
                    DomainValue
                        { domainValueSort
                        , domainValueChild = BuiltinDomainList _list
                        }
            OrOfExpandedPattern.make
                [ ExpandedPattern { term, predicate, substitution } ]
        BuiltinDomainSet set ->
            OrOfExpandedPattern.make
            [
                (ExpandedPattern.fromPurePattern . asPurePattern)
                (DomainValuePattern DomainValue
                    { domainValueSort
                    , domainValueChild = BuiltinDomainSet set
                    }
                )
            ]

simplifyBuiltinDomainListElement
    :: ( Eq (variable Object), Show (variable Object)
       , Given (SortTools Object)
       , SortedVariable variable
       )
    => MultiOr
        ( Seq (PureMLPattern Object variable)
        , Predicate Object variable
        , UnificationSubstitution Object variable
        )
    -> OrOfExpandedPattern Object variable
    -> MultiOr
        ( Seq (PureMLPattern Object variable)
        , Predicate Object variable
        , UnificationSubstitution Object variable
        )
simplifyBuiltinDomainListElement accum xs =
    do
        (terms, pred, subst) <- accum
        ExpandedPattern { term, predicate, substitution } <- xs
        let (pred', _) = Predicate.makeAndPredicate pred predicate
            subst' = subst <> substitution
        return (terms Seq.:|> term, pred', subst')

simplifyBuiltinDomainMapElement
    :: ( Eq (variable Object), Show (variable Object)
       , Given (SortTools Object)
       , SortedVariable variable
       )
    => MultiOr
        ( Map (ConcretePurePattern Object) (PureMLPattern Object variable)
        , Predicate Object variable
        , UnificationSubstitution Object variable
        )
    -> ConcretePurePattern Object
    -> OrOfExpandedPattern Object variable
    -> MultiOr
        ( Map (ConcretePurePattern Object) (PureMLPattern Object variable)
        , Predicate Object variable
        , UnificationSubstitution Object variable
        )
simplifyBuiltinDomainMapElement accum key xs =
    do
        (terms, pred, subst) <- accum
        ExpandedPattern { term, predicate, substitution } <- xs
        let (pred', _) = Predicate.makeAndPredicate pred predicate
            subst' = subst <> substitution
        return (Map.insert key term terms, pred', subst')
