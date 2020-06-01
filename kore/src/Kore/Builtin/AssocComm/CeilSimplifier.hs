{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Builtin.AssocComm.CeilSimplifier
    ( newSetCeilSimplifier
    , newMapCeilSimplifier
    , generalizeMapElement
    ) where

import Prelude.Kore

import Control.Error
    ( MaybeT
    )
import qualified Control.Lens as Lens
import Control.Monad
    ( zipWithM
    )
import Control.Monad.Reader
    ( MonadReader
    )
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Builtin.Builtin as Builtin
import Kore.Domain.Builtin
    ( AcWrapper
    , Element
    , MapElement
    , NormalizedAc (..)
    , Value (MapValue)
    , emptyNormalizedAc
    )
import qualified Kore.Domain.Builtin as Domain
import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import qualified Kore.Internal.MultiAnd as MultiAnd
import Kore.Internal.OrCondition
    ( OrCondition
    )
import qualified Kore.Internal.OrCondition as OrCondition
import Kore.Internal.Predicate
    ( Predicate
    , makeCeilPredicate
    , makeForallPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
    ( Ceil (..)
    , ElementVariable
    , ElementVariableName (..)
    , InternalVariable
    , TermLike
    , Variable (..)
    , VariableName (..)
    , Void
    , fromVariableName
    , termLikeSort
    )
import qualified Kore.Internal.TermLike as TermLike
import qualified Kore.Step.Simplification.AndPredicates as And
import Kore.Step.Simplification.CeilSimplifier
import qualified Kore.Step.Simplification.Equals as Equals
import qualified Kore.Step.Simplification.Not as Not
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    )
import Kore.Variables.Fresh
    ( refreshElementVariable
    )

type BuiltinAssocComm normalized variable =
    Domain.InternalAc (TermLike Void) normalized (TermLike variable)

type MkBuiltinAssocComm normalized variable =
    BuiltinAssocComm normalized variable -> TermLike variable

type MkNotMember normalized variable =
        Element normalized (TermLike variable)
    ->  TermLike variable
    ->  Predicate variable

newSetCeilSimplifier
    ::  forall variable simplifier
    .   InternalVariable variable
    =>  MonadReader (SideCondition variable) simplifier
    =>  MonadSimplify simplifier
    =>  CeilSimplifier simplifier (TermLike variable) (OrCondition variable)
    ->  CeilSimplifier simplifier
            (BuiltinAssocComm Domain.NormalizedSet variable)
            (OrCondition variable)
newSetCeilSimplifier ceilSimplifierTermLike =
    CeilSimplifier $ \ceil@Ceil { ceilResultSort, ceilChild } -> do
        let mkInternalAc normalizedAc =
                ceilChild { Domain.builtinAcChild = Domain.wrapAc normalizedAc }
            mkNotMember element termLike =
                mkInternalAc (fromElement element) { opaque = [termLike] }
                & TermLike.mkBuiltinSet
                & makeCeilPredicate ceilResultSort
                -- TODO (thomas.tuegel): Do not mark this simplified.
                -- Marking here may prevent user-defined axioms from applying.
                -- At present, we wouldn't apply such an axiom, anyway.
                & Predicate.markSimplifiedMaybeConditional Nothing
        runCeilSimplifier
            (newBuiltinAssocCommCeilSimplifier
                TermLike.mkBuiltinSet
                mkNotMember
                ceilSimplifierTermLike
            )
            ceil

newMapCeilSimplifier
    ::  forall variable simplifier
    .   InternalVariable variable
    =>  MonadReader (SideCondition variable) simplifier
    =>  MonadSimplify simplifier
    =>  CeilSimplifier simplifier (TermLike variable) (OrCondition variable)
    ->  CeilSimplifier simplifier
            (BuiltinAssocComm Domain.NormalizedMap variable)
            (OrCondition variable)
newMapCeilSimplifier ceilSimplifierTermLike =
    CeilSimplifier $ \ceil@Ceil { ceilResultSort, ceilChild } -> do
        let mkInternalAc normalizedAc =
                ceilChild { Domain.builtinAcChild = Domain.wrapAc normalizedAc }
            mkNotMember element termLike =
                mkInternalAc (fromElement element') { opaque = [termLike] }
                & TermLike.mkBuiltinMap
                & makeCeilPredicate ceilResultSort
                -- TODO (thomas.tuegel): Do not mark this simplified.
                -- Marking here may prevent user-defined axioms from applying.
                -- At present, we wouldn't apply such an axiom, anyway.
                & Predicate.markSimplifiedMaybeConditional Nothing
                & makeForallPredicate variable
              where
                (variable, element') =
                    generalizeMapElement
                        (TermLike.freeVariables termLike)
                        element
        runCeilSimplifier
            (newBuiltinAssocCommCeilSimplifier
                TermLike.mkBuiltinMap
                mkNotMember
                ceilSimplifierTermLike
            )
            ceil

{- | Generalize a 'MapElement' by replacing the 'MapValue' with a variable.

The variable is renamed if required to avoid the given 'FreeVariables' and any
variables in the key of the 'MapElement'. The variable is returned along with
the generalized 'MapElement'

 -}
generalizeMapElement
    :: forall variable
    .  InternalVariable variable
    => FreeVariables variable
    -> MapElement (TermLike variable)
    -> (ElementVariable variable, MapElement (TermLike variable))
generalizeMapElement freeVariables' element =
    (variable, element')
  where
    (key, MapValue value) = Domain.unwrapElement element
    element' = Domain.wrapElement (key, MapValue $ TermLike.mkElemVar variable)
    avoiding =
        TermLike.freeVariables key <> freeVariables'
        & FreeVariables.toNames
    x =
        Variable
            { variableName =
                VariableName
                    { base = "x"
                    , counter = mempty
                    }
                & fromVariableName @variable & ElementVariableName
            , variableSort1 = termLikeSort value
            }
    variable = refreshElementVariable avoiding x & fromMaybe x

newBuiltinAssocCommCeilSimplifier
    :: forall normalized variable simplifier
    .   InternalVariable variable
    =>  MonadReader (SideCondition variable) simplifier
    =>  MonadSimplify simplifier
    =>  Traversable (Domain.Value normalized)
    =>  Domain.AcWrapper normalized
    =>  MkBuiltinAssocComm normalized variable
    ->  MkNotMember normalized variable
    ->  CeilSimplifier simplifier
            (TermLike variable)
            (OrCondition variable)
    ->  CeilSimplifier simplifier
            (BuiltinAssocComm normalized variable)
            (OrCondition variable)
newBuiltinAssocCommCeilSimplifier mkBuiltin mkNotMember ceilSimplifierTermLike =
    CeilSimplifier $ \Ceil { ceilResultSort, ceilChild } -> do
        let internalAc@Domain.InternalAc { builtinAcChild } = ceilChild
        sideCondition <- Reader.ask
        let normalizedAc = Domain.unwrapAc builtinAcChild
            Domain.NormalizedAc
                { elementsWithVariables = abstractElements
                , concreteElements
                , opaque
                }
              = normalizedAc

        let defineOpaquePair
                :: TermLike variable
                -> TermLike variable
                -> MultiAnd (OrCondition variable)
            defineOpaquePair opaque1 opaque2 =
                internalAc
                    { Domain.builtinAcChild =
                        Domain.wrapAc
                        emptyNormalizedAc { opaque = [opaque1, opaque2] }
                    }
                & mkBuiltin
                & makeCeilPredicate ceilResultSort
                -- TODO (thomas.tuegel): Do not mark this simplified.
                -- Marking here may prevent user-defined axioms from applying.
                -- At present, we wouldn't apply such an axiom, anyway.
                & Predicate.markSimplifiedMaybeConditional Nothing
                & OrCondition.fromPredicate
                & MultiAnd.singleton

            defineOpaquePairs
                :: TermLike variable
                -> [TermLike variable]
                -> MultiAnd (OrCondition variable)
            defineOpaquePairs this others =
                foldMap (defineOpaquePair this) others

            definedOpaquePairs :: MultiAnd (OrCondition variable)
            definedOpaquePairs =
                mconcat
                $ zipWith defineOpaquePairs opaque
                $ tail $ List.tails opaque

        let abstractKeys, concreteKeys
                :: [TermLike variable]
            abstractValues, concreteValues, allValues
                :: [Domain.Value normalized (TermLike variable)]
            (abstractKeys, abstractValues) =
                unzip (Domain.unwrapElement <$> abstractElements)
            concreteKeys = TermLike.fromConcrete <$> Map.keys concreteElements
            concreteValues = Map.elems concreteElements
            allValues = concreteValues <> abstractValues

        let makeEvaluateTerm, defineAbstractKey, defineOpaque
                :: TermLike variable -> MaybeT simplifier (OrCondition variable)
            makeEvaluateTerm termLike =
                runCeilSimplifier ceilSimplifierTermLike
                    Ceil
                        { ceilResultSort
                        , ceilOperandSort = TermLike.termLikeSort termLike
                        , ceilChild = termLike
                        }
            defineAbstractKey = makeEvaluateTerm
            defineOpaque = makeEvaluateTerm

            defineValue
                ::  Domain.Value normalized (TermLike variable)
                ->  MaybeT simplifier (MultiAnd (OrCondition variable))
            defineValue =
                Foldable.foldlM worker mempty
              where
                worker multiAnd termLike = do
                    evaluated <- makeEvaluateTerm termLike
                    return (multiAnd <> MultiAnd.singleton evaluated)

        TermLike.assertConstructorLikeKeys concreteKeys $ return ()

        -- concreteKeys are defined by assumption
        definedKeys <- traverse defineAbstractKey abstractKeys
        definedOpaque <- traverse defineOpaque opaque
        definedValues <- traverse defineValue allValues
        -- concreteKeys are distinct by assumption
        distinctConcreteKeys <-
            traverse (flip distinctKey concreteKeys) abstractKeys
        distinctAbstractKeys <-
            zipWithM distinctKey
                abstractKeys
                (tail $ List.tails abstractKeys)
        let conditions :: MultiAnd (OrCondition variable)
            conditions =
                mconcat
                    [ MultiAnd.make definedKeys
                    , MultiAnd.make definedOpaque
                    , mconcat definedValues
                    , mconcat distinctConcreteKeys
                    , mconcat distinctAbstractKeys
                    , Foldable.foldMap (notMembers normalizedAc) opaque
                    , definedOpaquePairs
                    ]

        And.simplifyEvaluatedMultiPredicate sideCondition conditions
  where

    distinctKey
        ::  TermLike variable
        ->  [TermLike variable]
        ->  MaybeT simplifier (MultiAnd (OrCondition variable))
    distinctKey thisKey otherKeys =
        MultiAnd.make <$> traverse (notEquals thisKey) otherKeys

    notEquals
        ::  TermLike variable
        ->  TermLike variable
        ->  MaybeT simplifier (OrCondition variable)
    notEquals t1 t2 = do
        sideCondition <- Reader.ask
        Equals.makeEvaluateTermsToPredicate tMin tMax sideCondition
            >>= Not.simplifyEvaluatedPredicate
      where
        -- Stabilize the order of terms under Equals.
        (tMin, tMax) = minMax t1 t2

    notMember
        :: TermLike variable
        -> Domain.Element normalized (TermLike variable)
        -> MultiAnd (OrCondition variable)
    notMember termLike element =
        mkNotMember element termLike
        & OrCondition.fromPredicate
        & MultiAnd.singleton

    notMembers
        :: NormalizedAc normalized (TermLike Void) (TermLike variable)
        -> TermLike variable
        -> MultiAnd (OrCondition variable)
    notMembers normalizedAc termLike =
        Lens.foldMapOf foldElements (notMember termLike) normalizedAc

foldElements
    ::  AcWrapper collection
    =>  InternalVariable variable
    =>  Lens.Fold
            (NormalizedAc collection (TermLike Void) (TermLike variable))
            (Element collection (TermLike variable))
foldElements =
    Lens.folding $ \normalizedAc ->
        let
            concreteElements' =
                concreteElements normalizedAc
                & Map.toList
                & map Domain.wrapConcreteElement
            symbolicElements' = elementsWithVariables normalizedAc
        in
            concreteElements' <> symbolicElements'

fromElement
    :: AcWrapper normalized
    => InternalVariable variable
    => Element normalized (TermLike variable)
    -> NormalizedAc normalized (TermLike Void) (TermLike variable)
fromElement element
  | Just concreteKey <- Builtin.toKey symbolicKey
  = emptyNormalizedAc { concreteElements = Map.singleton concreteKey value }
  | otherwise
  = emptyNormalizedAc { elementsWithVariables = [element] }
  where
    (symbolicKey, value) = Domain.unwrapElement element
