{- |
Module      : Kore.Builtin.Map
Description : Built-in key-value maps
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Map as Map
@
 -}
module Kore.Builtin.Map
    ( sort
    , verifiers
    , builtinFunctions
    , Map.asTermLike
    , internalize
    -- * Unification
    , unifyEquals
    -- * Raw evaluators
    , evalConcat
    , evalElement
    , evalUnit
    ) where

import Prelude.Kore

import Control.Error
    ( MaybeT (MaybeT)
    , fromMaybe
    , hoistMaybe
    , runMaybeT
    )
import qualified Control.Monad as Monad
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text
    ( Text
    )
import qualified Data.Text as Text

import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin.AssociativeCommutative as Ac
import Kore.Builtin.Attributes
    ( isConstructorModulo_
    )
import qualified Kore.Builtin.Bool as Bool
import Kore.Builtin.Builtin
    ( acceptAnySort
    )
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.List as Builtin.List
import qualified Kore.Builtin.Map.Map as Map
import qualified Kore.Builtin.Set as Builtin.Set
import qualified Kore.Domain.Builtin as Domain
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    )
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike
    ( pattern App_
    , pattern Builtin_
    , InternalVariable
    , TermLike
    , termLikeSort
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Sort
    ( Sort
    )
import Kore.Step.Simplification.Simplify as Simplifier
import Kore.Syntax.Sentence
    ( SentenceSort (..)
    )
import Kore.Unification.Unify
    ( MonadUnify
    )
import qualified Kore.Unification.Unify as Monad.Unify
import Kore.Variables.Fresh

{- | Builtin name of the @Map@ sort.
 -}
sort :: Text
sort = "MAP.Map"

{- | Is the given sort hooked to the builtin Map sort?

Returns Nothing if the sort is unknown (i.e. the _PREDICATE sort).
Returns Just False if the sort is a variable.
-}
isMapSort :: SmtMetadataTools attrs -> Sort -> Maybe Bool
isMapSort = Builtin.isSort sort

{- | Verify that the sort is hooked to the builtin @Int@ sort.

  See also: 'sort', 'Builtin.verifySort'

 -}
assertSort :: Builtin.SortVerifier
assertSort = Builtin.verifySort sort

verifiers :: Builtin.Verifiers
verifiers =
    Builtin.Verifiers
        { sortDeclVerifiers
        , symbolVerifiers
        , patternVerifierHook = mempty
        }

{- | Verify that hooked sort declarations are well-formed.

  See also: 'Builtin.verifySortDecl'

 -}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers =
    HashMap.fromList [ (sort, verifySortDecl) ]
  where
    verifySortDecl indexedModule sentenceSort attrs = do
        Builtin.verifySortDecl indexedModule sentenceSort attrs
        unitId <- Builtin.getUnitId attrs
        Builtin.assertSymbolHook indexedModule unitId Map.unitKey
        Builtin.assertSymbolResultSort indexedModule unitId expectedSort
        elementId <- Builtin.getElementId attrs
        Builtin.assertSymbolHook indexedModule elementId Map.elementKey
        Builtin.assertSymbolResultSort indexedModule elementId expectedSort
        concatId <- Builtin.getConcatId attrs
        Builtin.assertSymbolHook indexedModule concatId Map.concatKey
        Builtin.assertSymbolResultSort indexedModule concatId expectedSort
        return ()
      where
        SentenceSort { sentenceSortName } = sentenceSort
        expectedSort = TermLike.mkSort sentenceSortName

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( Map.concatKey
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( Map.elementKey
      , Builtin.verifySymbol assertSort [acceptAnySort, acceptAnySort]
      )
    , ( Map.lookupKey
      , Builtin.verifySymbol acceptAnySort [assertSort, acceptAnySort]
      )
    , ( Map.lookupOrDefaultKey
      , Builtin.verifySymbol acceptAnySort
            [assertSort, acceptAnySort, acceptAnySort]
      )
    , ( Map.unitKey
      , Builtin.verifySymbol assertSort []
      )
    , ( Map.updateKey
      , Builtin.verifySymbol assertSort
            [assertSort, acceptAnySort, acceptAnySort]
      )
    , ( Map.in_keysKey
      , Builtin.verifySymbol Bool.assertSort [acceptAnySort, assertSort]
      )
    , ( Map.keysKey
      , Builtin.verifySymbol Builtin.Set.assertSort [assertSort]
      )
    , ( Map.keys_listKey
      , Builtin.verifySymbol Builtin.List.assertSort [assertSort]
      )
    , ( Map.removeKey
      , Builtin.verifySymbol assertSort [assertSort, acceptAnySort]
      )
    , ( Map.removeAllKey
      , Builtin.verifySymbol assertSort [assertSort, Builtin.Set.assertSort]
      )
    , ( Map.sizeKey
      , Builtin.verifySymbol Int.assertSort [assertSort]
      )
    , ( Map.valuesKey
      , Builtin.verifySymbol Builtin.List.assertSort [assertSort]
      )
    , ( Map.inclusionKey
      , Builtin.verifySymbol Bool.assertSort [assertSort, assertSort]
      )
    ]

{- | Abort function evaluation if the argument is not a Map domain value.

    If the operand pattern is not a domain value, the function is simply
    'NotApplicable'. If the operand is a domain value, but not represented
    by a 'BuiltinDomainMap', it is a bug.

 -}
expectBuiltinMap
    :: Monad m
    => Text  -- ^ Context for error message
    -> TermLike variable  -- ^ Operand pattern
    -> MaybeT m (Ac.TermNormalizedAc Domain.NormalizedMap variable)
expectBuiltinMap ctx (Builtin_ builtin) =
    case builtin of
        Domain.BuiltinMap Domain.InternalAc { builtinAcChild } ->
            return builtinAcChild
        _ ->
            Builtin.verifierBug
            $ Text.unpack ctx ++ ": Domain value is not a map"
expectBuiltinMap _ _ = empty

{- | Returns @empty@ if the argument is not a @NormalizedMap@ domain value
which consists only of concrete elements.

Returns the @Map@ of concrete elements otherwise.
-}
expectConcreteBuiltinMap
    :: MonadSimplify m
    => Text  -- ^ Context for error message
    -> TermLike variable  -- ^ Operand pattern
    -> MaybeT m (Map (TermLike Concrete) (Domain.MapValue (TermLike variable)))
expectConcreteBuiltinMap ctx _map = do
    _map <- expectBuiltinMap ctx _map
    case Domain.unwrapAc _map of
        Domain.NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = []
            } -> return concreteElements
        _ -> empty

{- | Converts a @Map@ of concrete elements to a @NormalizedMap@ and returns it
as a function result.
-}
returnConcreteMap
    :: (MonadSimplify m, InternalVariable variable)
    => Sort
    -> Map (TermLike Concrete) (Domain.MapValue (TermLike variable))
    -> m (Pattern variable)
returnConcreteMap = Ac.returnConcreteAc

evalLookup :: BuiltinAndAxiomSimplifier
evalLookup =
    Builtin.functionEvaluator evalLookup0
  where
    evalLookup0 :: Builtin.Function
    evalLookup0 resultSort [_map, _key] = do
        let emptyMap = do
                _map <- expectConcreteBuiltinMap Map.lookupKey _map
                if Map.null _map
                    then return (Pattern.bottomOf resultSort)
                    else empty
            bothConcrete = do
                _key <- hoistMaybe $ Builtin.toKey _key
                _map <- expectConcreteBuiltinMap Map.lookupKey _map
                (return . maybeBottom)
                    (Domain.getMapValue <$> Map.lookup _key _map)
        emptyMap <|> bothConcrete
      where
        maybeBottom = maybe (Pattern.bottomOf resultSort) Pattern.fromTermLike
    evalLookup0 _ _ = Builtin.wrongArity Map.lookupKey

evalLookupOrDefault :: BuiltinAndAxiomSimplifier
evalLookupOrDefault =
    Builtin.functionEvaluator evalLookupOrDefault0
  where
    evalLookupOrDefault0 :: Builtin.Function
    evalLookupOrDefault0 _ [_map, _key, _def] = do
        _key <- hoistMaybe $ Builtin.toKey _key
        _map <- expectConcreteBuiltinMap Map.lookupKey _map
        Map.lookup _key _map
            & maybe _def Domain.getMapValue
            & Pattern.fromTermLike
            & return
    evalLookupOrDefault0 _ _ = Builtin.wrongArity Map.lookupOrDefaultKey

-- | evaluates the map element builtin.
evalElement :: BuiltinAndAxiomSimplifier
evalElement =
    Builtin.functionEvaluator evalElement0
  where
    evalElement0 resultSort [_key, _value] =
        case Builtin.toKey _key of
            Just concrete ->
                Map.singleton concrete (Domain.MapValue _value)
                & returnConcreteMap resultSort
                & TermLike.assertConstructorLikeKeys [_key]
            Nothing ->
                (Ac.returnAc resultSort . Domain.wrapAc)
                Domain.NormalizedAc
                    { elementsWithVariables =
                        [Domain.MapElement (_key, _value)]
                    , concreteElements = Map.empty
                    , opaque = []
                    }
    evalElement0 _ _ = Builtin.wrongArity Map.elementKey

-- | evaluates the map concat builtin.
evalConcat :: BuiltinAndAxiomSimplifier
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    evalConcat0 :: Builtin.Function
    evalConcat0 resultSort [map1, map2] =
        Ac.evalConcatNormalizedOrBottom @Domain.NormalizedMap
            resultSort
            (Ac.toNormalized map1)
            (Ac.toNormalized map2)
    evalConcat0 _ _ = Builtin.wrongArity Map.concatKey

evalUnit :: BuiltinAndAxiomSimplifier
evalUnit =
    Builtin.functionEvaluator evalUnit0
  where
    evalUnit0 resultSort =
        \case
            [] -> returnConcreteMap resultSort Map.empty
            _ -> Builtin.wrongArity Map.unitKey

evalUpdate :: BuiltinAndAxiomSimplifier
evalUpdate =
    Builtin.functionEvaluator evalUpdate0
  where
    evalUpdate0 resultSort [_map, _key, value] = do
        _key <- hoistMaybe $ Builtin.toKey _key
        _map <- expectConcreteBuiltinMap Map.updateKey _map
        Map.insert _key (Domain.MapValue value) _map
            & returnConcreteMap resultSort
            & TermLike.assertConstructorLikeKeys (_key : Map.keys _map)
    evalUpdate0 _ _ = Builtin.wrongArity Map.updateKey

evalInKeys :: Builtin.Function
evalInKeys resultSort [_key, _map] = do
    _key <- hoistMaybe $ Builtin.toKey _key
    _map <- expectConcreteBuiltinMap Map.in_keysKey _map
    Map.member _key _map
        & Bool.asPattern resultSort
        & return
evalInKeys _ _ = Builtin.wrongArity Map.in_keysKey

evalInclusion :: BuiltinAndAxiomSimplifier
evalInclusion =
    Builtin.functionEvaluator evalInclusion0
  where
    evalInclusion0 resultSort [_mapLeft, _mapRight] = do
        _mapLeft <- expectConcreteBuiltinMap Map.inclusionKey _mapLeft
        _mapRight <- expectConcreteBuiltinMap Map.inclusionKey _mapRight
        Map.isSubmapOf _mapLeft _mapRight
            & Bool.asPattern resultSort
            & return
    evalInclusion0 _ _ = Builtin.wrongArity Map.inclusionKey

evalKeys :: BuiltinAndAxiomSimplifier
evalKeys =
    Builtin.functionEvaluator evalKeys0
  where
    evalKeys0 resultSort [_map] = do
        _map <- expectConcreteBuiltinMap Map.keysKey _map
        fmap (const Domain.SetValue) _map
            & Builtin.Set.returnConcreteSet resultSort
    evalKeys0 _ _ = Builtin.wrongArity Map.keysKey

evalKeysList :: BuiltinAndAxiomSimplifier
evalKeysList =
    Builtin.functionEvaluator evalKeysList0
  where
    evalKeysList0 :: Builtin.Function
    evalKeysList0 resultSort [_map] = do
        _map <- expectConcreteBuiltinMap Map.keys_listKey _map
        Map.keys _map
            & fmap TermLike.fromConcrete
            & Seq.fromList
            & Builtin.List.returnList resultSort
    evalKeysList0 _ _ = Builtin.wrongArity Map.keys_listKey

evalRemove :: BuiltinAndAxiomSimplifier
evalRemove =
    Builtin.functionEvaluator evalRemove0
  where
    evalRemove0 :: Builtin.Function
    evalRemove0 resultSort [_map, _key] = do
        let emptyMap = do
                _map <- expectConcreteBuiltinMap Map.removeKey _map
                if Map.null _map
                    then returnConcreteMap resultSort Map.empty
                    else empty
            bothConcrete = do
                _map <- expectConcreteBuiltinMap Map.removeKey _map
                _key <- hoistMaybe $ Builtin.toKey _key
                returnConcreteMap resultSort $ Map.delete _key _map
        emptyMap <|> bothConcrete
    evalRemove0 _ _ = Builtin.wrongArity Map.removeKey

evalRemoveAll :: BuiltinAndAxiomSimplifier
evalRemoveAll =
    Builtin.functionEvaluator evalRemoveAll0
  where
    evalRemoveAll0 :: Builtin.Function
    evalRemoveAll0 resultSort [_map, _set] = do
        let emptyMap = do
                _map <- expectConcreteBuiltinMap Map.removeAllKey _map
                if Map.null _map
                    then returnConcreteMap resultSort Map.empty
                    else empty
            bothConcrete = do
                _map <- expectConcreteBuiltinMap Map.removeAllKey _map
                _set <-
                    Builtin.Set.expectConcreteBuiltinSet
                        Map.removeAllKey
                        _set
                Map.difference _map _set
                    & returnConcreteMap resultSort
        emptyMap <|> bothConcrete
    evalRemoveAll0 _ _ = Builtin.wrongArity Map.removeAllKey

evalSize :: BuiltinAndAxiomSimplifier
evalSize =
    Builtin.functionEvaluator evalSize0
  where
    evalSize0 :: Builtin.Function
    evalSize0 resultSort [_map] = do
        _map <- expectConcreteBuiltinMap Map.sizeKey _map
        (Map.size _map)
            & toInteger
            & Int.asPattern resultSort
            & return
    evalSize0 _ _ = Builtin.wrongArity Map.sizeKey

evalValues :: BuiltinAndAxiomSimplifier
evalValues =
    Builtin.functionEvaluator evalValues0
  where
    evalValues0 resultSort [_map] = do
        _map <- expectConcreteBuiltinMap Map.valuesKey _map
        fmap Domain.getMapValue (Map.elems _map)
            & Seq.fromList
            & Builtin.List.returnList resultSort
    evalValues0 _ _ = Builtin.wrongArity Map.valuesKey

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text BuiltinAndAxiomSimplifier
builtinFunctions =
    Map.fromList
        [ (Map.concatKey, evalConcat)
        , (Map.lookupKey, evalLookup)
        , (Map.lookupOrDefaultKey, evalLookupOrDefault)
        , (Map.elementKey, evalElement)
        , (Map.unitKey, evalUnit)
        , (Map.updateKey, evalUpdate)
        , (Map.in_keysKey, Builtin.functionEvaluator evalInKeys)
        , (Map.keysKey, evalKeys)
        , (Map.keys_listKey, evalKeysList)
        , (Map.removeKey, evalRemove)
        , (Map.removeAllKey, evalRemoveAll)
        , (Map.sizeKey, evalSize)
        , (Map.valuesKey, evalValues)
        , (Map.inclusionKey, evalInclusion)
        ]

{- | Convert a Map-sorted 'TermLike' to its internal representation.

The 'TermLike' is unmodified if it is not Map-sorted. @internalize@ only
operates at the top-most level, it does not descend into the 'TermLike' to
internalize subterms.

 -}
internalize
    :: InternalVariable variable
    => SmtMetadataTools Attribute.Symbol
    -> TermLike variable
    -> TermLike variable
internalize tools termLike
  | fromMaybe False (isMapSort tools sort')
  -- Ac.toNormalized is greedy about 'normalizing' opaque terms, we should only
  -- apply it if we know the term head is a constructor-like symbol.
  , App_ symbol _ <- termLike
  , isConstructorModulo_ symbol =
    case Ac.toNormalized @Domain.NormalizedMap termLike of
        Ac.Bottom                    -> TermLike.mkBottom sort'
        Ac.Normalized termNormalized
          | let unwrapped = Domain.unwrapAc termNormalized
          , null (Domain.elementsWithVariables unwrapped)
          , null (Domain.concreteElements unwrapped)
          , [singleOpaqueTerm] <- Domain.opaque unwrapped
          ->
            -- When the 'normalized' term consists of a single opaque Map-sorted
            -- term, we should prefer to return only that term.
            singleOpaqueTerm
          | otherwise -> Ac.asInternal tools sort' termNormalized
  | otherwise = termLike
  where
    sort' = termLikeSort termLike

{- | Simplify the conjunction or equality of two concrete Map domain values.

When it is used for simplifying equality, one should separately solve the
case ⊥ = ⊥. One should also throw away the term in the returned pattern.

The maps are assumed to have the same sort, but this is not checked. If
multiple sorts are hooked to the same builtin domain, the verifier should
reject the definition.
-}
unifyEquals
    :: forall variable unifier
    .  (InternalVariable variable, MonadUnify unifier)
    => (TermLike variable -> TermLike variable -> unifier (Pattern variable))
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
unifyEquals unifyEqualsChildren first second = do
    tools <- Simplifier.askMetadataTools
    (Monad.guard . fromMaybe False) (isMapSort tools sort1)
    MaybeT $ do
        unifiers <- Monad.Unify.gather (runMaybeT (unifyEquals0 first second))
        case sequence unifiers of
            Nothing -> return Nothing
            Just us -> Monad.Unify.scatter (map Just us)
  where
    sort1 = termLikeSort first

    -- | Unify the two argument patterns.
    unifyEquals0
        :: TermLike variable
        -> TermLike variable
        -> MaybeT unifier (Pattern variable)
    unifyEquals0
        (Builtin_ (Domain.BuiltinMap normalized1))
        (Builtin_ (Domain.BuiltinMap normalized2))
      = do
        tools <- Simplifier.askMetadataTools
        Ac.unifyEqualsNormalized
            tools
            first
            second
            unifyEqualsChildren
            normalized1
            normalized2

    unifyEquals0 pat1 pat2 = do
        firstDomain <- asDomain pat1
        secondDomain <- asDomain pat2
        unifyEquals0 firstDomain secondDomain
      where
        asDomain
            :: TermLike variable
            -> MaybeT unifier (TermLike variable)
        asDomain patt =
            case normalizedOrBottom of
                Ac.Normalized normalized -> do
                    tools <- Simplifier.askMetadataTools
                    return (Ac.asInternal tools sort1 normalized)
                Ac.Bottom ->
                    lift $ Monad.Unify.explainAndReturnBottom
                        "Duplicated elements in normalization."
                        first
                        second
          where
            normalizedOrBottom
                :: Ac.NormalizedOrBottom Domain.NormalizedMap variable
            normalizedOrBottom = Ac.toNormalized patt
