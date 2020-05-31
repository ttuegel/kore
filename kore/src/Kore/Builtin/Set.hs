{- |
Module      : Kore.Builtin.Set
Description : Built-in sets
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Set as Set
@
 -}
module Kore.Builtin.Set
    ( sort
    , assertSort
    , isSetSort
    , verifiers
    , builtinFunctions
    , Domain.Builtin
    , returnConcreteSet
    , Set.asTermLike
    , internalize
    , expectBuiltinSet
    , expectConcreteBuiltinSet
    , evalConcat
    , evalElement
    , evalUnit
      -- * Unification
    , unifyEquals
    ) where

import Prelude.Kore

import Control.Error
    ( MaybeT (MaybeT)
    , fromMaybe
    , hoistMaybe
    , runMaybeT
    )
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
    ( toList
    )
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
    ( Symbol
    )
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
import qualified Kore.Builtin.List as List
import qualified Kore.Builtin.Set.Set as Set
import qualified Kore.Domain.Builtin as Domain
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    )
import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeCeilPredicate
    )
import Kore.Internal.TermLike
    ( pattern App_
    , pattern Builtin_
    , InternalVariable
    , TermLike
    , Void
    , mkSort
    , termLikeSort
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Sort
    ( Sort
    )
import Kore.Step.Simplification.Simplify as Simplifier
import Kore.Syntax.Sentence
    ( SentenceSort (SentenceSort)
    )
import qualified Kore.Syntax.Sentence as Sentence.DoNotUse
    ( SentenceSort (..)
    )
import Kore.Unification.Unify
    ( MonadUnify
    )
import qualified Kore.Unification.Unify as Monad.Unify

{- | Builtin name of the @Set@ sort.
 -}
sort :: Text
sort = "SET.Set"

{- | Is the given sort hooked to the builtin Set sort?

Returns Nothing if the sort is unknown (i.e. the _PREDICATE sort).
Returns Just False if the sort is a variable.
-}
isSetSort :: SmtMetadataTools attrs -> Sort -> Maybe Bool
isSetSort = Builtin.isSort sort

{- | Verify that the sort is hooked to the builtin @Set@ sort.

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
        Builtin.assertSymbolHook indexedModule unitId Set.unitKey
        Builtin.assertSymbolResultSort indexedModule unitId expectedSort
        elementId <- Builtin.getElementId attrs
        Builtin.assertSymbolHook indexedModule elementId Set.elementKey
        Builtin.assertSymbolResultSort indexedModule elementId expectedSort
        concatId <- Builtin.getConcatId attrs
        Builtin.assertSymbolHook indexedModule concatId Set.concatKey
        Builtin.assertSymbolResultSort indexedModule concatId expectedSort
        return ()
      where
        SentenceSort { sentenceSortName } = sentenceSort
        expectedSort = mkSort sentenceSortName

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( Set.concatKey
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( Set.elementKey
      , Builtin.verifySymbol assertSort [acceptAnySort]
      )
    , ( Set.unitKey
      , Builtin.verifySymbol assertSort []
      )
    , ( Set.inKey
      , Builtin.verifySymbol Bool.assertSort [acceptAnySort, assertSort]
      )
    , ( Set.differenceKey
      , Builtin.verifySymbol assertSort [assertSort, assertSort]
      )
    , ( Set.toListKey
      , Builtin.verifySymbol List.assertSort [assertSort]
      )
    , ( Set.sizeKey
      , Builtin.verifySymbol Int.assertSort [assertSort]
      )
    , ( Set.intersectionKey
      , Builtin.verifySymbol assertSort [assertSort, assertSort]
      )
    , ( Set.list2setKey
      , Builtin.verifySymbol assertSort [List.assertSort]
      )
    ]

{- | Returns @empty@ if the argument is not a @NormalizedSet@ domain value.

Returns the @NormalizedSet@ otherwise.
-}
expectBuiltinSet
    :: MonadSimplify m
    => Text  -- ^ Context for error message
    -> TermLike variable  -- ^ Operand pattern
    -> MaybeT m (Ac.TermNormalizedAc Domain.NormalizedSet variable)
expectBuiltinSet ctx set =
    case set of
        Builtin_ domain ->
            case domain of
                Domain.BuiltinSet Domain.InternalAc { builtinAcChild } ->
                    return builtinAcChild
                _ ->
                    Builtin.verifierBug
                    $ Text.unpack ctx ++ ": Domain value is not a set"
        _ -> empty

{- | Returns @empty@ if the argument is not a @NormalizedSet@ domain value
which consists only of concrete elements.

Returns the @Set@ of concrete elements otherwise.
-}
expectConcreteBuiltinSet
    :: MonadSimplify m
    => Text  -- ^ Context for error message
    -> TermLike variable  -- ^ Operand pattern
    -> MaybeT m (Map (TermLike Void) (Domain.SetValue (TermLike variable)))
expectConcreteBuiltinSet ctx _set = do
    _set <- expectBuiltinSet ctx _set
    case Domain.unwrapAc _set of
        Domain.NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = []
            } -> return concreteElements
        _ -> empty

{- | Converts a @Set@ of concrete elements to a @NormalizedSet@ and returns it
as a function result.
-}
returnConcreteSet
    :: (MonadSimplify m, InternalVariable variable)
    => Sort
    -> Map (TermLike Void) (Domain.SetValue (TermLike variable))
    -> m (Pattern variable)
returnConcreteSet = Ac.returnConcreteAc

evalElement :: Builtin.Function
evalElement resultSort [_elem] =
    case Builtin.toKey _elem of
        Just concrete ->
            TermLike.assertConstructorLikeKeys [_elem]
            $ returnConcreteSet
                resultSort
                (Map.singleton concrete Domain.SetValue)
        Nothing ->
            (Ac.returnAc resultSort . Domain.wrapAc)
            Domain.NormalizedAc
                { elementsWithVariables =
                    [Domain.SetElement _elem]
                , concreteElements = Map.empty
                , opaque = []
                }
evalElement _ _ = Builtin.wrongArity Set.elementKey

evalIn :: Builtin.Function
evalIn resultSort [_elem, _set] = do
    let setSymbolic = do
            _elem <- hoistMaybe $ Builtin.toKey _elem
            _set' <- expectBuiltinSet Set.inKey _set
            let result = Domain.isConcreteKeyOfAc _elem _set'
            returnIfTrueAndDefined result _set
        bothSymbolic = do
            _set' <- expectBuiltinSet Set.inKey _set
            let result = Domain.isSymbolicKeyOfAc _elem _set'
            returnIfTrueAndDefined result _set
        bothConcrete = do
            _elem <- hoistMaybe $ Builtin.toKey _elem
            _set <- expectConcreteBuiltinSet Set.inKey _set
            Map.member _elem _set
                & asExpandedBoolPattern
                & return
    setSymbolic <|> bothSymbolic <|> bothConcrete
  where
    asExpandedBoolPattern = Bool.asPattern resultSort
    returnIfTrueAndDefined result setTerm
      | result = do
        let condition =
                Conditional.fromPredicate $ makeCeilPredicate resultSort setTerm
            trueWithCondition =
                Pattern.andCondition
                    (asExpandedBoolPattern result)
                    condition
        return trueWithCondition
      | otherwise = empty
evalIn _ _ = Builtin.wrongArity Set.inKey

evalUnit :: Builtin.Function
evalUnit resultSort =
    \case
        [] -> returnConcreteSet resultSort Map.empty
        _ -> Builtin.wrongArity Set.unitKey

evalConcat :: Builtin.Function
evalConcat resultSort [set1, set2] =
    Ac.evalConcatNormalizedOrBottom @Domain.NormalizedSet
        resultSort
        (Ac.toNormalized set1)
        (Ac.toNormalized set2)
evalConcat _ _ = Builtin.wrongArity Set.concatKey

evalDifference :: Builtin.Function
evalDifference resultSort [_set1, _set2] = do
    let rightIdentity = do
            _set2 <- expectConcreteBuiltinSet ctx _set2
            if Map.null _set2
                then return (Pattern.fromTermLike _set1)
                else empty
        bothConcrete = do
            _set1 <- expectConcreteBuiltinSet ctx _set1
            _set2 <- expectConcreteBuiltinSet ctx _set2
            returnConcreteSet resultSort (Map.difference _set1 _set2)
    rightIdentity <|> bothConcrete
  where
    ctx = Set.differenceKey
evalDifference _ _ = Builtin.wrongArity Set.differenceKey

evalToList :: Builtin.Function
evalToList resultSort [_set] = do
    _set <- expectConcreteBuiltinSet Set.toListKey _set
    map dropNoValue (Map.toList _set)
        & Seq.fromList
        & fmap TermLike.fromConcrete
        & List.returnList resultSort
  where
    dropNoValue (a, Domain.SetValue) = a
evalToList _ _ = Builtin.wrongArity Set.toListKey


evalSize :: Builtin.Function
evalSize resultSort [_set] = do
    _set <- expectConcreteBuiltinSet Set.sizeKey _set
    Map.size _set
        & toInteger
        & Int.asPattern resultSort
        & return
evalSize _ _ = Builtin.wrongArity Set.sizeKey

evalIntersection :: Builtin.Function
evalIntersection resultSort [_set1, _set2] = do
    _set1 <- expectConcreteBuiltinSet ctx _set1
    _set2 <- expectConcreteBuiltinSet ctx _set2
    returnConcreteSet resultSort (Map.intersection _set1 _set2)
  where
    ctx = Set.intersectionKey
evalIntersection _ _ = Builtin.wrongArity Set.intersectionKey

evalList2set :: Builtin.Function
evalList2set resultSort [_list] = do
    _list <- List.expectConcreteBuiltinList Set.list2setKey _list
    let _set =
            fmap (\x -> (x, Domain.SetValue)) _list
            & Foldable.toList
            & Map.fromList
            & TermLike.assertConstructorLikeKeys _list
    returnConcreteSet resultSort _set
evalList2set _ _ = Builtin.wrongArity Set.list2setKey

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text BuiltinAndAxiomSimplifier
builtinFunctions =
    Map.fromList
        [ (Set.concatKey, Builtin.functionEvaluator evalConcat)
        , (Set.elementKey, Builtin.functionEvaluator evalElement)
        , (Set.unitKey, Builtin.functionEvaluator evalUnit)
        , (Set.inKey, Builtin.functionEvaluator evalIn)
        , (Set.differenceKey, Builtin.functionEvaluator evalDifference)
        , (Set.toListKey, Builtin.functionEvaluator evalToList)
        , (Set.sizeKey, Builtin.functionEvaluator evalSize)
        , (Set.intersectionKey, Builtin.functionEvaluator evalIntersection)
        , (Set.list2setKey, Builtin.functionEvaluator evalList2set)
        ]

{- | Convert a Set-sorted 'TermLike' to its internal representation.

The 'TermLike' is unmodified if it is not Set-sorted. @internalize@ only
operates at the top-most level, it does not descend into the 'TermLike' to
internalize subterms.

 -}
internalize
    :: InternalVariable variable
    => SmtMetadataTools Attribute.Symbol
    -> TermLike variable
    -> TermLike variable
internalize tools termLike
  | fromMaybe False (isSetSort tools sort')
  -- Ac.toNormalized is greedy about 'normalizing' opaque terms, we should only
  -- apply it if we know the term head is a constructor-like symbol.
  , App_ symbol _ <- termLike
  , isConstructorModulo_ symbol =
    case Ac.toNormalized @Domain.NormalizedSet termLike of
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

{- | Simplify the conjunction or equality of two concrete Set domain values.

    When it is used for simplifying equality, one should separately solve the
    case ⊥ = ⊥. One should also throw away the term in the returned pattern.

    The sets are assumed to have the same sort, but this is not checked. If
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
unifyEquals
    unifyEqualsChildren
    first
    second
  = do
    tools <- Simplifier.askMetadataTools
    (Monad.guard . fromMaybe False) (isSetSort tools sort1)
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
        (Builtin_ (Domain.BuiltinSet normalized1))
        (Builtin_ (Domain.BuiltinSet normalized2))
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
                :: Ac.NormalizedOrBottom Domain.NormalizedSet variable
            normalizedOrBottom = Ac.toNormalized patt
