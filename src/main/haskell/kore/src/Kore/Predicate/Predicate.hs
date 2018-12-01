{-|
Module      : Kore.Predicate.Predicate
Description : Data structure holding a predicate and basic tools like
              predicate constructors.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Predicate.Predicate
    ( CommonPredicate -- Constructor not exported on purpose
    , Predicate -- Constructor not exported on purpose
    , pattern PredicateFalse
    , pattern PredicateTrue
    , compactPredicatePredicate
    , isFalse
    , makePredicate
    , makeAndPredicate
    , makeMultipleAndPredicate
    , makeCeilPredicate
    , makeEqualsPredicate
    , makeExistsPredicate
    , makeForallPredicate
    , makeFalsePredicate
    , makeFloorPredicate
    , makeIffPredicate
    , makeImpliesPredicate
    , makeInPredicate
    , makeNotPredicate
    , makeOrPredicate
    , makeMultipleOrPredicate
    , makeTruePredicate
    , allVariables
    , freeVariables
    , Kore.Predicate.Predicate.mapVariables
    , stringFromPredicate
    , substitutionToPredicate
    , unwrapPredicate
    , wrapPredicate
    ) where

import           Control.Comonad
import qualified Control.Comonad.Trans.Cofree as Cofree
import           Control.DeepSeq
                 ( NFData )
import qualified Data.Functor.Foldable as Recursive
import           Data.List
                 ( foldl', nub )
import           Data.Reflection
                 ( Given )
import           Data.Set
                 ( Set )
import           GHC.Generics
                 ( Generic )
import           GHC.Stack
                 ( HasCallStack )

import Kore.AST.Pure as Pure
import Kore.ASTUtils.SmartConstructors
import Kore.ASTUtils.SmartPatterns
       ( pattern Bottom_, pattern Ceil_, pattern Equals_, pattern Floor_,
       pattern In_, pattern Top_ )
import Kore.Error
       ( Error, koreFail )
import Kore.IndexedModule.MetadataTools
       ( SymbolOrAliasSorts )
import Kore.Step.Pattern
import Kore.Unification.Substitution as Substitution
import Kore.Unparser
import Kore.Variables.Free
       ( freePureVariables, pureAllVariables )

{-| 'GenericPredicate' is a wrapper for predicates used for type safety.
Should not be exported, and should be treated as an opaque entity which
can be manipulated only by functions in this module.
-}
newtype GenericPredicate pat = GenericPredicate pat
    deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

{-| 'Predicate' is a user-visible representation for predicates.
-}
type Predicate level variable = GenericPredicate (StepPattern level variable)

{-| 'CommonPredicate' follows the generic convention of particularizing types
to Variable.
-}
type CommonPredicate level = Predicate level Variable

{- 'compactPredicatePredicate' removes one level of 'GenericPredicate' which
sometimes occurs when, say, using Predicates as Traversable.
-}
compactPredicatePredicate
    :: GenericPredicate (GenericPredicate a) -> GenericPredicate a
compactPredicatePredicate (GenericPredicate x) = x

{- 'stringFromPredicate' extracts a string from a GenericPredicate,
useful in tests. This could be replaced by a generic extractor, but, for now,
treating it as an opaque entity seems useful.
-}
stringFromPredicate :: GenericPredicate String -> String
stringFromPredicate (GenericPredicate x) = x

{- 'wrapPredicate' wraps a pattern in a GenericPredicate. This is intended for
predicate evaluation and tests and should not be used outside of that.

We should consider deleting this and implementing the functionality otherwise.
-}
wrapPredicate :: StepPattern level variable -> Predicate level variable
wrapPredicate = GenericPredicate

{- 'unwrapPredicate' wraps a pattern in a GenericPredicate. This should be
not be used outside of that.

We should consider deleting this and implementing the functionality otherwise.
-}
unwrapPredicate :: Predicate level variable -> StepPattern level variable
unwrapPredicate (GenericPredicate p) = p

{-|'PredicateFalse' is a pattern for matching 'bottom' predicates.
-}
pattern PredicateFalse :: Predicate level variable
pattern PredicateFalse <-
    GenericPredicate (Recursive.project -> _ :< BottomPattern _)

{-|'PredicateTrue' is a pattern for matching 'top' predicates.
-}
pattern PredicateTrue :: Predicate level variable
pattern PredicateTrue <-
    GenericPredicate (Recursive.project -> _ :< TopPattern _)

{-|'isFalse' checks whether a predicate matches 'PredicateFalse'.
-}
isFalse :: Predicate level variable -> Bool
isFalse PredicateFalse = True
isFalse _ = False

{-| 'makeMultipleAndPredicate' combines a list of Predicates with 'and',
doing some simplification.
-}
makeMultipleAndPredicate
    ::  ( MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => Sort level
    -> [Predicate level variable]
    -> Predicate level variable
makeMultipleAndPredicate sort =
    foldl' makeAndPredicate (makeTruePredicate sort) . nub
    -- 'and' is idempotent so we eliminate duplicates
    -- TODO: This is O(n^2), consider doing something better.

{-| 'makeMultipleOrPredicate' combines a list of Predicates with 'or',
doing some simplification.
-}
makeMultipleOrPredicate
    ::  ( MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => Sort level
    -> [Predicate level variable]
    -> Predicate level variable
makeMultipleOrPredicate sort =
    foldl' makeOrPredicate (makeFalsePredicate sort) . nub
    -- 'or' is idempotent so we eliminate duplicates
    -- TODO: This is O(n^2), consider doing something better.

{-| 'makeAndPredicate' combines two Predicates with an 'and', doing some
simplification.
-}
makeAndPredicate
    ::  ( MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => Predicate level variable
    -> Predicate level variable
    -> Predicate level variable
makeAndPredicate b@PredicateFalse _ = b
makeAndPredicate _ b@PredicateFalse = b
makeAndPredicate PredicateTrue second = second
makeAndPredicate first PredicateTrue = first
makeAndPredicate p@(GenericPredicate first) (GenericPredicate second)
  | first == second = p
  | otherwise =
    GenericPredicate (mkAnd first second)

{-| 'makeOrPredicate' combines two Predicates with an 'or', doing
some simplification.
-}
makeOrPredicate
    ::  ( MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => Predicate level variable
    -> Predicate level variable
    -> Predicate level variable
makeOrPredicate t@PredicateTrue _ = t
makeOrPredicate _ t@PredicateTrue = t
makeOrPredicate PredicateFalse second = second
makeOrPredicate first PredicateFalse = first
makeOrPredicate p@(GenericPredicate first) (GenericPredicate second)
  | first == second = p
  | otherwise =
    GenericPredicate (mkOr first second)

{-| 'makeImpliesPredicate' combines two Predicates into an
implication, doing some simplification.
-}
makeImpliesPredicate
    ::  ( MetaOrObject level
        , Unparse (variable level)
        )
    => Predicate level variable
    -> Predicate level variable
    -> Predicate level variable
makeImpliesPredicate f@PredicateFalse _ =
    GenericPredicate (mkTop patternSort)
  where
    GenericPredicate (extract -> Valid { patternSort }) = f
makeImpliesPredicate _ t@PredicateTrue = t
makeImpliesPredicate PredicateTrue second = second
makeImpliesPredicate first PredicateFalse = makeNotPredicate first
makeImpliesPredicate (GenericPredicate first) (GenericPredicate second) =
    GenericPredicate $ mkImplies first second

{-| 'makeIffPredicate' combines two evaluated with an 'iff', doing
some simplification.
-}
makeIffPredicate
    ::  ( MetaOrObject level
        , Unparse (variable level)
        )
    => Predicate level variable
    -> Predicate level variable
    -> Predicate level variable
makeIffPredicate PredicateFalse second = makeNotPredicate second
makeIffPredicate PredicateTrue second = second
makeIffPredicate first PredicateFalse = makeNotPredicate first
makeIffPredicate first PredicateTrue = first
makeIffPredicate (GenericPredicate first) (GenericPredicate second) =
    GenericPredicate $ mkIff first second

{-| 'makeNotPredicate' negates an evaluated Predicate, doing some
simplification.
-}
makeNotPredicate
    ::  ( MetaOrObject level
        , Unparse (variable level)
        )
    => Predicate level variable
    -> Predicate level variable
makeNotPredicate f@PredicateFalse =
    GenericPredicate (mkTop patternSort)
  where
    GenericPredicate (extract -> Valid { patternSort }) = f
makeNotPredicate t@PredicateTrue  =
    GenericPredicate (mkBottom patternSort)
  where
    GenericPredicate (extract -> Valid { patternSort }) = t
makeNotPredicate (GenericPredicate predicate) =
    GenericPredicate $ mkNot predicate

{-| 'makeEqualsPredicate' combines two patterns with equals, producing a
predicate.
-}
makeEqualsPredicate
    ::  ( HasCallStack
        , MetaOrObject level
        , Unparse (variable level)
        )
    => Sort level
    -> StepPattern level variable
    -> StepPattern level variable
    -> Predicate level variable
makeEqualsPredicate resultSort first second =
    GenericPredicate $ mkEquals resultSort first second

{-| 'makeInPredicate' combines two patterns with 'in', producing a
predicate.
-}
makeInPredicate
    ::  ( HasCallStack
        , MetaOrObject level
        , Unparse (variable level)
        )
    => Sort level
    -> StepPattern level variable
    -> StepPattern level variable
    -> Predicate level variable
makeInPredicate resultSort first second =
    GenericPredicate $ mkIn resultSort first second

{-| 'makeCeilPredicate' takes the 'ceil' of a pattern, producing a
predicate.
-}
makeCeilPredicate
    :: MetaOrObject level
    => Sort level
    -> StepPattern level variable
    -> Predicate level variable
makeCeilPredicate resultSort patt =
    GenericPredicate $ mkCeil resultSort patt

{-| 'makeFloorPredicate' takes the 'floor' of a pattern, producing a
predicate.
-}
makeFloorPredicate
    :: MetaOrObject level
    => Sort level
    -> StepPattern level variable
    -> Predicate level variable
makeFloorPredicate resultSort patt =
    GenericPredicate $ mkFloor resultSort patt

{-| Existential quantification for the given variable in the given predicate.
-}
makeExistsPredicate
    :: MetaOrObject level
    => variable level
    -> Predicate level variable
    -> Predicate level variable
makeExistsPredicate _ p@PredicateFalse = p
makeExistsPredicate _ t@PredicateTrue = t
makeExistsPredicate v (GenericPredicate p) =
    GenericPredicate $ mkExists v p

{-| Universal quantification for the given variable in the given predicate.
-}
makeForallPredicate
    :: MetaOrObject level
    => variable level
    -> Predicate level variable
    -> Predicate level variable
makeForallPredicate _ p@PredicateFalse = p
makeForallPredicate _ t@PredicateTrue = t
makeForallPredicate v (GenericPredicate p) =
    GenericPredicate $ mkForall v p

{-| 'makeTruePredicate' produces a predicate wrapping a 'top'.
-}
makeTruePredicate
    :: MetaOrObject level
    => Sort level -> Predicate level variable
makeTruePredicate sort =
    GenericPredicate (mkTop sort)

{-| 'makeFalsePredicate' produces a predicate wrapping a 'bottom'.
-}
makeFalsePredicate
    :: MetaOrObject level
    => Sort level -> Predicate level variable
makeFalsePredicate sort =
    GenericPredicate (mkBottom sort)

makePredicate
    :: forall level variable e .
        ( MetaOrObject level
        , Eq (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => StepPattern level variable
    -> Either (Error e) (Predicate level variable)
makePredicate = Recursive.elgot makePredicateBottomUp makePredicateTopDown
  where
    makePredicateBottomUp
        :: Base (StepPattern level variable)
            (Either (Error e) (Predicate level variable))
        -> Either (Error e) (Predicate level variable)
    makePredicateBottomUp (valid :< patE) = do
        let Valid { patternSort } = valid
        pat <- sequence patE
        case pat of
            TopPattern _ -> return (makeTruePredicate patternSort)
            BottomPattern _ -> return (makeFalsePredicate patternSort)
            AndPattern p -> return $ makeAndPredicate (andFirst p) (andSecond p)
            OrPattern p -> return $ makeOrPredicate (orFirst p) (orSecond p)
            IffPattern p -> return $ makeIffPredicate (iffFirst p) (iffSecond p)
            ImpliesPattern p -> return $
                makeImpliesPredicate (impliesFirst p) (impliesSecond p)
            NotPattern p -> return $ makeNotPredicate (notChild p)
            ExistsPattern p -> return $
                makeExistsPredicate (existsVariable p) (existsChild p)
            ForallPattern p -> return $
                makeForallPredicate (forallVariable p) (forallChild p)
            p ->
                (koreFail . unlines)
                    [ "Cannot translate to predicate:"
                    , show p
                    ]
    makePredicateTopDown
        :: StepPattern level variable
        -> Either
            (Either (Error e) (Predicate level variable))
            (Base (StepPattern level variable) (StepPattern level variable))
    makePredicateTopDown (Recursive.project -> projected@(valid :< pat)) =
        case pat of
            CeilPattern Ceil { ceilChild } ->
                Left $ pure $ makeCeilPredicate patternSort ceilChild
            FloorPattern Floor { floorChild } ->
                Left $ pure $ makeFloorPredicate patternSort floorChild
            EqualsPattern Equals { equalsFirst, equalsSecond } ->
                (Left . pure)
                    (makeEqualsPredicate
                        patternSort
                        equalsFirst
                        equalsSecond
                    )
            InPattern In { inContainedChild, inContainingChild } ->
                (Left . pure)
                    (makeInPredicate
                        patternSort
                        inContainedChild
                        inContainingChild
                    )
            _ -> Right projected
      where
        Valid { patternSort } = valid

{- | Replace all variables in a @Predicate@ using the provided mapping.
-}
mapVariables
    :: (from level -> to level) -> Predicate level from -> Predicate level to
mapVariables f = fmap (Pure.mapVariables f)

{- | Extract the set of all (free and bound) variables from a @Predicate@.
-}
allVariables
    :: Ord (variable level)
    => Predicate level variable
    -> Set (variable level)
allVariables = pureAllVariables . unwrapPredicate

{- | Extract the set of free variables from a @Predicate@.
-}
freeVariables
    :: (MetaOrObject level , Ord (variable level))
    => Predicate level variable
    -> Set (variable level)
freeVariables = freePureVariables . unwrapPredicate

{- | 'substitutionToPredicate' transforms a substitution in a predicate.

    An empty substitution list returns a true predicate. A non-empty
    substitution returns a conjunction of variable/substitution equalities.

-}
substitutionToPredicate
    ::  ( HasCallStack
        , MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => Sort level
    -> Substitution level variable
    -> Predicate level variable
substitutionToPredicate sort =
    makeMultipleAndPredicate sort
    . fmap (singleSubstitutionToPredicate sort)
    . Substitution.unwrap

singleSubstitutionToPredicate
    ::  ( HasCallStack
        , MetaOrObject level
        , Unparse (variable level)
        )
    => Sort level
    -> (variable level, StepPattern level variable)
    -> Predicate level variable
singleSubstitutionToPredicate sort (var, patt) =
    makeEqualsPredicate sort (mkVar patternSort var) patt
  where
    Valid { patternSort } = extract patt
