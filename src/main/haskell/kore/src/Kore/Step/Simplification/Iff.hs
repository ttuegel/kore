{-|
Module      : Kore.Step.Simplification.Iff
Description : Tools for Iff pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Iff
    ( makeEvaluate
    , simplify
    ) where

import qualified Data.Functor.Foldable as Recursive

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeIffPredicate, makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..), substitutionToPredicate )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( extractPatterns, isFalse, isTrue, make, toExpandedPattern )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )
import qualified Kore.Step.Simplification.Not as Not
import           Kore.Unparser

{-|'simplify' simplifies an 'Iff' pattern with 'OrOfExpandedPattern'
children.

Right now this has special cases only for top and bottom children
and for children with top terms.
-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => Iff level (OrOfExpandedPattern level variable)
    ->  ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplify
    Iff
        { iffFirst = first
        , iffSecond = second
        , iffSort
        }
  | OrOfExpandedPattern.isTrue first =
    (second, SimplificationProof)
  | OrOfExpandedPattern.isFalse first =
    Not.simplify Not { notSort = iffSort, notChild = second }
  | OrOfExpandedPattern.isTrue second =
    (first, SimplificationProof)
  | OrOfExpandedPattern.isFalse second =
    Not.simplify Not { notSort = iffSort, notChild = first }
  | otherwise =
    case ( firstPatterns, secondPatterns )
      of
        ([firstP], [secondP]) -> makeEvaluate firstP secondP
        _ ->
            makeEvaluate
                (OrOfExpandedPattern.toExpandedPattern first)
                (OrOfExpandedPattern.toExpandedPattern second)
  where
    firstPatterns = OrOfExpandedPattern.extractPatterns first
    secondPatterns = OrOfExpandedPattern.extractPatterns second

{-| evaluates an 'Iff' given its two 'ExpandedPattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level variable
    -> ExpandedPattern level variable
    -> (OrOfExpandedPattern level variable, SimplificationProof level)
makeEvaluate first second
  | ExpandedPattern.isTop first =
    (OrOfExpandedPattern.make [second], SimplificationProof)
  | ExpandedPattern.isBottom first =
    (fst $ Not.makeEvaluate second, SimplificationProof)
  | ExpandedPattern.isTop second =
    (OrOfExpandedPattern.make [first], SimplificationProof)
  | ExpandedPattern.isBottom second =
    (fst $ Not.makeEvaluate first, SimplificationProof)
  | otherwise =
    makeEvaluateNonBoolIff first second

makeEvaluateNonBoolIff
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level variable
    -> ExpandedPattern level variable
    -> (OrOfExpandedPattern level variable, SimplificationProof level)
makeEvaluateNonBoolIff
    patt1@Predicated
        { term = firstTerm
        , predicate = firstPredicate
        , substitution = firstSubstitution
        }
    patt2@Predicated
        { term = secondTerm
        , predicate = secondPredicate
        , substitution = secondSubstitution
        }
  | (Recursive.project -> _ :< TopPattern _) <- firstTerm
  , (Recursive.project -> _ :< TopPattern _) <- secondTerm
  =
    ( OrOfExpandedPattern.make
        [ Predicated
            { term = firstTerm
            , predicate =
                makeIffPredicate
                    (makeAndPredicate
                        firstPredicate
                        (substitutionToPredicate firstSubstitution))
                    (makeAndPredicate
                        secondPredicate
                        (substitutionToPredicate secondSubstitution)
                    )
            , substitution = mempty
            }
        ]
    , SimplificationProof
    )
  | otherwise =
    ( OrOfExpandedPattern.make
        [ Predicated
            { term = mkIff
                (ExpandedPattern.toMLPattern patt1)
                (ExpandedPattern.toMLPattern patt2)
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        ]
    , SimplificationProof
    )
