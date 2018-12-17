{-|
Module      : Kore.Step.Simplification.Not
Description : Tools for Not pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Not
    ( makeEvaluate
    , simplify
    ) where

import qualified Data.Functor.Foldable as Recursive

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeNotPredicate, makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..), substitutionToPredicate )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( toMLPattern, top )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern, makeFromSinglePurePattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( extractPatterns, isFalse, isTrue, make, toExpandedPattern )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )
import           Kore.Unparser

{-|'simplify' simplifies a 'Not' pattern with an 'OrOfExpandedPattern'
child.

Right now this uses the following:

* not top = bottom
* not bottom = top

-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => Not level (OrOfExpandedPattern level variable)
    ->  ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplify Not { notChild, notSort }
  | OrOfExpandedPattern.isFalse notChild =
    ( OrOfExpandedPattern.make [ExpandedPattern.top notSort]
    , SimplificationProof
    )
  | OrOfExpandedPattern.isTrue notChild =
    (OrOfExpandedPattern.make [], SimplificationProof)
  | otherwise =
    case OrOfExpandedPattern.extractPatterns notChild of
        [patt] -> makeEvaluate patt
        _ ->
            ( makeFromSinglePurePattern
                (mkNot
                    (ExpandedPattern.toMLPattern
                        (OrOfExpandedPattern.toExpandedPattern notChild)
                    )
                )
            , SimplificationProof
            )

{-|'makeEvaluate' simplifies a 'Not' pattern given its 'ExpandedPattern'
child.

See 'simplify' for details.
-}
makeEvaluate
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level variable
    -> (OrOfExpandedPattern level variable, SimplificationProof level)
makeEvaluate
    Predicated {term, predicate, substitution}
  =
    ( OrOfExpandedPattern.make
        [ Predicated
            { term = makeTermNot term
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        , Predicated
            { term = mkTopOf term
            , predicate =
                makeNotPredicate
                    (makeAndPredicate
                        predicate
                        (substitutionToPredicate substitution)
                    )
            , substitution = mempty
            }
        ]
    , SimplificationProof
    )

makeTermNot
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => StepPattern level variable
    -> StepPattern level variable
-- TODO: maybe other simplifications like
-- not ceil = floor not
-- not forall = exists not
makeTermNot term@(Recursive.project -> Valid { patternSort } :< projected)
  | BottomPattern _ <- projected = mkTop patternSort
  | TopPattern _ <- projected = mkBottom patternSort
  | otherwise = mkNot term
