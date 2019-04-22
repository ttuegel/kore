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
    , simplifyEvaluated
    ) where

import qualified Data.Functor.Foldable as Recursive

import           Kore.AST.Pure
import           Kore.AST.Valid
import qualified Kore.Attribute.Symbol as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeIffPredicate, makeTruePredicate )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Representation.ExpandedPattern
                 ( ExpandedPattern, Predicated (..), substitutionToPredicate )
import qualified Kore.Step.Representation.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( extractPatterns, make )
import           Kore.Step.Representation.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.Representation.OrOfExpandedPattern as OrOfExpandedPattern
                 ( isFalse, isTrue, toExpandedPattern )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, SimplificationProof (..),
                 Simplifier, StepPatternSimplifier, gather )
import qualified Kore.Step.Simplification.Not as Not
                 ( makeEvaluate, simplifyEvaluated )
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-|'simplify' simplifies an 'Iff' pattern with 'OrOfExpandedPattern'
children.

Right now this has special cases only for top and bottom children
and for children with top terms.
-}
simplify
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => MetadataTools Object Attribute.Symbol
    -> PredicateSubstitutionSimplifier Object
    -> StepPatternSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object
    -> Iff Object (OrOfExpandedPattern Object variable)
    -> Simplifier
        (OrOfExpandedPattern Object variable, SimplificationProof Object)
simplify
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    Iff
        { iffFirst = first
        , iffSecond = second
        }
  =
    fmap withProof $ simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
        second
  where
    withProof a = (a, SimplificationProof)

{-| evaluates an 'Iff' given its two 'OrOfExpandedPattern' children.

See 'simplify' for detailed documentation.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Iff level) (Valid level) (OrOfExpandedPattern level variable)

instead of two 'OrOfExpandedPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Valid' annotation will eventually cache information
besides the pattern sort, which will make it even more useful to carry around.

-}
simplifyEvaluated
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => MetadataTools Object Attribute.Symbol
    -> PredicateSubstitutionSimplifier Object
    -> StepPatternSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object
    -> OrOfExpandedPattern Object variable
    -> OrOfExpandedPattern Object variable
    -> Simplifier (OrOfExpandedPattern Object variable)
simplifyEvaluated
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrOfExpandedPattern.isTrue first = return second
  | OrOfExpandedPattern.isFalse first =
    gather $ Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        second
  | OrOfExpandedPattern.isTrue second = return first
  | OrOfExpandedPattern.isFalse second =
    gather $ Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
  | otherwise =
    return $ case ( firstPatterns, secondPatterns ) of
        ([firstP], [secondP]) -> makeEvaluate firstP secondP
        _ ->
            makeEvaluate
                (OrOfExpandedPattern.toExpandedPattern first)
                (OrOfExpandedPattern.toExpandedPattern second)
  where
    firstPatterns = MultiOr.extractPatterns first
    secondPatterns = MultiOr.extractPatterns second

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
    -> OrOfExpandedPattern level variable
makeEvaluate first second
  | ExpandedPattern.isTop first = MultiOr.make [second]
  | ExpandedPattern.isBottom first = Not.makeEvaluate second
  | ExpandedPattern.isTop second = MultiOr.make [first]
  | ExpandedPattern.isBottom second = Not.makeEvaluate first
  | otherwise = makeEvaluateNonBoolIff first second

makeEvaluateNonBoolIff
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level variable
    -> ExpandedPattern level variable
    -> OrOfExpandedPattern level variable
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
    MultiOr.make
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
  | otherwise =
    MultiOr.make
        [ Predicated
            { term = mkIff
                (ExpandedPattern.toMLPattern patt1)
                (ExpandedPattern.toMLPattern patt2)
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        ]
