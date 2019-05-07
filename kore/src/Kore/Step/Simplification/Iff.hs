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

import qualified Kore.Attribute.Symbol as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import qualified Kore.Internal.MultiOr as MultiOr
import           Kore.Internal.OrPattern
                 ( OrPattern )
import qualified Kore.Internal.OrPattern as OrPattern
import           Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike
import qualified Kore.Predicate.Predicate as Syntax.Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, Simplifier, TermLikeSimplifier )
import qualified Kore.Step.Simplification.Not as Not
                 ( makeEvaluate, simplifyEvaluated )
import           Kore.Syntax.Iff
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-|'simplify' simplifies an 'Iff' pattern with 'OrPattern'
children.

Right now this has special cases only for top and bottom children
and for children with top terms.
-}
simplify
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Iff Sort (OrPattern variable)
    -> Simplifier (OrPattern variable)
simplify
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    Iff { iffFirst = first, iffSecond = second }
  =
    simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
        second

{-| evaluates an 'Iff' given its two 'OrPattern' children.

See 'simplify' for detailed documentation.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Iff Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache information
besides the pattern sort, which will make it even more useful to carry around.

-}
simplifyEvaluated
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> OrPattern variable
    -> OrPattern variable
    -> Simplifier (OrPattern variable)
simplifyEvaluated
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrPattern.isTrue first = return second
  | OrPattern.isFalse first =
    Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        second
  | OrPattern.isTrue second = return first
  | OrPattern.isFalse second =
    Not.simplifyEvaluated
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
                (OrPattern.toPattern first)
                (OrPattern.toPattern second)
  where
    firstPatterns = MultiOr.extractPatterns first
    secondPatterns = MultiOr.extractPatterns second

{-| evaluates an 'Iff' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => Pattern variable
    -> Pattern variable
    -> OrPattern variable
makeEvaluate first second
  | Pattern.isTop first = OrPattern.fromPatterns [second]
  | Pattern.isBottom first = Not.makeEvaluate second
  | Pattern.isTop second = OrPattern.fromPatterns [first]
  | Pattern.isBottom second = Not.makeEvaluate first
  | otherwise = makeEvaluateNonBoolIff first second

makeEvaluateNonBoolIff
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => Pattern variable
    -> Pattern variable
    -> OrPattern variable
makeEvaluateNonBoolIff
    patt1@Conditional
        { term = firstTerm
        , predicate = firstPredicate
        , substitution = firstSubstitution
        }
    patt2@Conditional
        { term = secondTerm
        , predicate = secondPredicate
        , substitution = secondSubstitution
        }
  | isTop firstTerm, isTop secondTerm
  =
    OrPattern.fromPatterns
        [ Conditional
            { term = firstTerm
            , predicate =
                Syntax.Predicate.makeIffPredicate
                    (Syntax.Predicate.makeAndPredicate
                        firstPredicate
                        (Syntax.Predicate.fromSubstitution firstSubstitution)
                    )
                    (Syntax.Predicate.makeAndPredicate
                        secondPredicate
                        (Syntax.Predicate.fromSubstitution secondSubstitution)
                    )
            , substitution = mempty
            }
        ]
  | otherwise =
    OrPattern.fromTermLike $ mkIff
        (Pattern.toMLPattern patt1)
        (Pattern.toMLPattern patt2)
