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

import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
import qualified Kore.Internal.TermLike as TermLike
    ( markSimplified
    )
import qualified Kore.Step.Simplification.Not as Not
    ( makeEvaluate
    , simplifyEvaluated
    )
import Kore.Step.Simplification.Simplify

{-|'simplify' simplifies an 'Iff' pattern with 'OrPattern'
children.

Right now this has special cases only for top and bottom children
and for children with top terms.
-}
simplify
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> Iff Sort (OrPattern variable)
    -> simplifier (OrPattern variable)
simplify sideCondition Iff { iffFirst = first, iffSecond = second } =
    simplifyEvaluated sideCondition first second

{-| evaluates an 'Iff' given its two 'OrPattern' children.

See 'simplify' for detailed documentation.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Iff Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}
simplifyEvaluated
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> OrPattern variable
    -> OrPattern variable
    -> simplifier (OrPattern variable)
simplifyEvaluated
    sideCondition
    first
    second
  | OrPattern.isTrue first   = return second
  | OrPattern.isFalse first  = Not.simplifyEvaluated sideCondition second
  | OrPattern.isTrue second  = return first
  | OrPattern.isFalse second = Not.simplifyEvaluated sideCondition first
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
    :: InternalVariable variable
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
    :: InternalVariable variable
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
                Predicate.markSimplified
                $ Predicate.makeIffPredicate
                    (Predicate.makeAndPredicate
                        firstPredicate
                        (Predicate.fromSubstitution firstSubstitution)
                    )
                    (Predicate.makeAndPredicate
                        secondPredicate
                        (Predicate.fromSubstitution secondSubstitution)
                    )
            , substitution = mempty
            }
        ]
  | otherwise =
    OrPattern.fromTermLike $ TermLike.markSimplified $ mkIff
        (Pattern.toTermLike patt1)
        (Pattern.toTermLike patt2)
