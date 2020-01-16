{-|
Module      : Kore.Step.Simplification.In
Description : Tools for In pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.In
    ( simplify
    ) where

import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeInPredicate_
    )
import qualified Kore.Internal.Predicate as Predicate
    ( markSimplified
    )
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
import qualified Kore.Step.Simplification.Ceil as Ceil
    ( makeEvaluate
    , simplifyEvaluated
    )
import Kore.Step.Simplification.Simplify

{-|'simplify' simplifies an 'In' pattern with 'OrPattern'
children.

Right now this uses the following simplifications:

* bottom in a = bottom
* a in bottom = bottom
* top in a = ceil(a)
* a in top = ceil(a)

TODO(virgil): It does not have yet a special case for children with top terms.
-}
simplify
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> In Sort (OrPattern variable)
    -> simplifier (OrPattern variable)
simplify
    sideCondition
    In { inContainedChild = first, inContainingChild = second }
  =
    simplifyEvaluatedIn sideCondition first second

{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make
'simplifyEvaluatedIn' take an argument of type

> CofreeF (In Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluateIn' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}
simplifyEvaluatedIn
    :: forall variable simplifier
    .  (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> OrPattern variable
    -> OrPattern variable
    -> simplifier (OrPattern variable)
simplifyEvaluatedIn sideCondition first second
  | OrPattern.isFalse first  = return OrPattern.bottom
  | OrPattern.isFalse second = return OrPattern.bottom

  | OrPattern.isTrue first = Ceil.simplifyEvaluated sideCondition second
  | OrPattern.isTrue second = Ceil.simplifyEvaluated sideCondition first

  | otherwise =
    OrPattern.flatten <$> sequence
                            (makeEvaluateIn sideCondition <$> first <*> second)

makeEvaluateIn
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> Pattern variable
    -> Pattern variable
    -> simplifier (OrPattern variable)
makeEvaluateIn sideCondition first second
  | Pattern.isTop first = Ceil.makeEvaluate sideCondition second
  | Pattern.isTop second = Ceil.makeEvaluate sideCondition first
  | Pattern.isBottom first || Pattern.isBottom second = return OrPattern.bottom
  | otherwise = return $ makeEvaluateNonBoolIn first second

makeEvaluateNonBoolIn
    :: InternalVariable variable
    => Pattern variable
    -> Pattern variable
    -> OrPattern variable
makeEvaluateNonBoolIn patt1 patt2 =
    OrPattern.fromPattern Conditional
        { term = mkTop_
        , predicate =
            Predicate.markSimplified
            $ makeInPredicate_
                -- TODO: Wrap in 'contained' and 'container'.
                (Pattern.toTermLike patt1)
                (Pattern.toTermLike patt2)
        , substitution = mempty
        }
