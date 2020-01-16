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
    , makeEvaluatePredicate
    , simplify
    , simplifyEvaluated
    , simplifyEvaluatedPredicate
    ) where

import qualified Data.Foldable as Foldable

import Branch
import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import qualified Kore.Internal.MultiAnd as MultiAnd
import Kore.Internal.MultiOr
    ( MultiOr (..)
    )
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrCondition
    ( OrCondition
    )
import qualified Kore.Internal.OrCondition as OrCondition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeNotPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike hiding
    ( mkAnd
    )
import qualified Kore.Internal.TermLike as TermLike
    ( markSimplified
    )
import qualified Kore.Step.Simplification.And as And
import Kore.Step.Simplification.Simplify
import Kore.TopBottom
    ( TopBottom
    )

{-|'simplify' simplifies a 'Not' pattern with an 'OrPattern'
child.

Right now this uses the following:

* not top = bottom
* not bottom = top

-}
simplify
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> Not Sort (OrPattern variable)
    -> simplifier (OrPattern variable)
simplify sideCondition Not { notChild } =
    simplifyEvaluated sideCondition notChild

{-|'simplifyEvaluated' simplifies a 'Not' pattern given its
'OrPattern' child.

See 'simplify' for details.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Not Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of an 'OrPattern' argument. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually
cache information besides the pattern sort, which will make it even more useful
to carry around.

-}
simplifyEvaluated
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> OrPattern variable
    -> simplifier (OrPattern variable)
simplifyEvaluated sideCondition simplified =
    fmap OrPattern.fromPatterns $ gather $ do
        let not' = Not { notChild = simplified, notSort = () }
        andPattern <- scatterAnd (makeEvaluateNot <$> distributeNot not')
        mkMultiAndPattern sideCondition andPattern

simplifyEvaluatedPredicate
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => OrCondition variable
    -> simplifier (OrCondition variable)
simplifyEvaluatedPredicate notChild =
    fmap OrCondition.fromConditions $ gather $ do
        let not' = Not { notChild = notChild, notSort = () }
        andPredicate <-
            scatterAnd (makeEvaluateNotPredicate <$> distributeNot not')
        mkMultiAndPredicate andPredicate

{-|'makeEvaluate' simplifies a 'Not' pattern given its 'Pattern'
child.

See 'simplify' for details.
-}
makeEvaluate
    :: InternalVariable variable
    => Pattern variable
    -> OrPattern variable
makeEvaluate = makeEvaluateNot . Not ()

makeEvaluateNot
    :: InternalVariable variable
    => Not sort (Pattern variable)
    -> OrPattern variable
makeEvaluateNot Not { notChild } =
    MultiOr.merge
        (Pattern.fromTermLike <$> makeTermNot term)
        (MultiOr.singleton $ Pattern.fromConditionSorted
            (termLikeSort term)
            (makeEvaluatePredicate predicate)
        )
  where
    (term, predicate) = Conditional.splitTerm notChild

{- | Given a not's @Internal.Condition@ argument, simplifies the @not@.

Right now there is no actual simplification, this function just creates
a negated @Internal.Condition@.

I.e. if we want to simplify @not (predicate and substitution)@, we may pass
@predicate and substitution@ to this function, which will convert
@predicate and substitution@ into a @Kore.Internal.Predicate@ and will apply
a @not@ on top of that.
-}
makeEvaluatePredicate
    :: InternalVariable variable
    => Condition variable
    -> Condition variable
makeEvaluatePredicate
    Conditional
        { term = ()
        , predicate
        , substitution
        }
  = Conditional
        { term = ()
        , predicate =
            Predicate.markSimplified
            $ makeNotPredicate
            $ makeAndPredicate predicate
            $ Predicate.fromSubstitution substitution
        , substitution = mempty
        }

makeEvaluateNotPredicate
    :: InternalVariable variable
    => Not sort (Condition variable)
    -> OrCondition variable
makeEvaluateNotPredicate Not { notChild = predicate } =
    OrCondition.fromConditions [ makeEvaluatePredicate predicate ]

makeTermNot
    :: InternalVariable variable
    => TermLike variable
    -> MultiOr (TermLike variable)
-- TODO: maybe other simplifications like
-- not ceil = floor not
-- not forall = exists not
makeTermNot (Not_ _ term) = MultiOr.singleton term
makeTermNot (And_ _ term1 term2) =
    MultiOr.merge (makeTermNot term1) (makeTermNot term2)
makeTermNot term
  | isBottom term = MultiOr.singleton mkTop_
  | isTop term    = MultiOr.singleton mkBottom_
  | otherwise     = MultiOr.singleton $ TermLike.markSimplified $ mkNot term

{- | Distribute 'Not' over 'MultiOr' using de Morgan's identity.
 -}
distributeNot
    :: (Ord sort, Ord child, TopBottom child)
    => Not sort (MultiOr child)
    -> MultiAnd (Not sort child)
distributeNot notOr@Not { notChild } =
    MultiAnd.make $ worker <$> Foldable.toList notChild
  where
    worker child = notOr { notChild = child }

{- | Distribute 'MultiAnd' over 'MultiOr'.
 -}
distributeAnd
    :: MultiAnd (MultiOr child)
    -> MultiOr (MultiAnd child)
distributeAnd = sequenceA

{- | Distribute 'MultiAnd' over 'MultiOr' and 'scatter' into 'BranchT'.
 -}
scatterAnd
    :: MultiAnd (MultiOr child)
    -> BranchT m (MultiAnd child)
scatterAnd = scatter . distributeAnd

{- | Conjoin and simplify a 'MultiAnd' of 'Pattern'.
 -}
mkMultiAndPattern
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => SideCondition variable
    -> MultiAnd (Pattern variable)
    -> BranchT simplifier (Pattern variable)
mkMultiAndPattern sideCondition patterns =
    Foldable.foldrM (And.makeEvaluate sideCondition) Pattern.top patterns

{- | Conjoin and simplify a 'MultiAnd' of 'Condition'.
 -}
mkMultiAndPredicate
    :: SimplifierVariable variable
    => MultiAnd (Condition variable)
    -> BranchT simplifier (Condition variable)
mkMultiAndPredicate predicates =
    -- Using Foldable.fold because the Monoid instance of Condition
    -- implements And semantics.
    return $ Foldable.fold predicates
