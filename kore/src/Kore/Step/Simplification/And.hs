{-|
Module      : Kore.Step.Simplification.And
Description : Tools for And pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.And
    ( makeEvaluate
    , simplify
    , simplifyEvaluated
    , simplifyEvaluatedMultiple
    , And (..)
    , termAnd
    ) where

import Prelude.Kore

import Control.Error
    ( runMaybeT
    )
import Control.Monad.State.Strict
    ( StateT
    , evalStateT
    )
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor
    ( bimap
    )
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Foldable as Recursive
import Data.HashMap.Strict
    ( HashMap
    )
import qualified Data.HashMap.Strict as HashMap
import Data.List
    ( foldl1'
    , sortBy
    )
import Data.Ord
    ( comparing
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Data.Traversable
    ( for
    )
import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import qualified Kore.Internal.MultiAnd as MultiAnd

import Changed
import Kore.Attribute.Synthetic
    ( synthesize
    )
import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , makePredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
    ( And (..)
    , pattern And_
    , pattern Exists_
    , pattern Forall_
    , pattern Mu_
    , pattern Not_
    , pattern Nu_
    , Sort
    , TermLike
    , Variable (..)
    , mkAnd
    , mkBottom
    , mkBottom_
    , mkNot
    , mkTop
    , termLikeSort
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Step.Simplification.AndTerms
    ( maybeTermAnd
    )
import Kore.Step.Simplification.NotSimplifier
import Kore.Step.Simplification.Simplify
import qualified Kore.Step.Substitution as Substitution
import Kore.Unification.UnifierT
    ( UnifierT (..)
    , runUnifierT
    )
import Kore.Unparser
    ( unparse
    )
import Logic
import qualified Pretty

{-|'simplify' simplifies an 'And' of 'OrPattern'.

To do that, it first distributes the terms, making it an Or of And patterns,
each And having 'Pattern's as children, then it simplifies each of
those.

Since an Pattern is of the form term /\ predicate /\ substitution,
making an and between two Patterns roughly means and-ing each of their
components separately.

This means that a bottom component anywhere makes the result bottom, while
top can always be ignored.

When we 'and' two terms:
by Proposition 5.24 from (1),
    x and functional-pattern = functional-pattern and [x=phi]
We can generalize that to:
    x and function-pattern
        = function-pattern and ceil(function-pattern) and [x=phi]
        but note that ceil(function-pattern) is not actually needed.
We can still generalize that to:
    function-like-pattern1 and function-like-pattern2
        = function-pattern1 and function-pattern1 == function-pattern2
Also, we have
    constructor1(s1, ..., sk) and constructor2(t1, ..., tk):
        if constructor1 != constructor2 then this is bottom
        else it is
            constructor1(s1 and t1, ..., sk and tk)
    * constructor - 'inj' (sort injection) pairs become bottom
    * injection-injection pairs with the same injection work the same as
      identical constructors
    domain-value1 and domain-value1, where both are string-based:
        domain-value1 if they are equal
        bottom otherwise
    the same for two string literals and two chars
-}
simplify
    :: InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> And Sort (OrPattern variable)
    -> simplifier (OrPattern variable)
simplify notSimplifier sideCondition And { andFirst = first, andSecond = second } =
    simplifyEvaluated notSimplifier sideCondition first second

{-| simplifies an And given its two 'OrPattern' children.

See 'simplify' for details.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (And Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually
cache information besides the pattern sort, which will make it even more useful
to carry around.

-}
simplifyEvaluated
    :: InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> OrPattern variable
    -> OrPattern variable
    -> simplifier (OrPattern variable)
simplifyEvaluated notSimplifier sideCondition first second =
    simplifyEvaluatedMultiple notSimplifier sideCondition
        (MultiAnd.make [first, second])

simplifyEvaluatedMultiple
    :: InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> MultiAnd (OrPattern variable)
    -> simplifier (OrPattern variable)
simplifyEvaluatedMultiple notSimplifier sideCondition orPatterns =
    OrPattern.observeAllT $ do
        patterns <- traverse scatter orPatterns
        makeEvaluateMulti notSimplifier sideCondition patterns

{- | 'makeEvaluateMulti' simplifies a 'MultiAnd' of 'Pattern's.

See the comment for 'simplify' to find more details.

-}
makeEvaluateMulti
    :: forall variable simplifier
    .  HasCallStack
    => InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> MultiAnd (Pattern variable)
    -> LogicT simplifier (Pattern variable)
makeEvaluateMulti notSimplifier sideCondition patterns
  | isBottom patterns = empty
  | Pattern.isTop patterns = return Pattern.top
  | otherwise = makeEvaluateNonBool notSimplifier sideCondition patterns

{- | 'makeEvaluate' simplifies an 'And' of 'Pattern's.

See the comment for 'simplify' to find more details.

-}
makeEvaluate
    :: forall variable simplifier
    .  HasCallStack
    => InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> Pattern variable
    -> Pattern variable
    -> LogicT simplifier (Pattern variable)
makeEvaluate notSimplifier sideCondition first second =
    makeEvaluateMulti notSimplifier sideCondition
        (MultiAnd.make [first, second])

makeEvaluateNonBool
    :: forall variable simplifier
    .  HasCallStack
    => InternalVariable variable
    => MonadSimplify simplifier
    => NotSimplifier (UnifierT simplifier)
    -> SideCondition variable
    -> MultiAnd (Pattern variable)
    -> LogicT simplifier (Pattern variable)
makeEvaluateNonBool notSimplifier sideCondition patterns = do
    let unify pattern1 term2 = do
            let (term1, condition1) = Pattern.splitTerm pattern1
            unified <- termAnd notSimplifier term1 term2
            pure (Pattern.andCondition unified condition1)
    unified <- Foldable.foldlM unify Pattern.top (term <$> patterns)
    let substitutions =
            Pattern.substitution unified
            <> foldMap Pattern.substitution patterns
    normalized <-
        from @_ @(Condition _) substitutions
        & Substitution.normalize sideCondition
    let substitution = Pattern.substitution normalized
        predicates :: Changed (MultiAnd (Predicate variable))
        predicates =
            mconcat
                [ MultiAnd.fromPredicate (predicate unified)
                , MultiAnd.fromPredicate (predicate normalized)
                , foldMap (from @(Predicate _) . predicate) patterns
                ]
            & promoteSubTermsToTop
        term =
            applyAndIdempotenceAndFindContradictions
                (Conditional.term unified)
    case predicates of
        Unchanged unchanged ->
            Pattern.withCondition term (from substitution <> from predicate)
            & return
          where
            predicate =
                MultiAnd.toPredicate unchanged
                & Predicate.setSimplified simplified
            simplified = foldMap Predicate.simplifiedAttribute unchanged
        Changed changed ->
            Pattern.withCondition term (from substitution <> from predicate)
            & simplifyCondition sideCondition
          where
            predicate = MultiAnd.toPredicate changed

promoteSubTermsToTop
    :: forall variable
    .  InternalVariable variable
    => MultiAnd (Predicate variable)
    -> Changed (MultiAnd (Predicate variable))
promoteSubTermsToTop (Foldable.toList -> andPredicates) =
    fmap MultiAnd.make
    $ flip evalStateT HashMap.empty
    $ for (sortBySize andPredicates)
    $ \predicate' -> do
        let original = Predicate.unwrapPredicate predicate'
        result <- replaceWithTopNormalized original
        insertAssumption result
        return result
  where
    -- Sorting by size ensures that every clause is considered before any clause
    -- which could contain it, because the containing clause is necessarily
    -- larger.
    sortBySize :: [Predicate variable] -> [Predicate variable]
    sortBySize = sortBy (comparing (size . from))

    size :: TermLike variable -> Int
    size =
        Recursive.fold $ \(_ :< termLikeF) ->
            case termLikeF of
                TermLike.EvaluatedF evaluated -> TermLike.getEvaluated evaluated
                TermLike.DefinedF defined -> TermLike.getDefined defined
                _ -> 1 + Foldable.sum termLikeF

    insertAssumption
        :: Predicate variable
        -> StateT (HashMap (TermLike variable) (TermLike variable)) Changed ()
    insertAssumption predicate1 =
        State.modify' insert
      where
        insert =
            case termLike of
                -- Infer that the predicate is \bottom.
                Not_ _ notChild -> HashMap.insert notChild (mkBottom sort)
                -- Infer that the predicate is \top.
                _               -> HashMap.insert termLike (mkTop    sort)
        termLike = Predicate.unwrapPredicate predicate1
        sort = termLikeSort termLike

    replaceWithTopNormalized
        ::  TermLike variable
        ->  StateT (HashMap (TermLike variable) (TermLike variable)) Changed
                (Predicate variable)
    replaceWithTopNormalized replaceIn = do
        replacements <- State.get
        lift $ fmap
            (unsafeMakePredicate replacements replaceIn)
            (replaceWithTop replacements replaceIn)

    unsafeMakePredicate replacements original result =
        case makePredicate result of
            -- TODO (ttuegel): https://github.com/kframework/kore/issues/1442
            -- should make it impossible to have an error here.
            Left err ->
                (error . show . Pretty.vsep)
                [ "Replacing"
                , (Pretty.indent 4 . Pretty.vsep) (unparse <$> HashMap.keys replacements)
                , "in"
                , Pretty.indent 4 (unparse original)
                , Pretty.indent 4 (Pretty.pretty err)
                ]
            Right p -> p

    replaceWithTop
        :: HashMap (TermLike variable) (TermLike variable)
        -> TermLike variable
        -> Changed (TermLike variable)
    replaceWithTop replacements original
      | Just result <- HashMap.lookup original replacements = Changed result

      | HashMap.null replacements' = Unchanged original

      | otherwise =
        traverse (replaceWithTop replacements') replaceIn
        & getChanged
        -- The next line ensures that if the result is Unchanged, any allocation
        -- performed while computing that result is collected.
        & maybe (Unchanged original) (Changed . synthesize)

      where
        _ :< replaceIn = Recursive.project original

        replacements'
          | Exists_ _ var _ <- original = restrictReplacements (inject var)
          | Forall_ _ var _ <- original = restrictReplacements (inject var)
          | Mu_       var _ <- original = restrictReplacements (inject var)
          | Nu_       var _ <- original = restrictReplacements (inject var)
          | otherwise = replacements

        restrictReplacements Variable { variableName } =
            HashMap.filterWithKey
                (\termLike _ -> wouldNotCapture termLike)
                replacements
          where
            wouldNotCapture = not . TermLike.hasFreeVariable variableName

applyAndIdempotenceAndFindContradictions
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
applyAndIdempotenceAndFindContradictions patt =
    if noContradictions
        then foldl1' mkAndSimplified . Set.toList $ Set.union terms negatedTerms
        else mkBottom_

  where
    (terms, negatedTerms) = splitIntoTermsAndNegations patt
    noContradictions = Set.disjoint (Set.map mkNot terms) negatedTerms
    mkAndSimplified a b =
        TermLike.setSimplified
            (TermLike.simplifiedAttribute a <> TermLike.simplifiedAttribute b)
            (mkAnd a b)

splitIntoTermsAndNegations
    :: forall variable
    .  Ord variable
    => TermLike variable
    -> (Set (TermLike variable), Set (TermLike variable))
splitIntoTermsAndNegations =
    bimap Set.fromList Set.fromList
        . partitionWith termOrNegation
        . children
  where
    children :: TermLike variable -> [TermLike variable]
    children (And_ _ p1 p2) = children p1 ++ children p2
    children p = [p]

    -- Left is for regular terms, Right is negated terms
    termOrNegation
        :: TermLike variable
        -> Either (TermLike variable) (TermLike variable)
    termOrNegation t@(Not_ _ _) = Right t
    termOrNegation t            = Left t

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = partitionEithers . fmap f

{- | Simplify the conjunction (@\\and@) of two terms.

The comment for 'simplify' describes all the special cases handled by this.

-}
termAnd
    :: forall variable simplifier
    .  InternalVariable variable
    => MonadSimplify simplifier
    => HasCallStack
    => NotSimplifier (UnifierT simplifier)
    -> TermLike variable
    -> TermLike variable
    -> LogicT simplifier (Pattern variable)
termAnd notSimplifier p1 p2 =
    Logic.scatter
    =<< (lift . runUnifierT notSimplifier) (termAndWorker p1 p2)
  where
    termAndWorker
        :: TermLike variable
        -> TermLike variable
        -> UnifierT simplifier (Pattern variable)
    termAndWorker first second = do
        let maybeTermAnd' = maybeTermAnd notSimplifier termAndWorker first second
        patt <- runMaybeT maybeTermAnd'
        return $ fromMaybe andPattern patt
      where
        andPattern = Pattern.fromTermLike (mkAnd first second)
