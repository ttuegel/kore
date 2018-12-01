{-|
Module      : Kore.Step.ExpandedPattern
Description : Data structures and functions for manipulating
              ExpandedPatterns, i.e. a representation of patterns
              optimized for the stepper.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.ExpandedPattern
    ( Predicated (..)
    , ExpandedPattern
    , CommonExpandedPattern
    , PredicateSubstitution
    , CommonPredicateSubstitution
    , allVariables
    , erasePredicatedTerm
    , bottom
    , isBottom
    , isTop
    , mapVariables
    , substitutionToPredicate
    , toMLPattern
    , top
    , topPredicate
    , bottomPredicate
    , fromPurePattern
    , toPredicate
    , freeVariables
    ) where

import           Control.Comonad
import           Control.DeepSeq
                 ( NFData )
import           Data.Functor
                 ( ($>) )
import qualified Data.Functor.Foldable as Recursive
import           Data.Monoid
                 ( (<>) )
import qualified Data.Set as Set
import           GHC.Generics
                 ( Generic )

import           Kore.AST.MetaOrObject
import           Kore.AST.Pure hiding
                 ( fromPurePattern, mapVariables )
import qualified Kore.AST.Pure as Pure
import           Kore.ASTUtils.SmartConstructors
                 ( mkAnd, mkBottom, mkTop )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern Bottom_, pattern Top_ )
import           Kore.Predicate.Predicate
                 ( Predicate, pattern PredicateFalse, pattern PredicateTrue,
                 makeAndPredicate, makeFalsePredicate, makeTruePredicate,
                 substitutionToPredicate, unwrapPredicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Pattern
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser
import           Kore.Variables.Free
                 ( pureAllVariables )

{- | @Predicated@ represents a value conditioned on a predicate.

@Predicated level variable child@ represents a @child@ conditioned on a
@predicate@ and @substitution@ (which is a specialized form of predicate).

The 'Applicative' instance conjoins the predicates and substitutions so that the
result is conditioned on the combined predicates of the inputs. The combined
predicate and substitution are /not/ normalized.

There is intentionally no 'Monad' instance; such an instance would have
quadratic complexity.

 -}
data Predicated level variable child = Predicated
    { term :: child
    , predicate :: !(Sort level -> Predicate level variable)
    , substitution :: !(Substitution level variable)
    }
    deriving (Foldable, Functor, Generic, Traversable)

instance
    (NFData child, NFData (variable level)) =>
    NFData (Predicated level variable child)

instance
    ( MetaOrObject level
    , Eq (variable level)
    , Unparse (variable level)
    ) =>
    Applicative (Predicated level variable)
  where
    pure a = Predicated a makeTruePredicate mempty
    a <*> b =
        Predicated
            { term = f x
            , predicate =
                \sort -> makeAndPredicate (predicate1 sort) (predicate2 sort)
            , substitution = substitution1 <> substitution2
            }
      where
        Predicated f predicate1 substitution1 = a
        Predicated x predicate2 substitution2 = b

{- | The conjunction of a pattern, predicate, and substitution.

The form of @ExpandedPattern@ is intended to be convenient for Kore execution.

 -}
type ExpandedPattern level variable =
    Predicated level variable (StepPattern level variable)

{- | 'CommonExpandedPattern' particularizes 'ExpandedPattern' to 'Variable'.
-}
type CommonExpandedPattern level = ExpandedPattern level Variable

-- | A predicate and substitution without an accompanying term.
type PredicateSubstitution level variable =
    Predicated level variable (Sort level)

-- | A 'PredicateSubstitution' of the 'Variable' type.
type CommonPredicateSubstitution level = PredicateSubstitution level Variable

{-|'mapVariables' transforms all variables, including the quantified ones,
in an ExpandedPattern.
-}
mapVariables
    :: (variableFrom level -> variableTo level)
    -> ExpandedPattern level variableFrom
    -> ExpandedPattern level variableTo
mapVariables
    variableMapper
    Predicated { term, predicate, substitution }
  =
    Predicated
        { term = Pure.mapVariables variableMapper term
        , predicate =
            \sort -> Predicate.mapVariables variableMapper (predicate sort)
        , substitution =
            Substitution.mapVariables variableMapper substitution
        }

{-|'allVariables' extracts all variables, including the quantified ones,
from an ExpandedPattern.
-}
allVariables
    :: Ord (variable level)
    => ExpandedPattern level variable
    -> Set.Set (variable level)
allVariables
    Predicated { term, predicate, substitution }
  =
    pureAllVariables term
    <> Predicate.allVariables (predicate patternSort)
    <> allSubstitutionVars (Substitution.unwrap substitution)
  where
    Valid { patternSort } = extract term
    allSubstitutionVars sub =
        foldl
            (\ x y -> x <> Set.singleton (fst y))
            Set.empty
            sub
        <> foldl
            (\ x y -> x <> pureAllVariables (snd y))
            Set.empty
            sub

-- | Erase the @Predicated@ 'term' to yield a 'PredicateSubstitution'.
erasePredicatedTerm
    :: ExpandedPattern level variable
    -> PredicateSubstitution level variable
erasePredicatedTerm = (<$>) (patternSort . extract)

{-|'toMLPattern' converts an ExpandedPattern to a StepPattern.
-}
toMLPattern
    ::  forall level variable.
        ( MetaOrObject level
        , Eq (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level variable -> StepPattern level variable
toMLPattern
    Predicated { term, predicate, substitution }
  =
    simpleAnd
        (simpleAnd term (predicate patternSort))
        (substitutionToPredicate patternSort substitution)
  where
    Valid { patternSort } = extract term
    -- TODO: Most likely I defined this somewhere.
    simpleAnd
        ::  ( MetaOrObject level
            , Unparse (variable level)
            )
        => StepPattern level variable
        -> Predicate level variable
        -> StepPattern level variable
    simpleAnd stepPattern =
        \case
            PredicateTrue -> stepPattern
            PredicateFalse -> mkBottom patternSort
            predicate' ->
                case pat of
                    TopPattern _ -> unwrapped
                    BottomPattern _ -> stepPattern
                    _ -> mkAnd stepPattern unwrapped
              where
                ann :< pat = Recursive.project stepPattern
                unwrapped = unwrapPredicate predicate'

{-|'bottom' is an expanded pattern that has a bottom condition and that
should become Bottom when transformed to a ML pattern.
-}
bottom :: MetaOrObject level => Sort level -> ExpandedPattern level variable
bottom sort =
    Predicated
        { term      = mkBottom sort
        , predicate = makeFalsePredicate
        , substitution = mempty
        }

{-|'top' is an expanded pattern that has a top condition and that
should become Top when transformed to a ML pattern.
-}
top :: MetaOrObject level => Sort level -> ExpandedPattern level variable
top sort =
    Predicated
        { term      = mkTop sort
        , predicate = makeTruePredicate
        , substitution = mempty
        }

{-| 'isTop' checks whether an ExpandedPattern is equivalent to a top Pattern.
-}
isTop :: ExpandedPattern level variable -> Bool
isTop Predicated { term, predicate, substitution }
  | Valid { patternSort } :< TopPattern _ <- Recursive.project term
  , PredicateTrue <- predicate patternSort
  = Substitution.null substitution
isTop _ = False

{-| 'isBottom' checks whether an ExpandedPattern is equivalent to a bottom
Pattern.
-}
isBottom :: ExpandedPattern level variable -> Bool
isBottom Predicated { term }
  | _ :< BottomPattern _ <- Recursive.project term
  = True
isBottom Predicated { term, predicate }
  | PredicateFalse <- predicate patternSort
  = True
  where
    Valid { patternSort } :< _ = Recursive.project term
isBottom _ = False

{- | Construct an 'ExpandedPattern' from a 'StepPattern'.

  The resulting @ExpandedPattern@ has a true predicate and an empty
  substitution.

  See also: 'makeTruePredicate', 'pure'

 -}
fromPurePattern
    :: MetaOrObject level
    => StepPattern level variable
    -> ExpandedPattern level variable
fromPurePattern term =
    case pat of
        BottomPattern _ -> bottom patternSort
        _ ->
            Predicated
                { term
                , predicate = makeTruePredicate
                , substitution = mempty
                }
  where
    Valid { patternSort } :< pat = Recursive.project term

topPredicate
    :: MetaOrObject level
    => Sort level
    -> PredicateSubstitution level variable
topPredicate term =
    Predicated
        { term
        , predicate = makeTruePredicate
        , substitution = mempty
        }

bottomPredicate
    :: MetaOrObject level
    => Sort level
    -> PredicateSubstitution level variable
bottomPredicate term =
    Predicated
        { term
        , predicate = makeFalsePredicate
        , substitution = mempty
        }

{- | Transform a predicate and substitution into a predicate only.

    See also: 'substitutionToPredicate'.

-}
toPredicate
    :: ( MetaOrObject level
       , Eq (variable level)
       , Unparse (variable level)
       )
    => PredicateSubstitution level variable
    -> Predicate level variable
toPredicate Predicated { term = sort, predicate, substitution } =
    makeAndPredicate
        (predicate sort)
        (substitutionToPredicate sort substitution)

{- | Extract the set of free variables from a predicate and substitution.

    See also: 'Predicate.freeVariables'.
-}

freeVariables
    :: ( MetaOrObject level
       , Ord (variable level)
       , Unparse (variable level)
       )
    => PredicateSubstitution level variable
    -> Set.Set (variable level)
freeVariables = Predicate.freeVariables . toPredicate
