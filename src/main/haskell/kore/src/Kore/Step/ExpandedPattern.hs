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
    ( CommonExpandedPattern
    , PredicateSubstitution.CommonPredicateSubstitution  -- TODO(virgil): Stop exporting this.
    , ExpandedPattern
    , PredicateSubstitution.PredicateSubstitution (..)  -- TODO(virgil): Stop exporting this.
    , allVariables
    , bottom
    , isBottom
    , isTop
    , mapVariables
    , substitutionToPredicate
    , toMLPattern
    , top
    , fromPurePattern
    , Predicated(..)
    ) where

import           Data.List
                 ( foldl' )
import           Data.Monoid
                 ( (<>) )
import           Data.Reflection
                 ( Given )
import qualified Data.Set as Set

import           Kore.AST.Common
                 ( PureMLPattern, SortedVariable, Variable )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( mapPatternVariables )
import           Kore.ASTUtils.SmartConstructors
                 ( mkAnd, mkBottom, mkTop, mkVar )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern Bottom_, pattern Top_ )
import           Kore.IndexedModule.MetadataTools
                 ( SymbolOrAliasSorts )
import           Kore.Predicate.Predicate
                 ( Predicate, pattern PredicateFalse, pattern PredicateTrue,
                 makeAndPredicate, makeEqualsPredicate, makeFalsePredicate,
                 makeTruePredicate, unwrapPredicate )
import qualified Kore.Predicate.Predicate as Predicate
import qualified Kore.Step.PredicateSubstitution as PredicateSubstitution
                 ( CommonPredicateSubstitution, PredicateSubstitution (..) )
import           Kore.Unification.Data
import           Kore.Variables.Free
                 ( pureAllVariables )

{-|'ExpandedPattern' is a representation of a PureMLPattern that is easier
to use when executing Kore. It consists of an "and" between a term, a
predicate and a substitution
-}

type ExpandedPattern level variable =
    Predicated level variable (PureMLPattern level variable)

data Predicated level variable child = Predicated
    { term :: child
    , predicate :: !(Predicate level variable)
    , substitution :: !(UnificationSubstitution level variable)
    }
    deriving(Eq, Ord, Show)

instance Functor (Predicated level variable) where
    fmap f (Predicated a p s) = Predicated (f a) p s

-- `<*>` does not do normalization for now. 
-- Don't use it until we figure that out.

instance
  ( MetaOrObject level
  , SortedVariable variable
  , Show (variable level)
  , Eq (variable level)
  , Given (SymbolOrAliasSorts level)
  )
  => Applicative (Predicated level variable) where
    pure a = Predicated a makeTruePredicate []
    a <*> b = Predicated
        { term = f x
        , predicate = fst (predicate1 `makeAndPredicate` predicate2)
        , substitution = substitution1 ++ substitution2
        }
        where
            Predicated f predicate1 substitution1 = a
            Predicated x predicate2 substitution2 = b

-- The monad instance for `Predicated` is intentionally omitted.
-- It's not needed for anything, and has bad performance properties.

{-|'CommonExpandedPattern' particularizes ExpandedPattern to Variable.
-}
type CommonExpandedPattern level = ExpandedPattern level Variable

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
        { term = mapPatternVariables variableMapper term
        , predicate = Predicate.mapVariables variableMapper predicate
        , substitution = mapSubstitutionVariables variableMapper substitution
        }

{-|'allVariables' extracts all variables, including the quantified ones,
from an ExpandedPattern.
-}
allVariables
    ::  Ord (variable level)
    => ExpandedPattern level variable
    -> Set.Set (variable level)
allVariables
    Predicated { term, predicate, substitution }
  =
    pureAllVariables term
    <> Predicate.allVariables predicate
    <> allSubstitutionVars substitution
  where
    allSubstitutionVars sub =
        foldl
            (\ x y -> x <> Set.singleton (fst y))
            Set.empty
            sub
        <> foldl
            (\ x y -> x <> pureAllVariables (snd y))
            Set.empty
            sub

{-|'toMLPattern' converts an ExpandedPattern to a PureMLPattern.
-}
toMLPattern
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable variable
        , Eq (variable level)
        , Show (variable level))
    => ExpandedPattern level variable -> PureMLPattern level variable
toMLPattern
    Predicated { term, predicate, substitution }
  =
    simpleAnd
        (simpleAnd term predicate)
        (substitutionToPredicate substitution)
  where
    -- TODO: Most likely I defined this somewhere.
    simpleAnd
        ::  ( MetaOrObject level
            , Given (SymbolOrAliasSorts level)
            , SortedVariable variable
            , Show (variable level))
        => PureMLPattern level variable
        -> Predicate level variable
        -> PureMLPattern level variable
    simpleAnd (Top_ _)      predicate'     = unwrapPredicate predicate'
    simpleAnd patt          PredicateTrue  = patt
    simpleAnd b@(Bottom_ _) _              = b
    simpleAnd _             PredicateFalse = mkBottom
    simpleAnd pattern1      predicate'     =
        mkAnd pattern1 (unwrapPredicate predicate')

{-|'substitutionToPredicate' transforms a substitution in a predicate.
-}
substitutionToPredicate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable variable
        , Eq (variable level)
        , Show (variable level))
    => [(variable level, PureMLPattern level variable)]
    -> Predicate level variable
substitutionToPredicate =
    foldl'
        (\predicate subst ->
            fst $
                makeAndPredicate
                    predicate (singleSubstitutionToPredicate subst)
        )
        makeTruePredicate

singleSubstitutionToPredicate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable variable
        , Show (variable level))
    => (variable level, PureMLPattern level variable)
    -> Predicate level variable
singleSubstitutionToPredicate (var, patt) =
    makeEqualsPredicate (mkVar var) patt


{-|'bottom' is an expanded pattern that has a bottom condition and that
should become Bottom when transformed to a ML pattern.
-}
bottom :: MetaOrObject level => ExpandedPattern level variable
bottom =
    Predicated
        { term      = mkBottom
        , predicate = makeFalsePredicate
        , substitution = []
        }

{-|'top' is an expanded pattern that has a top condition and that
should become Top when transformed to a ML pattern.
-}
top :: MetaOrObject level => ExpandedPattern level variable
top =
    Predicated
        { term      = mkTop
        , predicate = makeTruePredicate
        , substitution = []
        }

{-| 'isTop' checks whether an ExpandedPattern is equivalent to a top Pattern.
-}
isTop :: ExpandedPattern level variable -> Bool
isTop
    Predicated
        { term = Top_ _, predicate = PredicateTrue, substitution = [] }
  = True
isTop _ = False

{-| 'isBottom' checks whether an ExpandedPattern is equivalent to a bottom
Pattern.
-}
isBottom :: ExpandedPattern level variable -> Bool
isBottom
    Predicated {term = Bottom_ _}
  = True
isBottom
    Predicated {predicate = PredicateFalse}
  = True
isBottom _ = False

{- | Construct an 'ExpandedPattern' from a 'PureMLPattern'.

  The resulting @ExpandedPattern@ has a true predicate and an empty substitution.

  See also: 'makeTruePredicate'

 -}
fromPurePattern
    :: MetaOrObject level
    => PureMLPattern level variable
    -> ExpandedPattern level variable
fromPurePattern term =
    case term of
        Bottom_ _ -> bottom
        _ ->
            Predicated
                { term
                , predicate = makeTruePredicate
                , substitution = []
                }
