{-|
Module      : Data.Kore.Substitution.List
Description : Defines an instance of 'SubstitutionClass' using a list of
              variable |-> pattern pairs.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Data.Kore.Substitution.List ( Substitution
                                   , SubstitutionClass(..)
                                   , MapClass(..)
                                   , fromList
                                   , toList
                                   ) where

import           Data.Fix
import           Data.List                         (nubBy)

import           Data.Kore.AST.Common
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.Datastructures.MapClass
import           Data.Kore.Substitution.Class
import           Data.Kore.Variables.Free

-- |A very simple substitution represented as a list of pairs
newtype Substitution var pat = Substitution { getSubstitution :: [(var, pat)] }

instance
    ( UnifiedPatternInterface pat
    , Functor (pat var)
    , Ord (var Object)
    , Ord (var Meta)
    ) => SubstitutionClass Substitution (Unified var) (Fix (pat var))
  where
    substitutionTermsFreeVars = foldMap (freeVariables . snd) . getSubstitution

instance Eq v => MapClass Substitution v t where
    isEmpty = null . getSubstitution
    empty = Substitution []
    lookup v (Substitution l) = Prelude.lookup v l
    delete v = Substitution . filter ((v /=) . fst) . getSubstitution
    insert v t  =
        Substitution . ((v,t) :) . filter ((v /=) . fst) . getSubstitution

fromList :: Eq k => [(k,v)] -> Substitution k v
fromList = Substitution . nubBy (\x y -> fst x == fst y)

toList :: Substitution k v -> [(k,v)]
toList = getSubstitution
