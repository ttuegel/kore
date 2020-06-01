{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    , toList
    , toSet
    , toNames
    , nullFreeVariables
    , freeVariable
    , isFreeVariable
    , bindVariable
    , bindVariables
    , mapFreeVariables
    , traverseFreeVariables
    , getFreeElementVariables
    , HasFreeVariables (..)
    ) where

import Prelude.Kore

import Control.DeepSeq
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Functor.Adjunction
    ( leftAdjunct
    , rightAdjunct
    , splitL
    )
import Data.Functor.Const
import Data.Functor.Rep
    ( tabulate
    )
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Syntax.Variable

newtype FreeVariables variable =
    FreeVariables
    { getFreeVariables :: AdjSomeVariableName (Map variable Sort) }
    deriving GHC.Generic
    deriving (Eq, Ord, Show)
    deriving (Semigroup, Monoid)

instance SOP.Generic (FreeVariables variable)

instance SOP.HasDatatypeInfo (FreeVariables variable)

instance Debug variable => Debug (FreeVariables variable)

instance (Debug variable, Diff variable) => Diff (FreeVariables variable)

instance NFData variable => NFData (FreeVariables variable)

instance Hashable variable => Hashable (FreeVariables variable) where
    hashWithSalt salt = hashWithSalt salt . fmap Map.toList . getFreeVariables
    {-# INLINE hashWithSalt #-}

instance
    Synthetic (FreeVariables variable) (Const (SomeVariable variable))
  where
    synthetic (Const var) = freeVariable var
    {-# INLINE synthetic #-}

instance From (FreeVariables variable) [SomeVariable variable] where
    from = toList
    {-# INLINE from #-}

instance
    Ord variable
    => From (FreeVariables variable) (Set (SomeVariable variable))
  where
    from = toSet
    {-# INLINE from #-}

instance
    Ord variable
    => From (FreeVariables variable) (Set (SomeVariableName variable))
  where
    from = toNames
    {-# INLINE from #-}

toList :: FreeVariables variable -> [SomeVariable variable]
toList = map (uncurry Variable) . go . fmap Map.toAscList . getFreeVariables
  where
    go adj = Foldable.fold (leftAdjunct classifyKeys () <*> adj)
    classifyKeys which = (map . Bifunctor.first) (which $>)
{-# INLINE toList #-}

fromList
    :: Ord variable
    => [SomeVariable variable]
    -> FreeVariables variable
fromList = foldMap freeVariable
{-# INLINE fromList #-}

toSet
    :: Ord variable
    => FreeVariables variable
    -> Set (SomeVariable variable)
toSet = Set.fromList . toList
{-# INLINE toSet #-}

toNames
    :: Ord variable
    => FreeVariables variable
    -> Set (SomeVariableName variable)
toNames = go . fmap Map.keysSet . getFreeVariables
  where
    go adj = Foldable.fold (leftAdjunct classifyKeys () <*> adj)
    classifyKeys which = Set.map (which $>)
{-# INLINE toNames #-}

nullFreeVariables :: FreeVariables variable -> Bool
nullFreeVariables = all Map.null . getFreeVariables
{-# INLINE nullFreeVariables #-}

bindVariable
    :: Ord variable
    => SomeVariable variable
    -> FreeVariables variable
    -> FreeVariables variable
bindVariable Variable { variableName } (FreeVariables adj) =
    (<*>)
        (tabulate $ \ix' -> if ix == ix' then Map.delete variable else id)
        adj
    & FreeVariables
  where
    (variable, ix) = splitL variableName
{-# INLINE bindVariable #-}

bindVariables
    :: Ord variable
    => Foldable f
    => f (SomeVariable variable)
    -> FreeVariables variable
    -> FreeVariables variable
bindVariables bound free =
    Foldable.foldl' (flip bindVariable) free bound
{-# INLINE bindVariables #-}

isFreeVariable
    :: Ord variable
    => SomeVariableName variable
    -> FreeVariables variable
    -> Bool
isFreeVariable someVariableName (FreeVariables adj) =
    rightAdjunct (\variable -> Map.member variable <$> adj) someVariableName
{-# INLINE isFreeVariable #-}

freeVariable :: SomeVariable variable -> FreeVariables variable
freeVariable Variable { variableName, variableSort } =
    tabulate
        (\ix' ->
                if ix == ix'
                then Map.singleton variable variableSort
                else Map.empty
        )
    & FreeVariables
  where
    (variable, ix) = splitL variableName
{-# INLINE freeVariable #-}

mapFreeVariables
    :: Ord variable2
    => AdjSomeVariableName (variable1 -> variable2)
    -> FreeVariables variable1 -> FreeVariables variable2
mapFreeVariables adj = fromList . map (mapSomeVariable adj) . toList
{-# INLINE mapFreeVariables #-}

traverseFreeVariables
    :: Applicative f
    => Ord variable2
    => AdjSomeVariableName (variable1 -> f variable2)
    -> FreeVariables variable1 -> f (FreeVariables variable2)
traverseFreeVariables adj =
    fmap fromList . traverse (traverseSomeVariable adj) . toList
{-# INLINE traverseFreeVariables #-}

{- | Extracts the list of free element variables
-}
getFreeElementVariables :: FreeVariables variable -> [ElementVariable variable]
getFreeElementVariables = mapMaybe retractElementVariable . toList

-- TODO (thomas.tuegel): Use an associated type family with HasFreeVariables to
-- fix type inference.

-- | Class for extracting the free variables of a pattern, term, rule, ...
class HasFreeVariables pat variable where
    freeVariables :: pat -> FreeVariables variable

instance Ord variable => HasFreeVariables () variable where
    freeVariables = const mempty
