{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    , toList
    , toSet
    , toAvoiding
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
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import Data.Functor.Const
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import qualified Data.Map as Map
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Syntax.ElementVariable
import Kore.Syntax.SetVariable
import Kore.Syntax.Variable
    ( NamedVariable (..)
    , SomeVariableName
    , SortedVariable
    , sortedVariableSort
    )
import Kore.Variables.Fresh
    ( Avoiding
    )
import Kore.Variables.UnifiedVariable

data FreeVariables variable where
    FreeVariables
        :: { getFreeVariables :: Map (UnifiedVariable variable) Sort }
        -> FreeVariables variable
    deriving GHC.Generic
    deriving (Eq, Ord, Show)

instance Ord variable => Semigroup (FreeVariables variable) where
    (<>) a b = FreeVariables $ on (<>) getFreeVariables a b

instance Ord variable => Monoid (FreeVariables variable) where
    mempty = FreeVariables mempty

instance SOP.Generic (FreeVariables variable)

instance SOP.HasDatatypeInfo (FreeVariables variable)

instance Debug variable => Debug (FreeVariables variable)

instance (Debug variable, Diff variable) => Diff (FreeVariables variable)

instance NFData variable => NFData (FreeVariables variable)

instance Hashable variable => Hashable (FreeVariables variable) where
    hashWithSalt salt = hashWithSalt salt . Map.toAscList . getFreeVariables
    {-# INLINE hashWithSalt #-}

instance
    SortedVariable variable
    => Synthetic (FreeVariables variable) (Const (UnifiedVariable variable))
  where
    synthetic (Const var) = freeVariable var
    {-# INLINE synthetic #-}

instance From (FreeVariables variable) [UnifiedVariable variable] where
    from = toList
    {-# INLINE from #-}

instance From (FreeVariables variable) (Set (UnifiedVariable variable)) where
    from = toSet
    {-# INLINE from #-}

toList :: FreeVariables variable -> [UnifiedVariable variable]
toList = Map.keys . getFreeVariables
{-# INLINE toList #-}

toSet :: FreeVariables variable -> Set (UnifiedVariable variable)
toSet = Map.keysSet . getFreeVariables
{-# INLINE toSet #-}

toAvoiding
    :: NamedVariable variable
    => Ord (VariableNameOf variable)
    => FreeVariables variable
    -> Avoiding (SomeVariableName (VariableNameOf variable))
toAvoiding =
    from @(Set _) @(Avoiding _)
    . Set.map (Lens.view lensVariableName)
    . toSet
{-# INLINE toAvoiding #-}

nullFreeVariables :: FreeVariables variable -> Bool
nullFreeVariables = Map.null . getFreeVariables
{-# INLINE nullFreeVariables #-}

bindVariable
    :: Ord variable
    => UnifiedVariable variable
    -> FreeVariables variable
    -> FreeVariables variable
bindVariable variable = Lens.over _Unwrapped (Map.delete variable)
{-# INLINE bindVariable #-}

bindVariables
    :: Ord variable
    => Foldable f
    => f (UnifiedVariable variable)
    -> FreeVariables variable
    -> FreeVariables variable
bindVariables bound free =
    Foldable.foldl' (flip bindVariable) free bound
{-# INLINE bindVariables #-}

isFreeVariable
    :: Ord variable
    => UnifiedVariable variable -> FreeVariables variable -> Bool
isFreeVariable variable = Map.member variable . getFreeVariables
{-# INLINE isFreeVariable #-}

freeVariable
    :: SortedVariable variable
    => UnifiedVariable variable
    -> FreeVariables variable
freeVariable variable =
    FreeVariables (Map.singleton variable sort)
  where
    sort = sortedVariableSort variable
{-# INLINE freeVariable #-}

mapFreeVariables
    :: (Ord variable2, SortedVariable variable2)
    => (ElementVariable variable1 -> ElementVariable variable2)
    -> (SetVariable variable1 -> SetVariable variable2)
    -> FreeVariables variable1 -> FreeVariables variable2
mapFreeVariables mapElemVar mapSetVar =
    Set.map (mapUnifiedVariable mapElemVar mapSetVar)
    & viaSet
    & Lens.over _Unwrapped
  where
    viaSet f = Map.fromSet sortedVariableSort . f . Map.keysSet
{-# INLINE mapFreeVariables #-}

traverseFreeVariables
    :: Applicative f
    => (Ord variable2, SortedVariable variable2)
    => (ElementVariable variable1 -> f (ElementVariable variable2))
    -> (SetVariable variable1 -> f (SetVariable variable2))
    -> FreeVariables variable1 -> f (FreeVariables variable2)
traverseFreeVariables traverseElemVar traverseSetVar (FreeVariables freeVars) =
    FreeVariables . Map.fromSet sortedVariableSort . Set.fromList
    <$> Traversable.traverse traversal (Map.keys freeVars)
  where
    traversal = traverseUnifiedVariable traverseElemVar traverseSetVar
{-# INLINE traverseFreeVariables #-}

{- | Extracts the list of free element variables
-}
getFreeElementVariables :: FreeVariables variable -> [ElementVariable variable]
getFreeElementVariables =
    mapMaybe extractElementVariable . Map.keys . getFreeVariables

-- TODO (thomas.tuegel): Use an associated type family with HasFreeVariables to
-- fix type inference.

-- | Class for extracting the free variables of a pattern, term, rule, ...
class HasFreeVariables pat variable where
    freeVariables :: pat -> FreeVariables variable

instance Ord variable => HasFreeVariables () variable where
    freeVariables = const mempty
