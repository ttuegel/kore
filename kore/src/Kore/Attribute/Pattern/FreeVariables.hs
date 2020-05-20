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
    -- * Re-exports
    , NamedVariable
    ) where

import Prelude.Kore

import Control.DeepSeq
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import Data.Functor.Const
import qualified Data.Map as Map
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Syntax.ElementVariable
import Kore.Syntax.SetVariable
import Kore.Syntax.Variable
    ( NamedVariable (..)
    , SomeVariableName
    , sortedVariableSort
    )
import Kore.Variables.Fresh
    ( Avoiding
    )
import Kore.Variables.UnifiedVariable

data FreeVariables variable where
    FreeVariables
        :: NamedVariable variable
        => { getFreeVariables :: Map (UnifiedVariable variable) Sort }
        -> FreeVariables variable

instance Eq variable => Eq (FreeVariables variable) where
    (==) = on (==) toList

instance Ord variable => Ord (FreeVariables variable) where
    compare = on compare toList

instance Show (FreeVariables variable) where
    showsPrec _ _ = showChar '_'

instance Ord variable => Semigroup (FreeVariables variable) where
    (<>) a@(FreeVariables _) b = FreeVariables $ on (<>) getFreeVariables a b

instance NamedVariable variable => Monoid (FreeVariables variable) where
    mempty = FreeVariables mempty

instance Debug variable => Debug (FreeVariables variable) where
    debugPrec _ _ = "_"

instance (Debug variable, Diff variable) => Diff (FreeVariables variable) where
    diffPrec a b = on diffPrec toList a b $> const "_"

instance NFData variable => NFData (FreeVariables variable) where
    rnf = rnf . toList

instance Hashable variable => Hashable (FreeVariables variable) where
    hashWithSalt salt = hashWithSalt salt . Map.toAscList . getFreeVariables
    {-# INLINE hashWithSalt #-}

instance
    NamedVariable variable
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
bindVariable variable (FreeVariables freeVars)=
    FreeVariables (Map.delete variable freeVars)
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
    :: NamedVariable variable
    => UnifiedVariable variable
    -> FreeVariables variable
freeVariable variable =
    FreeVariables (Map.singleton variable sort)
  where
    sort = sortedVariableSort variable
{-# INLINE freeVariable #-}

mapFreeVariables
    :: NamedVariable variable2
    => (ElementVariable variable1 -> ElementVariable variable2)
    -> (SetVariable variable1 -> SetVariable variable2)
    -> FreeVariables variable1 -> FreeVariables variable2
mapFreeVariables mapElemVar mapSetVar =
    toList
    >>> map (mapUnifiedVariable mapElemVar mapSetVar)
    >>> Set.fromList
    >>> Map.fromSet sortedVariableSort
    >>> FreeVariables
{-# INLINE mapFreeVariables #-}

traverseFreeVariables
    :: Applicative f
    => NamedVariable variable2
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

instance NamedVariable variable => HasFreeVariables () variable where
    freeVariables = const mempty
