{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Builtin.Endianness.Endianness
    ( Endianness (..)
    , toApplication
    ) where

import Control.DeepSeq
    ( NFData
    )
import Data.Functor.Const
import Data.Hashable
    ( Hashable
    )
import Data.Void
    ( Void
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Debug
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Functional
import Kore.Attribute.Pattern.Simplified
import Kore.Attribute.Synthetic
import Kore.Internal.Symbol
import Kore.Sort
import Kore.Syntax.Application
    ( Application (..)
    , Arguments (..)
    )
import Kore.Unparser

data Endianness
    = BigEndian !Symbol
    | LittleEndian !Symbol
    deriving (Eq, Ord, Show)
    deriving (GHC.Generic)

instance Hashable Endianness

instance NFData Endianness

instance SOP.Generic Endianness

instance SOP.HasDatatypeInfo Endianness

instance Debug Endianness

instance Diff Endianness

instance Unparse Endianness where
    unparse = unparse . toApplication @Void
    unparse2 = unparse2 . toApplication @Void

instance
    Ord variable
    => Synthetic (FreeVariables variable) (Const Endianness)
  where
    synthetic = const mempty
    {-# INLINE synthetic #-}

instance Synthetic Sort (Const Endianness) where
    synthetic = synthetic . toApplication . getConst
    {-# INLINE synthetic #-}

instance Synthetic Functional (Const Endianness) where
    synthetic = const (Functional True)
    {-# INLINE synthetic #-}

instance Synthetic Function (Const Endianness) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const Endianness) where
    synthetic = const (Defined True)
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const Endianness) where
    synthetic = const Simplified
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const Endianness) where
    synthetic =
        -- Endianness symbols are constructors
        const (ConstructorLike (Just ConstructorLikeHead))
    {-# INLINE synthetic #-}

toSymbol :: Endianness -> Symbol
toSymbol (BigEndian symbol) = symbol
toSymbol (LittleEndian symbol) = symbol

toApplication :: forall child. Endianness -> Application Symbol child
toApplication endianness = Application (toSymbol endianness) (Arguments [])
