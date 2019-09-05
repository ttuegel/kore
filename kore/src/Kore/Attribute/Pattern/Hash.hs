module Kore.Attribute.Pattern.Hash
    ( Hash (..)
    ) where

import           Control.DeepSeq
                 ( NFData )
import           Data.Hashable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
       ( Debug )

newtype Hash = Hash { getHash :: Int }
    deriving (Eq, GHC.Generic, Ord, Read, Show)

instance NFData Hash

instance Hashable Hash where
    hash = getHash
    {-# INLINE hash #-}

    hashWithSalt salt (Hash int) = hashWithSalt salt int
    {-# INLINE hashWithSalt #-}

instance SOP.Generic Hash

instance SOP.HasDatatypeInfo Hash

instance Debug Hash
