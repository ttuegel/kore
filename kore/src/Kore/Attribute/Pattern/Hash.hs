module Kore.Attribute.Pattern.Hash
    ( Hash (..)
    ) where

import           Control.DeepSeq
                 ( NFData )
import           Data.Functor.Const
import           Data.Hashable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import           Kore.Attribute.Synthetic
import           Kore.Debug
                 ( Debug )
import           Kore.Domain.Builtin
                 ( Builtin )
import qualified Kore.Internal.Alias as Internal
import qualified Kore.Internal.Symbol as Internal
import           Kore.Syntax
import           Kore.Variables.UnifiedVariable
                 ( UnifiedVariable (..) )

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

instance Hashable sort => Synthetic Hash (And sort) where
    synthetic and' = Hash (hash and')
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Bottom sort) where
    synthetic bottom = Hash (hash bottom)
    {-# INLINE synthetic #-}

-- | An 'Application' pattern is 'Hash' if the symbol is total and its
-- arguments are 'Hash'.
instance Synthetic Hash (Application Internal.Symbol) where
    synthetic application = Hash (hash application)
    {-# INLINE synthetic #-}

instance Synthetic Hash (Application Internal.Alias) where
    synthetic application = Hash (hash application)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Ceil sort) where
    synthetic ceil' = Hash (hash ceil')
    {-# INLINE synthetic #-}

-- | A 'DomainValue' patterns is 'Hash' if its argument is 'Hash'.
instance Hashable sort => Synthetic Hash (DomainValue sort) where
    synthetic domainValue = Hash (hash domainValue)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Equals sort) where
    synthetic equals = Hash (hash equals)
    {-# INLINE synthetic #-}

instance
    (Hashable sort, Hashable variable)
    => Synthetic Hash (Exists sort variable)
  where
    synthetic exists = Hash (hash exists)

instance Hashable sort => Synthetic Hash (Floor sort) where
    synthetic floor' = Hash (hash floor')
    {-# INLINE synthetic #-}

instance
    (Hashable sort, Hashable variable)
    => Synthetic Hash (Forall sort variable)
  where
    synthetic forall = Hash (hash forall)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Iff sort) where
    synthetic iff = Hash (hash iff)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Implies sort) where
    synthetic implies = Hash (hash implies)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (In sort) where
    synthetic in' = Hash (hash in')
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Mu sort) where
    synthetic mu = Hash (hash mu)
    {-# INLINE synthetic #-}

-- | A 'Next' pattern is 'Hash' if its argument is 'Hash'.
instance Hashable sort => Synthetic Hash (Next sort) where
    synthetic next = Hash (hash next)
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Not sort) where
    synthetic not' = Hash (hash not')
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Nu sort) where
    synthetic nu = Hash (hash nu)
    {-# INLINE synthetic #-}

-- | An 'Or' pattern is 'Hash' if any of its subterms is 'Hash'.
instance Hashable sort => Synthetic Hash (Or sort) where
    synthetic or' = Hash (hash or')
    {-# INLINE synthetic #-}

instance Hashable sort => Synthetic Hash (Rewrites sort) where
    synthetic rewrites = Hash (hash rewrites)
    {-# INLINE synthetic #-}

-- | A 'Builtin' pattern is defined if its subterms are 'Hash'.
instance Hashable key => Synthetic Hash (Builtin key) where
    synthetic builtin = Hash (hash builtin)
    {-# INLINE synthetic #-}

-- | A 'Top' pattern is always 'Hash'.
instance Hashable sort => Synthetic Hash (Top sort) where
    synthetic top = Hash (hash top)
    {-# INLINE synthetic #-}

-- | A 'StringLiteral' pattern is always 'Hash'.
instance Synthetic Hash (Const StringLiteral) where
    synthetic stringLiteral = Hash (hash stringLiteral)
    {-# INLINE synthetic #-}

-- | A 'CharLiteral' pattern is always 'Hash'.
instance Synthetic Hash (Const CharLiteral) where
    synthetic charLiteral = Hash (hash charLiteral)
    {-# INLINE synthetic #-}

-- | An 'Inhabitant' pattern is always 'Hash'.
instance Synthetic Hash Inhabitant where
    synthetic inhabitant = Hash (hash inhabitant)
    {-# INLINE synthetic #-}

instance
    Hashable variable
    => Synthetic Hash (Const (UnifiedVariable variable))
  where
    synthetic (Const unifiedVar)= Hash (hash unifiedVar)
    {-# INLINE synthetic #-}
