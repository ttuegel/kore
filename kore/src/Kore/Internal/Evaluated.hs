{-|
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Internal.Evaluated
    ( Evaluated (..)
    ) where

import Prelude.Kore

import Control.DeepSeq
    ( NFData (..)
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import qualified Kore.Attribute.Pattern.Simplified as Pattern
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Unparser
    ( Unparse (..)
    )
import qualified Kore.Unparser as Unparser
import qualified Pretty

{- | @Evaluated@ wraps patterns which are fully evaluated.

Fully-evaluated patterns will not be simplified further because no progress
could be made.

 -}
newtype Evaluated child = Evaluated { getEvaluated :: child }
    deriving (Eq, Foldable, Functor, GHC.Generic, Ord, Show, Traversable)

instance SOP.Generic (Evaluated child)

instance SOP.HasDatatypeInfo (Evaluated child)

instance Debug child => Debug (Evaluated child)

instance (Debug child, Diff child) => Diff (Evaluated child)

instance Hashable child => Hashable (Evaluated child)

instance NFData child => NFData (Evaluated child)

instance Unparse child => Unparse (Evaluated child) where
    unparse evaluated =
        Pretty.vsep ["/* evaluated: */", Unparser.unparseGeneric evaluated]
    unparse2 evaluated =
        Pretty.vsep ["/* evaluated: */", Unparser.unparse2Generic evaluated]

instance Synthetic syn Evaluated where
    synthetic = getEvaluated
    {-# INLINE synthetic #-}

instance {-# OVERLAPS #-} Synthetic Pattern.Simplified Evaluated where
    synthetic = const Pattern.fullySimplified
    {-# INLINE synthetic #-}
