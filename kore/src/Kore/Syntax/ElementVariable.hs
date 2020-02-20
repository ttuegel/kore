{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

module Kore.Syntax.ElementVariable
    ( ElementVariable (..)
    ) where

import Prelude.Kore

import Control.DeepSeq
    ( NFData (..)
    )
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import Data.Hashable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
import Kore.Syntax.Variable
    ( SortedVariable (..)
    )
import Kore.Unparser
import qualified Pretty
import qualified SQL

-- | Element (singleton) Kore variables
newtype ElementVariable variable
    = ElementVariable { getElementVariable :: variable }
    deriving (Eq, GHC.Generic, Ord, Show, Functor, Foldable, Traversable)

instance Hashable variable => Hashable (ElementVariable variable)

instance NFData variable => NFData (ElementVariable variable)

instance SOP.Generic (ElementVariable variable)

instance SOP.HasDatatypeInfo (ElementVariable variable)

instance Debug variable => Debug (ElementVariable variable)

instance (Debug variable, Diff variable) => Diff (ElementVariable variable)

instance Unparse variable => Unparse (ElementVariable variable) where
    unparse = unparse . getElementVariable
    unparse2 = unparse2 . getElementVariable

instance
    SortedVariable variable => SortedVariable (ElementVariable variable)
  where
    lensVariableSort = _Unwrapped . lensVariableSort
    {-# INLINE lensVariableSort #-}

instance Unparse variable => SQL.Column (ElementVariable variable) where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . unparse
