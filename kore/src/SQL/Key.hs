module SQL.Key
    ( Key(..)
    ) where

import Data.Int
    ( Int64
    )
import Data.Proxy
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Debug
import SQL.Column

{- | A foreign key into the table for type @a@.
 -}
newtype Key a = Key { getKey :: Int64 }
    deriving (Eq, Ord, Read, Show)
    deriving (Functor, Foldable)
    deriving (GHC.Generic)

instance SOP.Generic (Key a)

instance SOP.HasDatatypeInfo (Key a)

instance Debug (Key a)

instance Diff (Key a)

instance Column (Key a) where
    defineColumn _ = defineColumn (Proxy @Int64)
    toColumn = toColumn . getKey
