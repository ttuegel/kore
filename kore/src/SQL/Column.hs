{-|
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module SQL.Column
    ( TypeName
    , getTypeName
    , typeInteger
    , typeText
    , ColumnConstraints (..)
    , ColumnDef (..)
    , Column (..)
    , columnDef
    , columnNotNull
    , defineTextColumn
    ) where

import Prelude.Kore

import qualified Control.Lens as Lens
import Data.Default
import Data.Generics.Product.Fields
import Data.Int
    ( Int64
    )
import Data.Proxy
import Data.Text
    ( Text
    )
import qualified Database.SQLite.Simple as SQLite
import qualified GHC.Generics as GHC

import SQL.SQL

newtype TypeName = TypeName { getTypeName :: String }
    deriving (Eq, Ord, Read, Show)

typeInteger :: TypeName
typeInteger = TypeName "INTEGER"

typeText :: TypeName
typeText = TypeName "TEXT"

data ColumnConstraints =
    ColumnConstraints
        { notNull :: !Bool
        }
    deriving (GHC.Generic)

instance Default ColumnConstraints where
    def =
        ColumnConstraints
            { notNull = False
            }

data ColumnDef =
    ColumnDef
        { columnType :: !TypeName
        , columnConstraints :: !ColumnConstraints
        }
    deriving (GHC.Generic)

columnDef :: TypeName -> ColumnDef
columnDef columnType = ColumnDef { columnType, columnConstraints = def }

columnNotNull :: ColumnDef -> ColumnDef
columnNotNull = Lens.set (field @"columnConstraints" . field @"notNull") True

class Column a where
    defineColumn :: proxy a -> SQL ColumnDef
    toColumn :: a -> SQL SQLite.SQLData
    -- TODO (thomas.tuegel): Implement this!
    -- fromColumn :: SQLite.Connection -> SQLite.SQLData -> IO a

instance Column Int64 where
    defineColumn _ = return (columnNotNull $ columnDef typeInteger)
    toColumn = return . SQLite.SQLInteger

instance Column Text where
    defineColumn _ = return (columnNotNull $ columnDef typeText)
    toColumn = return . SQLite.SQLText

defineTextColumn :: proxy a -> SQL ColumnDef
defineTextColumn _ = defineColumn (Proxy @Text)
