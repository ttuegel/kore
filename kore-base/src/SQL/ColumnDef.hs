{-|
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module SQL.ColumnDef
    ( TypeName
    , getTypeName
    , typeInteger
    , typeText
    , ColumnConstraint
    , getColumnConstraint
    , notNull
    , primaryKey
    , ColumnDef (..)
    , columnDef
    , columnNotNull
    ) where

import Prelude.Kore

import qualified Control.Lens as Lens
import Data.Generics.Product.Fields
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified GHC.Generics as GHC

newtype TypeName = TypeName { getTypeName :: String }
    deriving (Eq, Ord, Read, Show)

typeInteger :: TypeName
typeInteger = TypeName "INTEGER"

typeText :: TypeName
typeText = TypeName "TEXT"

newtype ColumnConstraint = ColumnConstraint { getColumnConstraint :: String }
    deriving (Eq, Ord, Read, Show)

notNull :: Set ColumnConstraint
notNull = Set.singleton (ColumnConstraint "NOT NULL")

primaryKey :: Set ColumnConstraint
primaryKey = Set.singleton (ColumnConstraint "PRIMARY KEY")

data ColumnDef =
    ColumnDef
        { columnType :: !TypeName
        , columnConstraints :: !(Set ColumnConstraint)
        }
    deriving (GHC.Generic)

columnDef :: TypeName -> ColumnDef
columnDef columnType = ColumnDef { columnType, columnConstraints = mempty }

columnNotNull :: ColumnDef -> ColumnDef
columnNotNull = Lens.over (field @"columnConstraints") (<> notNull)
