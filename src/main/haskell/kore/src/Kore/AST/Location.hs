module Kore.AST.Location where

import Data.Hashable
       ( Hashable )
import GHC.Generics
       ( Generic )

-- | A position in a source file.
data FileLocation = FileLocation
    { fileName :: FilePath
    , line     :: Int
    , column   :: Int
    }
    deriving (Eq, Generic, Read, Show)

instance Hashable FileLocation

{- | The origin of an AST node.

The representation of @AstLocation@ is subject to change. Treat the type as an
opaque token as much as possible.

-}
data AstLocation
    = AstLocationNone
    | AstLocationImplicit
    | AstLocationGeneratedVariable
    | AstLocationTest
    | AstLocationFile FileLocation
    {- ^ @FileLocation@ may become a span instead of a single position.

      Only the Kore parser and unparser should access this constructor.
    -}
    | AstLocationLifted AstLocation
    | AstLocationUnknown
    -- ^ This should not be used and should be eliminated in further releases
    deriving (Eq, Generic, Read, Show)

instance Hashable AstLocation

-- | Display an 'AstLocation' in a user-friendly way.
prettyPrintAstLocation :: AstLocation -> String
prettyPrintAstLocation AstLocationNone = "<unknown location>"
prettyPrintAstLocation AstLocationImplicit = "<implicitly defined entity>"
prettyPrintAstLocation AstLocationGeneratedVariable =
    "<variable generated internally>"
prettyPrintAstLocation AstLocationTest = "<test data>"
prettyPrintAstLocation
    (AstLocationFile FileLocation { fileName, line, column })
  =
    fileName ++ " " ++ show line ++ ":" ++ show column
prettyPrintAstLocation (AstLocationLifted location) =
    "<lifted(" ++ prettyPrintAstLocation location ++ ")>"
prettyPrintAstLocation AstLocationUnknown = "<unknown location>"
