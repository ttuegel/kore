module Kore.Parser.Location where

import Control.DeepSeq
       ( NFData )
import GHC.Generics
       ( Generic )

import           Text.Megaparsec
                 ( SourcePos (..) )
import qualified Text.Megaparsec as Megaparsec
import           Text.Megaparsec.Pos

import Kore.Parser.ParserUtils
       ( Parser )

data SourceSpan =
    SourceSpan
    { sourceName :: FilePath
    , sourceBeginLine :: !Pos
    , sourceBeginColumn :: !Pos
    , sourceEndLine :: !Pos
    , sourceEndColumn :: !Pos
    }
  deriving (Eq, Generic, Show)

instance NFData SourceSpan

sourceSpan :: SourcePos -> SourcePos -> SourceSpan
sourceSpan
    SourcePos
    { sourceName = beginName
    , sourceLine = sourceBeginLine
    , sourceColumn = sourceBeginColumn
    }
    SourcePos
    { sourceName = endName
    , sourceLine = sourceEndLine
    , sourceColumn = sourceEndColumn
    }
    | beginName /= endName = error "sourceSpan: sourceName fields must match"
    | otherwise =
        SourceSpan
        { sourceName = beginName
        , sourceBeginLine
        , sourceBeginColumn
        , sourceEndLine
        , sourceEndColumn
        }

data Located a =
    Located
    { located :: !SourceSpan
    , unLocated :: !a
    }
  deriving (Eq, Generic, Show)

instance NFData a => NFData (Located a)

type LocatedString = Located String

parseLocated :: Parser a -> Parser (LocatedString, a)
parseLocated child = do
    begin <- Megaparsec.getPosition
    (unLocated, a) <- Megaparsec.match child  -- run child parser and keep tokens
    end <- Megaparsec.getPosition
    let located = sourceSpan begin end
    pure (Located { located, unLocated }, a)
