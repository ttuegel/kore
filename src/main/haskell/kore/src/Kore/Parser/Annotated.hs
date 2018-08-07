{-|
Module      : Kore.Parser.Annotated
Description : Annotated parser for the Kore language
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : POSIX

This is a parser for the Kore language, which annotates the parsed syntax tree
with text and location information.

Sample usage:
@
import Kore.Parser.KoreParser

import           Kore.Parser.ParserUtils (parseOnly)
import           System.Environment (getArgs)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    print (fromKore contents)
    -- or --
    print (parse koreParser fileName contents)
    -- or --
    print (parseOnly koreParser fileName contents)
@

-}
module Kore.Parser.Annotated
    ( Parser
    , fromKore
    , fromKorePattern
    , koreParser
    , attributesParser
    , korePatternParser
    , koreModuleParser
    , koreSentenceParser
    , aliasParser
    , inCurlyBracesListParser
    , inParenthesesListParser
    , sortParser
    , sortVariableParser
    , symbolParser
    , unifiedSortVariableParser
    , variableParser
    , module Kore.Parser.Location
    ) where

import           Kore.AST.Annotated.Kore
                 ( CommonKorePattern )
import           Kore.AST.Annotated.Sentence
import           Kore.Parser.Lexeme
                 ( skipWhitespace )
import           Kore.Parser.Location
import           Kore.Parser.ParserImpl
                 ( attributesParser, aliasParser, inCurlyBracesListParser, inParenthesesListParser, koreSentenceParser, sortParser, sortVariableParser, symbolParser, unifiedSortVariableParser, variableParser )
import qualified Kore.Parser.ParserImpl as KoreParser
                 ( koreDefinitionParser, korePatternParser, moduleParser )
import           Kore.Parser.ParserUtils

{-|'koreParser' is a parser for Kore.

The input must contain a full valid Kore defininition and nothing else.
-}
koreParser :: Parser (KoreDefinition LocatedString)
koreParser = skipWhitespace *> KoreParser.koreDefinitionParser <* endOfInput

{-|'korePatternParser' is a parser for Kore patterns.

The input must contain a full valid Kore pattern and nothing else.
-}
korePatternParser :: Parser (CommonKorePattern LocatedString)
korePatternParser = skipWhitespace *> KoreParser.korePatternParser <* endOfInput

koreModuleParser :: Parser (KoreModule LocatedString)
koreModuleParser = do
    skipWhitespace
    module_ <- KoreParser.moduleParser koreSentenceParser
    endOfInput
    pure module_

{-|'fromKore' takes a string representation of a Kore Definition and returns
a 'KoreDefinition' or a parse error.

The input must contain a full valid Kore definition and nothing else.
-}
fromKore :: FilePath -> String -> Either String (KoreDefinition LocatedString)
fromKore = parseOnly koreParser

{-|'fromKorePattern' takes a string representation of a Kore Pattern and returns
a 'KorePattern' or a parse error.

The input must contain a full valid Kore pattern and nothing else.
-}
fromKorePattern :: FilePath
                -> String
                -> Either String (CommonKorePattern LocatedString)
fromKorePattern = parseOnly korePatternParser
