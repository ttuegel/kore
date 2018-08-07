{-|
Module      : Kore.Parser.Parser
Description : Parser for the Kore language
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX

This is a parser for the Kore language. Sample usage:

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
module Kore.Parser.Parser
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
    ) where

import           Kore.AST.Annotated.Kore
                 ( unannotateKorePattern )
import           Kore.AST.Annotated.Sentence
                 ( unannotateAttributes, unannotateDefinition, unannotateModule, unannotateUnifiedSentence )
import           Kore.AST.Kore
                 ( CommonKorePattern )
import           Kore.AST.Sentence
                 ( Attributes, KoreDefinition, KoreModule, KoreSentence )
import           Kore.Parser.Lexeme
                 ( skipWhitespace )
import           Kore.Parser.ParserImpl
                 ( aliasParser, inCurlyBracesListParser, inParenthesesListParser, sortParser, sortVariableParser, symbolParser, unifiedSortVariableParser, variableParser )
import qualified Kore.Parser.ParserImpl as KoreParser
                 ( attributesParser, koreDefinitionParser, korePatternParser, koreSentenceParser, moduleParser )
import           Kore.Parser.ParserUtils

{-|'koreParser' is a parser for Kore.

The input must contain a full valid Kore defininition and nothing else.
-}
koreParser :: Parser KoreDefinition
koreParser = do
    skipWhitespace
    defn <- unannotateDefinition <$> KoreParser.koreDefinitionParser
    endOfInput
    pure defn

{-|'korePatternParser' is a parser for Kore patterns.

The input must contain a full valid Kore pattern and nothing else.
-}
korePatternParser :: Parser CommonKorePattern
korePatternParser = do
    skipWhitespace
    pat <- unannotateKorePattern <$> KoreParser.korePatternParser
    endOfInput
    pure pat

{- | Parse Kore sentences.

The input must contain a full valid Kore sentence and nothing else.
-}
koreSentenceParser :: Parser KoreSentence
koreSentenceParser = do
    skipWhitespace
    sentence <- unannotateUnifiedSentence <$> KoreParser.koreSentenceParser
    endOfInput
    pure sentence

koreModuleParser :: Parser KoreModule
koreModuleParser = do
    skipWhitespace
    attributes <- unannotateModule <$> moduleParser0
    endOfInput
    pure attributes
  where
    moduleParser0 = KoreParser.moduleParser KoreParser.koreSentenceParser

attributesParser :: Parser Attributes
attributesParser = do
    skipWhitespace
    attributes <- unannotateAttributes <$> KoreParser.attributesParser
    endOfInput
    pure attributes

{-|'fromKore' takes a string representation of a Kore Definition and returns
a 'KoreDefinition' or a parse error.

The input must contain a full valid Kore definition and nothing else.
-}
fromKore :: FilePath -> String -> Either String KoreDefinition
fromKore = parseOnly koreParser

{-|'fromKorePattern' takes a string representation of a Kore Pattern and returns
a 'KorePattern' or a parse error.

The input must contain a full valid Kore pattern and nothing else.
-}
fromKorePattern :: FilePath
                -> String
                -> Either String CommonKorePattern
fromKorePattern = parseOnly korePatternParser
