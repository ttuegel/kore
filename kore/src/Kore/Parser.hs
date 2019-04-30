{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

This is a parser for the Kore language. Sample usage:

@
import Kore.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    print (parseDefinition contents)
@

-}
module Kore.Parser
    ( parseDefinition
    , parsePattern
    , definitionParser
    , patternParser
    -- * Re-exports
    , Pattern
    , asPattern
    , ParsedSentence
    , ParsedDefinition
    ) where

import           Kore.Parser.Lexeme
                 ( skipWhitespace )
import qualified Kore.Parser.Parser as Parser
import           Kore.Parser.ParserUtils
import           Kore.Parser.Pattern
import           Kore.Parser.Sentence

{- | 'definitionParser' is a parser for Kore definitions.

The input must contain a full valid Kore definition and nothing else.

 -}
definitionParser :: Parser ParsedDefinition
definitionParser = skipWhitespace *> Parser.koreDefinitionParser <* endOfInput

{- | 'patternParser' is a parser for Kore patterns.

The input must contain a full valid Kore pattern and nothing else.

 -}
patternParser :: Parser (Pattern Variable)
patternParser = Parser.korePatternParser

{- | Parse a string representing a Kore definition.

@parseDefinition@ returns a 'Definition' upon success, or an parse error
message otherwise. The input must contain a valid Kore definition and nothing
else.

See also: 'definitionParser'

 -}
parseDefinition
    :: FilePath  -- ^ Filename used for error messages
    -> String  -- ^ The concrete syntax of a valid Kore definition
    -> Either String ParsedDefinition
parseDefinition = parseOnly definitionParser

{- | Parse a string representing a Kore pattern.

@parsePattern@ returns a 'Pattern' upon success, or an parse error message
otherwise. The input must contain a valid Kore pattern and nothing else.

See also: 'patternParser'

 -}
parsePattern
    :: FilePath  -- ^ Filename used for error messages
    -> String  -- ^ The concrete syntax of a valid Kore pattern
    -> Either String (Pattern Variable)
parsePattern = parseOnly patternParser
