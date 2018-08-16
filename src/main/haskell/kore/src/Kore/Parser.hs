module Kore.Parser where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import           Data.HashMap.Strict
                 ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet
                 ( HashSet )
import qualified Data.HashSet as HashSet
import Data.Maybe ( fromMaybe )
import           Data.Text
                 ( Text )
import qualified Data.Text as Text
import           Data.Void
                 ( Void )
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as Parsec.Lexer

import qualified Kore.AST.Common as Kore
import qualified Kore.AST.Kore as Kore
import           Kore.AST.MetaOrObject
                 ( IsMetaOrObject (..), Unified (..) )
import qualified Kore.Parser.CharSet as CharSet
import qualified Kore.Parser.CString as Kore.Parser

type Token = Char
type Tokens = Text
type Parser = Parsec.Parsec Void Text

space :: Parser ()
space = Parsec.Lexer.space Parsec.space1 lineComment blockComment
  where
    lineComment = Parsec.Lexer.skipLineComment "//"
    blockComment = Parsec.Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Parsec.Lexer.lexeme space

symbol :: Tokens -> Parser Tokens
symbol = Parsec.Lexer.symbol space

comma :: Parser Tokens
comma = symbol ","

colon :: Parser Tokens
colon = symbol ":"

lbrace :: Parser Tokens
lbrace = symbol "{"

rbrace :: Parser Tokens
rbrace = symbol "}"

braces :: Parser a -> Parser a
braces = Parsec.between lbrace rbrace

lparen :: Parser Tokens
lparen = symbol "("

rparen :: Parser Tokens
rparen = symbol ")"

parens :: Parser a -> Parser a
parens = Parsec.between lparen rparen

lbracket :: Parser Tokens
lbracket = symbol "["

rbracket :: Parser Tokens
rbracket = symbol "]"

brackets :: Parser a -> Parser a
brackets = Parsec.between lbracket rbracket

getLocation :: Parser Kore.AstLocation
getLocation = do
    pos <- Parsec.getPosition
    let Parsec.SourcePos { sourceName, sourceLine, sourceColumn }  = pos
    pure
        (Kore.AstLocationFile Kore.FileLocation
            { fileName = sourceName
            , line = Parsec.unPos sourceLine
            , column = Parsec.unPos sourceColumn
            }
        )

parseId :: IsMetaOrObject level -> Parser (Kore.Id level)
parseId level = do
    idLocation <- getLocation
    case level of
        IsMeta ->
            (Parsec.label "meta-level identifier" . lexeme)
            (do
                getId <- Text.unpack <$> parseMetaId
                pure Kore.Id { getId, idLocation }
            )
        IsObject ->
            (Parsec.label "object-level identifier" . lexeme)
            (do
                getId <- Text.unpack <$> parseObjectId
                pure Kore.Id { getId, idLocation }
            )

parseMetaId :: Parser Text
parseMetaId = do
    sharp <- Parsec.char '#'
    Text.cons sharp <$> (parseMetaBacktickId Parsec.<|> parseObjectId)

parseMetaBacktickId :: Parser Text
parseMetaBacktickId =
    Text.cons <$> Parsec.char '`' <*> parseObjectNonSlashId

parseObjectId :: Parser Text
parseObjectId = parseObjectSlashId Parsec.<|> parseObjectNonSlashId

parseObjectSlashId :: Parser Text
parseObjectSlashId =
    Text.cons <$> Parsec.char '\\' <*> parseObjectNonSlashId

parseObjectNonSlashId :: Parser Text
parseObjectNonSlashId = do
    letter <- Parsec.satisfy isObjectIdInitial
    nonSlashId <- Text.cons letter <$> parseIdTrailing
    assertNonKeyword nonSlashId

parseIdTrailing :: Parser Text
parseIdTrailing =
    Parsec.takeWhile1P
    (Just "letter, digit, apostrophe, or dash")
    isObjectIdTrailing

isObjectIdInitial :: Char -> Bool
isObjectIdInitial c =
    Char.isAscii c && Char.isLetter c

isObjectIdTrailing :: Char -> Bool
isObjectIdTrailing c =
    Char.isAscii c
    && (Char.isLetter c || Char.isDigit c || c == '\'' || c == '-')

assertNonKeyword :: Text -> Parser Text
assertNonKeyword ident
    | HashSet.member ident koreKeywords = fail "unexpected keyword"
    | otherwise = pure ident

parseStringLiteral :: Parser Kore.StringLiteral
parseStringLiteral =
    (Parsec.label "literal string" . lexeme)
    (parseLiteral '"' STRING toStringLiteral)
  where
    toStringLiteral = return . Kore.StringLiteral

parseCharLiteral :: Parser Kore.CharLiteral
parseCharLiteral =
    (Parsec.label "literal character" . lexeme)
    (parseLiteral '\'' STRING_END toCharLiteral)
  where
    toCharLiteral [] = fail "'' is not a valid literal character"
    toCharLiteral [c] = return (Kore.CharLiteral c)
    toCharLiteral _ = impossible

koreKeywords :: HashSet Text
koreKeywords =
    HashSet.fromList
    [ "module", "endmodule", "import"
    , "sort", "symbol", "alias", "axiom"
    , "hooked-sort", "hooked-symbol"
    ]

parseSort :: IsMetaOrObject level -> Parser (Kore.Sort level)
parseSort level =
    (Parsec.<|>)
    (Kore.SortVariableSort <$> parseSortVariable level)
    (Kore.SortActualSort <$> parseSortActual level)

parseSortVariable :: IsMetaOrObject level -> Parser (Kore.SortVariable level)
parseSortVariable level =
    Parsec.label "sort variable"
    (Kore.SortVariable <$> parseId level)

parseSortActual :: IsMetaOrObject level -> Parser (Kore.SortActual level)
parseSortActual level =
    Parsec.label "non-variable sort"
    (Kore.SortActual <$> parseId level <*> parseSortList level)

parseSortList :: IsMetaOrObject level -> Parser [Kore.Sort level]
parseSortList level =
    Parsec.label "sort list"
    (braces (Parsec.sepBy (parseSort level) comma))

parseHead :: IsMetaOrObject level -> Parser (Kore.SymbolOrAlias level)
parseHead level =
    Parsec.label "symbol or alias head"
    (Kore.SymbolOrAlias <$> parseId level <*> parseSortList level)

parsePatternList :: Parser child -> Parser [child]
parsePatternList parseChild =
    Parsec.label "pattern list"
    (parens (Parsec.sepBy parseChild comma))

parseApplication :: IsMetaOrObject level -> Parser child -> Parser (Kore.Application level child)
parseApplication level parseChild =
    Kore.Application <$> parseHead level <*> parsePatternList parseChild

parseVariable :: IsMetaOrObject level -> Parser (Kore.Variable level)
parseVariable level =
    Kore.Variable <$> parseId level <*> (colon *> parseSort level)

parseUnified
    :: (forall level.
           IsMetaOrObject level
        -> Parser (Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
       )
    -> Parser Kore.CommonKorePattern
parseUnified parseAtLevel =
    (Parsec.<|>)
    (Kore.asObjectKorePattern <$> parseAtLevel IsObject)
    (Kore.asMetaKorePattern <$> parseAtLevel IsMeta)

assert1 :: [a] -> Parser a
assert1 [a] = pure a
assert1 as = fail $! "expected one argument, found " ++ show (length as)

assert2 :: [a] -> Parser (a, a)
assert2 [a, b] = pure (a, b)
assert2 as = fail $! "expected two arguments, found " ++ show (length as)

parseBinaryOperator
    :: (forall level. f level Kore.CommonKorePattern -> Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
    -> (forall level child. Kore.Sort level -> child -> child -> f level child)
    -> Parser Kore.CommonKorePattern
parseBinaryOperator wrap construct =
    parseUnified
    (\level -> do
        sort <- parseSortList level >>= assert1
        (left, right) <- parsePatternList parsePattern >>= assert2
        (pure . wrap) (construct sort left right)
    )

parseUnaryOperator
    :: (forall level. f level Kore.CommonKorePattern -> Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
    -> (forall level child. Kore.Sort level -> child -> f level child)
    -> Parser Kore.CommonKorePattern
parseUnaryOperator wrap construct =
    parseUnified
    (\level -> do
        sort <- parseSortList level >>= assert1
        arg <- parsePatternList parsePattern >>= assert1
        (pure . wrap) (construct sort arg)
    )

parseNullary
    :: (forall level. f level Kore.CommonKorePattern -> Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
    -> (forall level child. Kore.Sort level -> f level child)
    -> Parser Kore.CommonKorePattern
parseNullary wrap construct =
    parseUnified
    (\level -> do
        sort <- parseSortList level >>= assert1
        (pure . wrap) (construct sort)
    )

parseUnaryPredicate
    :: (forall level. f level Kore.CommonKorePattern -> Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
    -> (forall level child. Kore.Sort level -> Kore.Sort level -> child -> f level child)
    -> Parser Kore.CommonKorePattern
parseUnaryPredicate wrap construct =
    parseUnified
    (\level -> do
        (operandSort, resultSort) <- parseSortList level >>= assert2
        arg <- parsePatternList parsePattern >>= assert1
        (pure . wrap) (construct operandSort resultSort arg)
    )

parseBinaryPredicate
    :: (forall level. f level Kore.CommonKorePattern -> Kore.Pattern level Kore.Variable Kore.CommonKorePattern)
    -> (forall level child. Kore.Sort level -> Kore.Sort level -> child -> child -> f level child)
    -> Parser Kore.CommonKorePattern
parseBinaryPredicate wrap construct =
    parseUnified
    (\level -> do
        (operandSort, resultSort) <- parseSortList level >>= assert2
        (a, b) <- parsePatternList parsePattern >>= assert2
        (pure . wrap) (construct operandSort resultSort a b)
    )

koreConstructors :: HashMap Text (Parser Kore.CommonKorePattern)
koreConstructors =
    HashMap.fromList
    [ ("\\and", parseBinaryOperator Kore.AndPattern Kore.And)
    , ("\\or", parseBinaryOperator Kore.OrPattern Kore.Or)
    , ("\\implies", parseBinaryOperator Kore.ImpliesPattern Kore.Implies)
    , ("\\iff", parseBinaryOperator Kore.IffPattern Kore.Iff)
    , ("\\not", parseUnaryOperator Kore.NotPattern Kore.Not)
    , ("\\top", parseNullary Kore.TopPattern Kore.Top)
    , ("\\bottom", parseNullary Kore.BottomPattern Kore.Bottom)
    , ("\\equals", parseBinaryPredicate Kore.EqualsPattern Kore.Equals)
    , ("\\in", parseBinaryPredicate Kore.InPattern Kore.In)
    , ("\\ceil", parseUnaryPredicate Kore.CeilPattern Kore.Ceil)
    , ("\\floor", parseUnaryPredicate Kore.FloorPattern Kore.Floor)
    , ("\\forall", _)
    , ("\\exists", _)
    -- Object-level pattern constructors
    , ("\\dv", Kore.asObjectKorePattern <$> _)
    , ("\\next", Kore.asObjectKorePattern <$> _)
    , ("\\rewrites", Kore.asObjectKorePattern <$> _)
    ]

parseConstructor :: Parser Kore.CommonKorePattern
parseConstructor = do
    continue <- Parsec.try (HashMap.lookup <$> parseObjectSlashId <*> pure koreConstructors)
    fromMaybe Parsec.empty continue

parsePattern :: Parser Kore.CommonKorePattern
parsePattern = _

impossible :: a
impossible = error "The impossible happened!"

data StringScannerState
    = STRING
    | STRING_END
    | ESCAPE
    | OCTAL
    | VARIABLE_HEX
    | HEX StringScannerState

parseLiteral
    :: Char -> StringScannerState -> (String -> Parser a) -> Parser a
parseLiteral delimiter nextCharState constructor = do
    Monad.void (Parsec.char delimiter)
    s <- scan STRING delta
    Monad.void (Parsec.char delimiter)
    case Kore.Parser.unescapeCString s of
        Left e   -> fail e
        Right s' -> constructor s'
  where
    pow _ 0 = id
    pow f n = f . pow f (n-1::Int)
    delta STRING c
      | c == delimiter = Nothing
      | c == '\\' = Just ESCAPE
      | otherwise = Just nextCharState
    delta STRING_END _ = Nothing
    delta ESCAPE c
      | c `CharSet.elem` Kore.Parser.oneCharEscapeDict = Just nextCharState
      | Char.isOctDigit c = Just OCTAL -- ingore actual codes for now
      | c == 'x' = Just (HEX VARIABLE_HEX)
      | c == 'u' = Just ((HEX `pow` 4) nextCharState)
      | c == 'U' = Just ((HEX `pow` 8) nextCharState)
      | otherwise = Nothing
    delta OCTAL c
      | Char.isOctDigit c = Just OCTAL
      | otherwise = delta nextCharState c
    delta VARIABLE_HEX c
      | Char.isHexDigit c = Just VARIABLE_HEX
      | otherwise = delta nextCharState c
    delta (HEX s) c
      | Char.isHexDigit c = Just s
      | otherwise = Nothing

{-|'scan' is similar to Attoparsec's 'scan'. It does the same thing as
'runScanner', but without returning the last state.
-}
scan :: a -> (a -> Char -> Maybe a) -> Parser String
scan state delta = fst <$> runScanner state delta

{-|'runScanner' is similar to Attoparsec's 'runScanner'. It parses a string
with the given state machine, stopping when the state function returns
'Nothing' or at the end of the input (without producing an error).

Returns a pair of the parsed string and the last state.
-}
runScanner :: a -> (a -> Char -> Maybe a) -> Parser (String, a)
runScanner state delta = do
    maybeC <- Parsec.optional (Parsec.lookAhead Parsec.anyChar)
    case maybeC >>= delta state of
        Nothing -> return ("", state)
        Just s -> do
            c <- Parsec.anyChar
            (reminder, finalState) <- runScanner s delta
            return (c : reminder, finalState)
