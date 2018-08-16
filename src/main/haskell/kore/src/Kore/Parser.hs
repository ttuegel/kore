module Kore.Parser where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import           Data.Function
                 ( fix )
import qualified Data.Functor.Foldable
import           Data.HashMap.Strict
                 ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet
                 ( HashSet )
import qualified Data.HashSet as HashSet
import           Data.Maybe
                 ( fromMaybe )
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
                 ( IsMetaOrObject (..) )
import qualified Kore.AST.PureML as Kore
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

labelSortList :: Parser a -> Parser a
labelSortList = Parsec.label "sort list" . braces

parseSortList :: IsMetaOrObject level -> Parser [Kore.Sort level]
parseSortList level =
    labelSortList (Parsec.sepBy (parseSort level) comma)

parseSortList1 :: IsMetaOrObject level -> Parser (Kore.Sort level)
parseSortList1 level = labelSortList (parseSort level)

parseSortList2
    :: IsMetaOrObject level
    -> Parser (Kore.Sort level, Kore.Sort level)
parseSortList2 level =
    labelSortList ((,) <$> parseSort level <* comma <*> parseSort level)

parseHead :: IsMetaOrObject level -> Parser (Kore.SymbolOrAlias level)
parseHead level =
    Parsec.label "symbol or alias head"
    (Kore.SymbolOrAlias <$> parseId level <*> parseSortList level)

labelPatternList :: Parser a -> Parser a
labelPatternList = Parsec.label "pattern list" . parens

parsePatternList :: Parser child -> Parser [child]
parsePatternList parseChild = labelPatternList (Parsec.sepBy parseChild comma)

parsePatternList0 :: Parser ()
parsePatternList0 = labelPatternList (pure ())

parsePatternList1 :: Parser child -> Parser child
parsePatternList1 = labelPatternList

parsePatternList2 :: Parser child -> Parser (child, child)
parsePatternList2 parseChild =
    labelPatternList ((,) <$> parseChild <* comma <*> parseChild)

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

koreConstructors
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> HashMap Text (IsMetaOrObject level -> Parser (Kore.Pattern level var child))
koreConstructors parseVar parseChild =
    HashMap.fromList
    [ ("\\and", (<$>) Kore.AndPattern . parseAnd parseChild)
    , ("\\or", (<$>) Kore.OrPattern . parseOr parseChild)
    , ("\\implies", (<$>) Kore.ImpliesPattern . parseImplies parseChild)
    , ("\\iff", (<$>) Kore.IffPattern . parseIff parseChild)

    , ("\\not", (<$>) Kore.NotPattern . parseNot parseChild)

    , ("\\top", (<$>) Kore.TopPattern . parseTop)
    , ("\\bottom", (<$>) Kore.BottomPattern . parseBottom)

    , ("\\equals", (<$>) Kore.EqualsPattern . parseEquals parseChild)
    , ("\\in", (<$>) Kore.InPattern . parseIn parseChild)

    , ("\\ceil", (<$>) Kore.CeilPattern . parseCeil parseChild)
    , ("\\floor", (<$>) Kore.FloorPattern . parseFloor parseChild)

    , ("\\exists", (<$>) Kore.ExistsPattern . parseExists parseVar parseChild)
    , ("\\forall", (<$>) Kore.ForallPattern . parseForall parseVar parseChild)

    -- Object-level pattern constructors
    , ("\\dv", parseDomainValuePattern)
    , ("\\next", parseNextPattern parseChild)
    , ("\\rewrites", parseRewritesPattern parseChild)
    ]

parseBinaryOperator
    :: (Kore.Sort level -> child -> child -> f level child)
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (f level child)
parseBinaryOperator construct = \parseChild level ->
    (\sort (left, right) -> construct sort left right)
    <$> parseSortList1 level
    <*> parsePatternList2 parseChild

parseAnd :: Parser child -> IsMetaOrObject level -> Parser (Kore.And level child)
parseAnd = parseBinaryOperator Kore.And

parseOr :: Parser child -> IsMetaOrObject level -> Parser (Kore.Or level child)
parseOr = parseBinaryOperator Kore.Or

parseImplies :: Parser child -> IsMetaOrObject level -> Parser (Kore.Implies level child)
parseImplies = parseBinaryOperator Kore.Implies

parseIff :: Parser child -> IsMetaOrObject level -> Parser (Kore.Iff level child)
parseIff = parseBinaryOperator Kore.Iff

parseRewritesPattern :: Parser child -> IsMetaOrObject level -> Parser (Kore.Pattern level var child)
parseRewritesPattern parseChild =
    \case
        IsObject -> Kore.RewritesPattern <$> parseBinaryOperator Kore.Rewrites parseChild IsObject
        IsMeta -> fail "cannot have a \\rewrites meta-pattern"

parseUnaryOperator
    :: (Kore.Sort level -> child -> f level child)
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (f level child)
parseUnaryOperator construct = \parseChild level ->
    construct
    <$> parseSortList1 level
    <*> parsePatternList1 parseChild

parseNot :: Parser child -> IsMetaOrObject level -> Parser (Kore.Not level child)
parseNot = parseUnaryOperator Kore.Not

parseNextPattern :: Parser child -> IsMetaOrObject level -> Parser (Kore.Pattern level var child)
parseNextPattern parseChild =
    \case
        IsObject -> Kore.NextPattern <$> parseUnaryOperator Kore.Next parseChild IsObject
        IsMeta -> fail "cannot have a \\next meta-pattern"

parseDomainValuePattern :: IsMetaOrObject level -> Parser (Kore.Pattern level var child)
parseDomainValuePattern =
    \case
        IsObject -> Kore.DomainValuePattern <$> parseUnaryOperator Kore.DomainValue (parseCommonPurePattern IsMeta) IsObject
        IsMeta -> fail "cannot have a \\dv meta-pattern"

parseConstant
    :: (Kore.Sort level -> f level child)
    -> IsMetaOrObject level
    -> Parser (f level child)
parseConstant construct = \level ->
    construct <$> parseSortList1 level <* parsePatternList0

parseTop :: IsMetaOrObject level -> Parser (Kore.Top level child)
parseTop = parseConstant Kore.Top

parseBottom :: IsMetaOrObject level -> Parser (Kore.Bottom level child)
parseBottom = parseConstant Kore.Bottom

parseBinaryPredicate
    :: (Kore.Sort level -> Kore.Sort level -> child -> child -> f level child)
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (f level child)
parseBinaryPredicate construct = \parseChild level ->
    construct'
    <$> parseSortList2 level
    <*> parsePatternList2 parseChild
  where
    construct' (operandSort, resultSort) (left, right) =
        construct operandSort resultSort left right

parseEquals :: Parser child -> IsMetaOrObject level -> Parser (Kore.Equals level child)
parseEquals = parseBinaryPredicate Kore.Equals

parseIn :: Parser child -> IsMetaOrObject level -> Parser (Kore.In level child)
parseIn = parseBinaryPredicate Kore.In

parseUnaryPredicate
    :: (Kore.Sort level -> Kore.Sort level -> child -> f level child)
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (f level child)
parseUnaryPredicate construct = \parseChild level ->
    construct'
    <$> parseSortList2 level
    <*> parsePatternList1 parseChild
  where
    construct' (operandSort, resultSort) = construct operandSort resultSort

parseCeil :: Parser child -> IsMetaOrObject level -> Parser (Kore.Ceil level child)
parseCeil = parseUnaryPredicate Kore.Ceil

parseFloor :: Parser child -> IsMetaOrObject level -> Parser (Kore.Floor level child)
parseFloor = parseUnaryPredicate Kore.Floor

parseConstructor
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (Kore.Pattern level var child)
parseConstructor parseVar parseChild level = do
    let constructors = koreConstructors parseVar parseChild
    continue <- Parsec.try (HashMap.lookup <$> parseObjectSlashId <*> pure constructors)
    fromMaybe (const Parsec.empty) continue level

parseBinder
    :: (Kore.Sort level -> var level -> child -> f level var child)
    -> (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (f level var child)
parseBinder construct = \parseVar parseChild level ->
    construct'
    <$> parseSortList1 level
    <*> labelPatternList ((,) <$> parseVar level <* comma <*> parseChild)
  where
    construct' sort (var, child) = construct sort var child

parseExists
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (Kore.Exists level var child)
parseExists = parseBinder Kore.Exists

parseForall
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> IsMetaOrObject level
    -> Parser (Kore.Forall level var child)
parseForall = parseBinder Kore.Forall

parsePattern
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> IsMetaOrObject level
    -> Parser child
    -> Parser (Kore.Pattern level var child)
parsePattern parseVar =
    \case
        IsObject -> \parseChild ->
            parseConstructor parseVar parseChild IsObject
            Parsec.<|> (Kore.VariablePattern <$> parseVar IsObject)
        IsMeta -> \parseChild ->
            parseConstructor parseVar parseChild IsMeta
            Parsec.<|> (Kore.VariablePattern <$> parseVar IsMeta)
            Parsec.<|> (Kore.StringLiteralPattern <$> parseStringLiteral)
            Parsec.<|> (Kore.CharLiteralPattern <$> parseCharLiteral)

parsePurePattern
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> IsMetaOrObject level
    -> Parser (Kore.PureMLPattern level var)
parsePurePattern parseVar = \level ->
    fix ((<$>) Data.Functor.Foldable.embed . parsePattern parseVar level)

parseCommonPurePattern :: IsMetaOrObject level -> Parser (Kore.CommonPurePattern level)
parseCommonPurePattern =
    parsePurePattern parseVariable

parseUnifiedPattern
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser child
    -> Parser (Kore.UnifiedPattern var child)
parseUnifiedPattern parseVar = \parseChild ->
    (Parsec.<|>)
    (Kore.UnifiedMetaPattern <$> parsePattern parseVar IsMeta parseChild)
    (Kore.UnifiedObjectPattern <$> parsePattern parseVar IsObject parseChild)

parseKorePattern
    :: (forall l. IsMetaOrObject l -> Parser (var l))
    -> Parser (Kore.KorePattern var)
parseKorePattern parseVar =
    fix ((<$>) Data.Functor.Foldable.embed . parseUnifiedPattern parseVar)

parseCommonKorePattern :: Parser Kore.CommonKorePattern
parseCommonKorePattern = parseKorePattern parseVariable

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
