{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

Parser definition for Kore. Meant for internal use only.

Conventions used:

1. In various cases we distinguish between @object-@ and @meta-@ versions of an
   element. For this we parametrize the element's type with a @level@ and we
   provide an element of type @level@ to the parser, usually called @x@.

2. The meta versions are identified by the presence of @#@ characters, usually
   found at the start of the element. However, when they are found inside,
   we may split the parser in two pieces, one that parses everything until the
   first element that may start with @#@ and identifies the value of @x@ and
   another one that receives 'x' and parses the reminder.

3. Whenever we have both an element which can be meta or object and
   an element which is the union of the two, the union is called 'Unified*'.
   As an example, if we have @⟨object-variable⟩@, @⟨meta-variable⟩@ and
   @⟨variable⟩ ::= ⟨object-variable⟩ | ⟨meta-variable⟩@, then we'll represent
   the fist two by "Variable" and the latter by "UnifiedVariable".

3. Parsers called 'Raw' receive a constructor that constructs the element.

4. Parsers called 'Reminder' receive an element's parsed prefix and an element
   constructor, parse whatever is left of the element and construct it.

5. All parsers consume the whitespace after the parsed element and expect no
   whitespace before.
-}
module Kore.Parser.Parser where

import           Control.Arrow
                 ( (&&&) )
import           Control.Monad
                 ( unless, void )
import           Data.Maybe
                 ( isJust )
import           Text.Megaparsec
                 ( some )
import qualified Text.Megaparsec.Char as Parser
                 ( char )

import           Kore.AST.Common
                 ( MLPatternType (..), allPatternTypes, patternString )
import           Kore.AST.MetaOrObject
import           Kore.Parser.Lexeme
import           Kore.Parser.ParserUtils
                 ( Parser )
import qualified Kore.Parser.ParserUtils as ParserUtils
import           Kore.Parser.Pattern
import           Kore.Parser.Sentence

{-|'sortVariableParser' parses either an @object-sort-variable@, or a
@meta-sort-variable@.

BNF definition:

@
⟨object-sort-variable⟩ ::= ⟨object-identifier⟩
⟨meta-sort-variable⟩ ::= ⟨meta-identifier⟩
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
sortVariableParser :: Parser SortVariable
sortVariableParser = SortVariable <$> idParser

{-|'sortParser' parses either an @object-sort@, or a @meta-sort@.

BNF definition:

@
⟨object-sort⟩ ::=
    | ⟨object-sort-variable⟩
    | ⟨object-sort-constructor⟩ ‘{’ ⟨object-sort-list⟩ ‘}’
⟨meta-sort⟩ ::= ⟨meta-sort-variable⟩ | ⟨meta-sort-constructor⟩ ‘{’ ‘}’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
sortParser :: Parser Sort
sortParser = do
    identifier <- idParser
    c <- ParserUtils.peekChar
    case c of
        Just '{' -> actualSortParser identifier
        _        -> return (SortVariableSort $ SortVariable identifier)
  where
    actualSortParser identifier = do
        sorts <- inCurlyBracesListParser sortParser
        return $ SortActualSort SortActual
            { sortActualName = identifier
            , sortActualSorts = sorts
            }

{-|'validateMetaSort' checks that a @meta-sort@ is well-formed.

Relevant BNF definitions:

@
⟨meta-sort⟩ ::= ⟨meta-sort-variable⟩ | ⟨meta-sort-constructor⟩ ‘{’ ‘}’
⟨meta-sort-constructor⟩ ::=
    | ‘#Char’       | ‘#String’
@
-}
validateMetaSort
    :: Id     -- ^ The sort name
    -> [Sort] -- ^ The sort arguments
    -> Parser ()
validateMetaSort identifier [] =
    unless (isJust (metaSortConverter metaId))
        (fail ("metaSortConverter: Invalid constructor: '" ++ metaId ++ "'."))
  where
    metaId = getIdForError identifier
validateMetaSort _ _ = fail "metaSortConverter: Non empty parameter sorts."

{-|'symbolOrAliasDeclarationRawParser' parses a head and constructs it using the provided
constructor.

BNF definitions:

@
⟨object-head⟩ ::= ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-list⟩ ‘}’
⟨meta-head⟩ ::= ⟨meta-head-constructor⟩ ‘{’ ⟨meta-sort-list⟩ ‘}’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
symbolOrAliasDeclarationRawParser
    :: (Id -> [SortVariable] -> result)  -- ^ Element constructor.
    -> Parser result
symbolOrAliasDeclarationRawParser constructor = do
    headConstructor <- idParser
    symbolOrAliasDeclarationRemainderRawParser (constructor headConstructor)

{-|'symbolOrAliasDeclarationRemainderRawParser' parses the sort list that occurs
in heads and constructs the head using the provided constructor.

BNF fragments:

@
... ::= ... ‘{’ ⟨object-sort-variable-list⟩ ‘}’ ...
... ::= ... ‘{’ head-sort-variable-list⟩ ‘}’ ...
@

Always starts with @{@.
-}
symbolOrAliasDeclarationRemainderRawParser
    :: ([SortVariable] -> result)  -- ^ Element constructor.
    -> Parser result
symbolOrAliasDeclarationRemainderRawParser constructor =
    constructor <$> inCurlyBracesListParser sortVariableParser

{-|'aliasParser' parses either an @object-head@ or a @meta-head@ and interprets
it as an alias head.

BNF definitions:

@
⟨object-head⟩ ::= ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-list⟩ ‘}’
⟨meta-head⟩ ::= ⟨meta-head-constructor⟩ ‘{’ ⟨meta-sort-list⟩ ‘}’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
aliasParser :: Parser (Alias Object)
aliasParser = symbolOrAliasDeclarationRawParser Alias


{-|'symbolParser' is the same as 'aliasParser', but it interprets the head
as a symbol one.
-}
symbolParser :: Parser (Symbol Object)
symbolParser = symbolOrAliasDeclarationRawParser Symbol

{-|'unaryOperatorRemainderParser' parses the part after an unary operator's
name and the first open curly brace and constructs it using the provided
constructor.
It uses an open recursion scheme for the children.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
... ::= ... ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
unaryOperatorRemainderParser
    :: Parser child
    -> (Sort -> child -> result)
    -- ^ Element constructor.
    -> Parser result
unaryOperatorRemainderParser childParser constructor =
    constructor
    <$> inCurlyBracesRemainderParser sortParser
    <*> inParenthesesParser childParser

{-|'binaryOperatorRemainderParser' parses the part after a binary operator's
name and the first open curly brace and constructs it using the provided
constructor.
It uses an open recursion scheme for the children.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
... ::= ... ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
binaryOperatorRemainderParser
    :: Parser child
    -> (Sort -> child -> child -> m child)
    -- ^ Element constructor.
    -> Parser (m child)
binaryOperatorRemainderParser childParser constructor = do
    sort <- inCurlyBracesRemainderParser sortParser
    (child1, child2) <- parenPairParser childParser childParser
    return (constructor sort child1 child2)

{-|'existsForallRemainderParser' parses the part after an exists or forall
operator's name and the first open curly brace and constructs it using the
provided constructor.
It uses an open recursion scheme for the children.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘}’ ‘(’ ⟨object-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
... ::= ... ⟨meta-sort⟩ ‘}’ ‘(’ ⟨meta-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
existsForallRemainderParser
    :: Parser child
    -> (Sort -> Variable -> child -> m child)
    -- ^ Element constructor.
    -> Parser (m child)
existsForallRemainderParser childParser constructor = do
    sort <- inCurlyBracesRemainderParser sortParser
    (variable, qChild) <- parenPairParser variableParser childParser
    return (constructor sort variable qChild)

{-|'ceilFloorRemainderParser' parses the part after a ceil or floor
operator's name and the first open curly brace and constructs it using the
provided constructor.
It uses an open recursion scheme for the children.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
... ::= ... ⟨meta-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
ceilFloorRemainderParser
    :: Parser child
    -> (Sort -> Sort -> child -> m child)
    -- ^ Element constructor.
    -> Parser (m child)
ceilFloorRemainderParser childParser constructor = do
    (sort1, sort2) <- curlyPairRemainderParser sortParser
    cfChild <- inParenthesesParser childParser
    return (constructor sort1 sort2 cfChild)

{-|'equalsInRemainderParser' parses the part after an equals or in
operator's name and the first open curly brace and constructs it using the
provided constructor.
It uses an open recursion scheme for the children.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
... ::= ... ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
equalsInRemainderParser
    :: Parser child
    -> (Sort -> Sort -> child -> child -> m child)
    -- ^ Element constructor.
    -> Parser (m child)
equalsInRemainderParser childParser constructor = do
    (sort1, sort2) <- curlyPairRemainderParser sortParser
    (child1, child2) <- parenPairParser childParser childParser
    return (constructor sort1 sort2 child1 child2)

{-|'topBottomRemainderParser' parses the part after a top or bottom
operator's name and the first open curly brace and constructs it using the
provided constructor.

BNF fragments:

@
... ::= ... ⟨object-sort⟩ ‘}’ ‘(’ ‘)’
... ::= ... ⟨meta-sort⟩ ‘}’ ‘(’ ‘)’
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
topBottomRemainderParser
    :: (Sort -> m child)  -- ^ Element constructor.
    -> Parser (m child)
topBottomRemainderParser constructor = do
    sort <- inCurlyBracesRemainderParser sortParser
    inParenthesesParser (return ())
    return (constructor sort)

{-|'symbolOrAliasPatternRemainderParser' parses the part after a the first
identifier in an application pattern and constructs it.
It uses an open recursion scheme for the children.

BNF fragments:

@
⟨object-pattern⟩ = ⟨object-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
⟨object-head⟩ ::= ... ‘{’ ⟨object-sort-list⟩ ‘}’

⟨meta-pattern⟩ = ⟨meta-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
⟨meta-head⟩ ::= ... ‘{’ ⟨meta-sort-list⟩ ‘}’
@

Always starts with @{@.
-}
symbolOrAliasPatternRemainderParser
    :: Parser child
    -> Id  -- ^ The already parsed prefix.
    -> Parser (PatternF Variable child)
symbolOrAliasPatternRemainderParser childParser identifier =
    ApplicationF
    <$> (   Application
        <$> (   SymbolOrAlias identifier
            <$> inCurlyBracesListParser sortParser
            )
        <*> inParenthesesListParser childParser
        )

applicationParser
    :: Parser child
    -> Parser (Application SymbolOrAlias child)
applicationParser childParser =
    Application
        <$> headParser
        <*> inParenthesesListParser childParser

{-|'variableRemainderParser' parses the part after a variable's name and
constructs it.

BNF fragments:

@
⟨object-variable⟩ ::= ... ‘:’ ⟨object-sort⟩
@

Always starts with @:@.
-}
variableRemainderParser
    :: Id  -- ^ The already parsed prefix.
    -> Parser Variable
variableRemainderParser identifier = do
    colonParser
    sort <- sortParser
    return Variable
        { variableName = identifier
        , variableSort = sort
        , variableCounter = mempty
        }

{-|'variableParser' parses either an @object-variable@, or a @meta-variable@.

BNF definitions:

@
⟨object-variable⟩ ::= ⟨object-identifier⟩ ‘:’ ⟨object-sort⟩
⟨meta-variable⟩ ::= ⟨meta-identifier⟩ ‘:’ ⟨meta-sort⟩
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
variableParser :: Parser Variable
variableParser = idParser >>= variableRemainderParser

{-|'variableOrTermPatternParser' parses an (object or meta) (variable pattern or
application pattern), using an open recursion scheme for its children.

BNF definitions:

@
⟨pattern⟩ ::=
    | ⟨variable⟩
    | ⟨set-variable⟩
    | ⟨object-head⟩ ‘(’ ⟨child-list⟩ ‘)’
⟨variable⟩ ::= ⟨object-identifier⟩ ‘:’ ⟨object-sort⟩
⟨set-variable⟩ ::= '#' ⟨object-identifier⟩ ‘:’ ⟨object-sort⟩
⟨object-head⟩ ::= ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-list⟩ ‘}’
⟨object-head-constructor⟩ ::= ⟨object-identifier⟩
@
-}
variableOrTermPatternParser
    :: Parser child
    -> Bool  -- ^ Whether it can be a Set Variable
    -> Parser (PatternF Variable child)
variableOrTermPatternParser childParser isSetVar = do
    identifier <- idParser
    c <- ParserUtils.peekChar'
    if c == ':'
        then do
            var <- variableRemainderParser identifier
            if isSetVar
                then return . SetVariableF . SetVariable $ var
                else return $ VariableF var
        else symbolOrAliasPatternRemainderParser childParser identifier


{-| parses a symbol or alias constructor and sort list
@
⟨head⟩ ::= ⟨object-head⟩ | ⟨meta-head⟩

⟨object-head⟩ ::= ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-list⟩ ‘}’
⟨object-head-constructor⟩ ::= ⟨object-identifier⟩

⟨meta-head⟩ ::= ⟨meta-head-constructor⟩ ‘{’ ⟨meta-sort-list⟩ ‘}’
⟨meta-head-constructor⟩ ::= ⟨meta-identifier⟩
@
-}
headParser :: Parser SymbolOrAlias
headParser =
    SymbolOrAlias
        <$> idParser
        <*> inCurlyBracesListParser sortParser

{-|'koreVariableOrTermPatternParser' parses a variable pattern or an
application one.

BNF definitions:

@
⟨object-pattern⟩ ::=
    | ⟨object-variable⟩
    | ⟨object-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
⟨meta-pattern⟩ ::=
    | ⟨meta-variable⟩
    | ⟨meta-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
@
-}
koreVariableOrTermPatternParser :: Parser (Pattern Variable)
koreVariableOrTermPatternParser = do
    c <- ParserUtils.peekChar'
    asPattern <$> variableOrTermPatternParser
        korePatternParser
        (c == '#')

{-|'koreMLConstructorParser' parses a pattern starting with @\@.

BNF definitions:

@
⟨object-pattern⟩ ::=
   | ‘\and’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\not’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\or’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\implies’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\iff’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\forall’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨object-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\exists’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨object-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\ceil’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\dv’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\floor’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\equals’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\in’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ pattern ‘,’ ⟨pattern⟩ ‘)’
    | ‘\next’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\rewrites’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\top’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ‘)’
    | ‘\bottom’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ‘)’

⟨meta-pattern⟩ ::=
    | ‘\and’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\not’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\or’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\implies’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\iff’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\forall’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨meta-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\exists’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨meta-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\ceil’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\floor’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\equals’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\in’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ pattern ‘,’ ⟨pattern⟩ ‘)’
    | ‘\top’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ‘)’
    | ‘\bottom’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ‘)’
@

Always starts with @\@.
-}
koreMLConstructorParser :: Parser (Pattern Variable)
koreMLConstructorParser = do
    void (Parser.char '\\')
    asPattern <$> mlPatternParser
  where
    mlPatternParser = keywordBasedParsers
        (map
            (patternString &&& koreMLConstructorRemainderParser)
            allPatternTypes
        )
    koreMLConstructorRemainderParser patternType = do
        openCurlyBraceParser
        mlConstructorRemainderParser
            korePatternParser
            patternType

{-|'leveledMLConstructorParser' is similar to 'koreMLConstructorParser'
in that it parses a pattern starting with @\@.  However, it only parses
patterns types which can belong to both 'Meta' and 'Object' categories, and
returns an object of the 'Pattern' type.

BNF definitions (here cat ranges over meta and object):

@
⟨cat-pattern⟩ ::=
    | ‘\and’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\not’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\or’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\implies’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\iff’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\forall’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨cat-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\exists’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨cat-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\ceil’ ‘{’ ⟨cat-sort⟩ ‘,’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\floor’ ‘{’ ⟨cat-sort⟩ ‘,’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\equals’ ‘{’ ⟨cat-sort⟩ ‘,’ ⟨cat-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\in’ ‘{’ ⟨cat-sort⟩ ‘,’ ⟨cat-sort⟩ ‘}’ ‘(’ pattern ‘,’ ⟨pattern⟩ ‘)’
    | ‘\top’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ‘)’
    | ‘\bottom’ ‘{’ ⟨cat-sort⟩ ‘}’ ‘(’ ‘)’
@
-}
leveledMLConstructorParser
    :: Parser child
    -> Parser (PatternF Variable child)
leveledMLConstructorParser childParser = do
    void (Parser.char '\\')
    keywordBasedParsers
        (map
            (patternString &&& leveledMLConstructorRemainderParser)
            allPatternTypes
        )
  where
    leveledMLConstructorRemainderParser patternType = do
        openCurlyBraceParser
        mlConstructorRemainderParser
            childParser
            patternType

{-|'mlConstructorRemainderParser' represents a continuation parser for
'leveledMLConstructorParser', called after the constructor and the open curly
brace were parsed. Note that parsing the constructor and open curly brace is
required to be able to peek at the first character of the sort identifier, in
order to determine whether we are parsing a 'Meta' or an 'Object' 'Pattern'.
-}
mlConstructorRemainderParser
    :: Parser child
    -> MLPatternType
    -> Parser (PatternF Variable child)
mlConstructorRemainderParser childParser patternType =
    case patternType of
        AndPatternType -> AndF <$>
            binaryOperatorRemainderParser childParser And
        BottomPatternType -> BottomF <$>
            topBottomRemainderParser Bottom
        CeilPatternType -> CeilF <$>
            ceilFloorRemainderParser childParser Ceil
        EqualsPatternType -> EqualsF <$>
            equalsInRemainderParser childParser Equals
        ExistsPatternType -> ExistsF <$>
            existsForallRemainderParser childParser Exists
        FloorPatternType -> FloorF <$>
            ceilFloorRemainderParser childParser Floor
        ForallPatternType -> ForallF <$>
            existsForallRemainderParser childParser Forall
        IffPatternType -> IffF <$>
            binaryOperatorRemainderParser childParser Iff
        ImpliesPatternType -> ImpliesF <$>
            binaryOperatorRemainderParser childParser Implies
        InPatternType -> InF <$>
            equalsInRemainderParser childParser In
        NotPatternType -> NotF <$>
            unaryOperatorRemainderParser childParser Not
        OrPatternType -> OrF <$>
            binaryOperatorRemainderParser childParser Or
        TopPatternType -> TopF <$>
            topBottomRemainderParser Top
        DomainValuePatternType ->
            DomainValueF <$> domainValueParser childParser
        NextPatternType ->
            NextF
            <$> unaryOperatorRemainderParser childParser Next
        RewritesPatternType ->
            RewritesF
            <$> binaryOperatorRemainderParser childParser Rewrites

domainValueParser :: Parser child -> Parser (DomainValue Sort child)
domainValueParser childParser = do
    domainValueSort <- inCurlyBracesRemainderParser sortParser
    domainValueChild <- inParenthesesParser childParser
    return DomainValue { domainValueSort, domainValueChild }

{-|'korePatternParser' parses an unifiedPattern

BNF definitions:

@
⟨object-pattern⟩ ::=
    | ⟨object-variable⟩
    | ⟨object-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
    | ‘\and’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\not’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\or’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\implies’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\iff’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\forall’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨object-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\exists’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨object-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\ceil’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\floor’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\equals’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\in’ ‘{’ ⟨object-sort⟩ ‘,’ ⟨object-sort⟩ ‘}’ ‘(’ pattern ‘,’ ⟨pattern⟩ ‘)’
    | ‘\top’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ‘)’
    | ‘\bottom’ ‘{’ ⟨object-sort⟩ ‘}’ ‘(’ ‘)’

⟨meta-pattern⟩ ::=
    | ⟨meta-variable⟩
    | ⟨char⟩
    | ⟨string⟩
    | ⟨meta-head⟩ ‘(’ ⟨pattern-list⟩ ‘)’
    | ‘\and’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\not’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\or’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\implies’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\iff’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\forall’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨meta-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\exists’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨meta-variable⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\ceil’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\floor’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘)’
    | ‘\equals’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ ⟨pattern⟩ ‘,’ ⟨pattern⟩ ‘)’
    | ‘\in’ ‘{’ ⟨meta-sort⟩ ‘,’ ⟨meta-sort⟩ ‘}’ ‘(’ pattern ‘,’ ⟨pattern⟩ ‘)’
    | ‘\top’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ‘)’
    | ‘\bottom’ ‘{’ ⟨meta-sort⟩ ‘}’ ‘(’ ‘)’
@

Note that the @meta-pattern@ can be a @string@, while the @object-pattern@
can't.
-}
korePatternParser :: Parser (Pattern Variable)
korePatternParser = do
    c <- ParserUtils.peekChar'
    case c of
        '\\' -> koreMLConstructorParser
        '"'  -> asPattern . StringLiteralF <$> stringLiteralParser
        '\'' -> asPattern . CharLiteralF <$> charLiteralParser
        _    -> koreVariableOrTermPatternParser

{-|'inSquareBracketsListParser' parses a @list@ of items delimited by
square brackets and separated by commas.

Always starts with @[@,
-}
inSquareBracketsListParser :: Parser item -> Parser [item]
inSquareBracketsListParser =
    ParserUtils.sepByCharWithDelimitingChars skipWhitespace '[' ']' ','

{-|'inParenthesesListParser' is the same as
'inSquareBracketsListParser' except that it uses parentheses instead of
square brackets.
-}
inParenthesesListParser :: Parser item -> Parser [item]
inParenthesesListParser =
    ParserUtils.sepByCharWithDelimitingChars skipWhitespace '(' ')' ','

{-|'inCurlyBracesListParser' is the same as
'inSquareBracketsListParser' except that it uses curly braces instead of
square brackets.
-}
inCurlyBracesListParser :: Parser item -> Parser [item]
inCurlyBracesListParser =
    ParserUtils.sepByCharWithDelimitingChars skipWhitespace '{' '}' ','

{-|'attributesParser' parses an @attribute@.

BNF definition:

@
⟨attribute⟩ ::= ‘[’ ⟨pattern-list⟩ ‘]’
@

Always starts with @[@.
-}
attributesParser :: Parser Attributes
attributesParser = Attributes <$> inSquareBracketsListParser korePatternParser

{-|'koreDefinitionParser' parses a Kore @definition@

BNF definition:
@
⟨definition⟩ ::= ⟨attribute⟩ ‘module’ ⟨module-name⟩ ⟨declaration⟩ ∗ ‘endmodule’ ⟨attribute⟩
@
-}
koreDefinitionParser :: Parser ParsedDefinition
koreDefinitionParser = definitionParser koreSentenceParser

definitionParser
    :: Parser sentence
    -> Parser (Definition sentence)
definitionParser sentenceParser =
    Definition
        <$> attributesParser
        <*> some (moduleParser sentenceParser)

{-|'moduleParser' parses the module part of a Kore @definition@

BNF definition fragment:
@
... ::= ... ‘module’ ⟨module-name⟩ ⟨declaration⟩ ∗ ‘endmodule’ ⟨attribute⟩ ...
@
-}
moduleParser
    :: Parser sentence
    -> Parser (Module sentence)
moduleParser sentenceParser = do
    mlLexemeParser "module"
    name <- moduleNameParser
    sentences <- ParserUtils.manyUntilChar 'e' sentenceParser
    mlLexemeParser "endmodule"
    attributes <- attributesParser
    return Module
           { moduleName = name
           , moduleSentences = sentences
           , moduleAttributes = attributes
           }

{-|Enum for the sentence types that have meta- and object- versions.
-}
data SentenceType
    = AliasSentenceType
    | SymbolSentenceType


{-|'koreSentenceParser' parses a @declaration@.

BNF definition fragments:
@
⟨declaration⟩ ::=
    | ⟨import-declaration⟩
    | ⟨sort-declaration⟩
    | ⟨symbol-declaration⟩
    | ⟨alias-declaration⟩
    | ⟨axiom-declaration⟩
    | ⟨hook-declaration⟩
⟨axiom-declaration⟩ ::= ‘axiom’ ...
⟨sort-declaration⟩ ::= ‘sort’ ...
⟨import-declaration⟩ ::= ‘import’ ⟨module-name⟩ ⟨attribute⟩
⟨symbol-declaration⟩ ::= ( ⟨object-symbol-declaration⟩ | ⟨meta-symbol-declaration⟩ ) ⟨attribute⟩
⟨object-symbol-declaration⟩ ::= ‘symbol’ ...
⟨meta-symbol-declaration⟩ ::= ‘symbol’ ...
⟨alias-declaration⟩ ::= ( ⟨object-alias-declaration⟩ | ⟨meta-alias-declaration⟩ ) ⟨attribute⟩
⟨object-alias-declaration⟩ ::= ‘alias’ ...
⟨meta-alias-declaration⟩ ::= ‘alias’ ...
⟨hook-declararion⟩ ::= ‘hooked-sort’ ... | 'hooked-symbol' ...
@
-}
koreSentenceParser :: Parser ParsedSentence
koreSentenceParser = keywordBasedParsers
    [ ( "alias", sentenceConstructorRemainderParser AliasSentenceType )
    , ( "axiom", axiomSentenceRemainderParser SentenceAxiomSentence )
    , ( "claim", axiomSentenceRemainderParser SentenceClaimSentence )
    , ( "sort", sentenceSortRemainderParser )
    , ( "symbol", sentenceConstructorRemainderParser SymbolSentenceType )
    , ( "import", importSentenceRemainderParser )
    , ( "hooked-sort", hookedSortSentenceRemainderParser )
    , ( "hooked-symbol", hookedSymbolSentenceRemainderParser )
    ]

sentenceConstructorRemainderParser :: SentenceType -> Parser ParsedSentence
sentenceConstructorRemainderParser AliasSentenceType =
    SentenceAliasSentence <$> aliasSentenceRemainderParser
sentenceConstructorRemainderParser SymbolSentenceType =
    SentenceSymbolSentence
    <$> symbolSentenceRemainderParser symbolParser SentenceSymbol

sentenceSortRemainderParser :: Parser ParsedSentence
sentenceSortRemainderParser =
    SentenceSortSentence <$> sortSentenceRemainderParser

{-|'symbolSentenceRemainderParser' parses the part after the starting
keyword of an alias or symbol declaration using the given head parser
to parse the head and constructs it using the given constructor.

BNF fragment example:

@
... ::=  ... ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-variable-list⟩ ‘}’
             ‘(’ ⟨object-sort-variable-list⟩ ‘)’ ‘:’ ⟨object-sort⟩ ⟨attribute⟩
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
symbolSentenceRemainderParser
    :: Parser m  -- Head parser.
    -> (m -> [Sort] -> Sort -> Attributes -> as)
    -- ^ Element constructor.
    -> Parser as
symbolSentenceRemainderParser aliasSymbolParser constructor
  = do
    aliasSymbol <- aliasSymbolParser
    sorts <- inParenthesesListParser sortParser
    colonParser
    resultSort <- sortParser
    attributes <- attributesParser
    return (constructor aliasSymbol sorts resultSort attributes)


{-|'aliasSentenceRemainderParser' parses the part after the starting
keyword of an alias declaration.

BNF fragment example:

@
... ::=  `alias` ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-variable-list⟩ ‘}’ ‘(’ ⟨object-sort-list⟩ ‘)’ ‘:’ ⟨object-sort⟩ ⟨attribute⟩
         `where` ⟨object-head-constructor⟩ ‘{’ ⟨object-sort-variable-list⟩ ‘}’ ‘(’ ⟨object-variable-list⟩ ‘)’ `:=` ⟨object-pattern⟩
@

The @meta-@ version always starts with @#@, while the @object-@ one does not.
-}
aliasSentenceRemainderParser :: Parser (SentenceAlias Object (Pattern Variable))
aliasSentenceRemainderParser = do
    aliasSymbol <- aliasParser
    sorts <- inParenthesesListParser sortParser
    colonParser
    resultSort <- sortParser
    mlLexemeParser "where"
    -- Note: constraints for left pattern checked in verifySentence
    leftPattern <- applicationParser variableParser
    mlLexemeParser ":="
    rightPattern <- korePatternParser
    attributes <- attributesParser
    return (SentenceAlias aliasSymbol sorts resultSort leftPattern rightPattern attributes)

{-|'importSentenceRemainderParser' parses the part after the starting
'import' keyword of an import-declaration and constructs it.

BNF example:

@
⟨import-declaration⟩ ::= ... ⟨module-name⟩ ⟨attribute⟩
@
-}
importSentenceRemainderParser :: Parser ParsedSentence
importSentenceRemainderParser =
    SentenceImportSentence
    <$> ( SentenceImport
          <$> moduleNameParser
          <*> attributesParser
        )
{-|'axiomSentenceRemainderParser' parses the part after the starting
'axiom' keyword of an axiom-declaration and constructs it.

BNF example:

@
⟨axiom-declaration⟩ ::= ... ‘{’ ⟨sort-variable-list⟩ ‘}’ ⟨pattern⟩ ⟨attribute⟩
@

Always starts with @{@.
-}
axiomSentenceRemainderParser
    :: (SentenceAxiom SortVariable (Pattern Variable) -> result)
    -> Parser result
axiomSentenceRemainderParser ctor =
  ctor
  <$> ( SentenceAxiom
        <$> inCurlyBracesListParser sortVariableParser
        <*> korePatternParser
        <*> attributesParser
      )

{-|'sortSentenceRemainderParser' parses the part after the starting
@sort@ keyword of a sort-declaration and constructs it.

BNF example:

@
⟨sort-declaration⟩ ::= ... ‘{’ ⟨sort-variable-list⟩ ‘}’ ⟨object-sort⟩ ⟨attribute⟩
@

Always starts with @{@.
-}
sortSentenceRemainderParser :: Parser ParsedSentenceSort
sortSentenceRemainderParser =
    SentenceSort
    <$> idParser
    <*> inCurlyBracesListParser sortVariableParser
    <*> attributesParser

{-|'hookedSymbolSentenceRemainderParser' parses the part after the starting
@hooked-symbol@ keyword of an hook-declaration as a 'SentenceSymbol' and
constructs the corresponding 'SentenceHook'.
-}
hookedSymbolSentenceRemainderParser :: Parser ParsedSentence
hookedSymbolSentenceRemainderParser =
    (<$>)
        (SentenceHookSentence . SentenceHookedSymbol)
        (symbolSentenceRemainderParser symbolParser SentenceSymbol)

{-|'hookedSortSentenceRemainderParser' parses the part after the starting
'hooked-sort@ keyword of a sort-declaration as a 'SentenceSort' and constructs
the corresponding 'SentenceHook'.
-}
hookedSortSentenceRemainderParser :: Parser ParsedSentence
hookedSortSentenceRemainderParser =
    SentenceHookSentence . SentenceHookedSort <$> sortSentenceRemainderParser

leveledPatternParser :: Parser child -> Parser (PatternF Variable child)
leveledPatternParser patternParser = do
    c <- ParserUtils.peekChar'
    case c of
        '\\' -> leveledMLConstructorParser patternParser
        '"'  -> StringLiteralF <$> stringLiteralParser
        '\'' -> CharLiteralF <$> charLiteralParser
        _ -> variableOrTermPatternParser patternParser (c == '#')

purePatternParser :: Parser (Pattern Variable)
purePatternParser = do
    patternHead <- leveledPatternParser childParser
    return $ asPattern patternHead
  where
    childParser = purePatternParser
