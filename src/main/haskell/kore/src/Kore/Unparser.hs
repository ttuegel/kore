{-|
Module      : Kore.Unparser
Description : Render abstract to concrete Kore syntax
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Unparser
    ( unparseToString
    , layoutPrettyUnbounded
    , Unparser
    , runUnparser
    , unparseDefinition
    , unparseModule
    , unparseKorePattern
    , unparseSentence
    , unparsePurePattern
    , unparseVariable
    , unparseSort
    ) where

import           Control.Monad
                 ( (>=>) )
import           Control.Monad.Except
                 ( ExceptT, runExceptT )
import qualified Control.Monad.Except as Except
import           Control.Monad.Reader
                 ( Reader, runReader )
import qualified Control.Monad.Reader as Reader
import           Data.Functor
                 ( ($>) )
import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.Maybe
                 ( catMaybes )
import           Data.String
                 ( IsString (fromString) )
import           Data.Text.Prettyprint.Doc hiding
                 ( list )
import           Data.Text.Prettyprint.Doc.Render.String
                 ( renderString )

import           Kore.AST.Common
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
import           Kore.AST.Sentence
import qualified Kore.Builtin.Map as Builtin.Map
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
                 ( KoreIndexedModule )
import           Kore.Parser.CString
                 ( escapeCString )

data UnparseError

type Unparser = ExceptT (Error UnparseError) (Reader (KoreIndexedModule ()))

runUnparser
    :: Unparser a
    -> KoreIndexedModule attrs
    -> Either (Error UnparseError) a
runUnparser unparser indexedModule =
    runReader (runExceptT unparser) (indexedModule $> ())

-- | Serialize a 'Doc' to 'String'.
unparseToString :: Doc ann -> String
unparseToString = renderString . layoutPretty defaultLayoutOptions

-- | Render a 'Doc ann' with indentation and without extra line breaks.
layoutPrettyUnbounded :: Doc ann -> SimpleDocStream ann
layoutPrettyUnbounded = layoutPretty LayoutOptions { layoutPageWidth = Unbounded }

unparseId :: Id level -> Doc ann
unparseId = fromString . getId

unparseStringLiteral :: StringLiteral -> Unparser (Doc ann)
unparseStringLiteral =
    return . dquotes . fromString . escapeCString . getStringLiteral

unparseCharLiteral :: CharLiteral -> Unparser (Doc ann)
unparseCharLiteral =
    return . squotes . fromString . escapeCString . (: []) . getCharLiteral

unparseSymbolOrAlias :: SymbolOrAlias level -> Doc ann
unparseSymbolOrAlias
    SymbolOrAlias { symbolOrAliasConstructor , symbolOrAliasParams }
  =
    let
        constructor = unparseId symbolOrAliasConstructor
        sorts = unparseSorts symbolOrAliasParams
    in
        constructor <> parameters sorts

unparseSymbol :: Symbol level -> Doc ann
unparseSymbol
    Symbol { symbolConstructor, symbolParams }
  =
    let
        constructor = unparseId symbolConstructor
        params = unparseSortVariables symbolParams
    in
        constructor <> parameters params

unparseAlias :: Alias level -> Doc ann
unparseAlias
    Alias { aliasConstructor, aliasParams }
  =
    let
        constructor = unparseId aliasConstructor
        params = unparseSortVariables aliasParams
    in
        constructor <> parameters params

unparseSortVariable :: SortVariable level -> Doc ann
unparseSortVariable SortVariable { getSortVariable } = unparseId getSortVariable

unparseSortVariables :: [SortVariable level] -> [Doc ann]
unparseSortVariables = map unparseSortVariable

unparseSortActual :: SortActual level -> Doc ann
unparseSortActual
    SortActual { sortActualName, sortActualSorts }
  =
    let
        constructor = unparseId sortActualName
        sorts = unparseSorts sortActualSorts
    in
        constructor <> parameters sorts

unparseSort :: Sort level -> Doc ann
unparseSort =
    \case
        SortVariableSort sortVariable -> unparseSortVariable sortVariable
        SortActualSort sortActual -> unparseSortActual sortActual

unparseSorts :: [Sort level] -> [Doc ann]
unparseSorts = map unparseSort

unparseVariable :: Variable level -> Doc ann
unparseVariable
    Variable { variableName, variableSort }
  =
    let
        name = unparseId variableName
        sort = unparseSort variableSort
    in
        name <> colon <> sort

unparseAnd :: And level (Doc ann) -> Unparser (Doc ann)
unparseAnd
    And { andSort, andFirst, andSecond }
  = do
    let
        sorts = unparseSorts [andSort]
        children = [andFirst, andSecond]
    return ("\\and" <> parameters sorts <> arguments children)

unparseApplication :: Application level (Doc ann) -> Unparser (Doc ann)
unparseApplication
    Application { applicationSymbolOrAlias, applicationChildren }
  = do
    let symbolOrAlias = unparseSymbolOrAlias applicationSymbolOrAlias
    return (symbolOrAlias <> arguments applicationChildren)

unparseBottom :: Bottom level child -> Unparser (Doc ann)
unparseBottom Bottom { bottomSort }
  = do
    let sorts = unparseSorts [bottomSort]
    return ("\\bottom" <> parameters sorts <> arguments [])

unparseCeil :: Ceil level (Doc ann) -> Unparser (Doc ann)
unparseCeil
    Ceil { ceilOperandSort, ceilResultSort, ceilChild }
  = do
    let sorts = unparseSorts [ceilOperandSort, ceilResultSort]
    return ("\\ceil" <> parameters sorts <> arguments [ceilChild])

unparseDomainValue
    :: DomainValue level (Doc ann)
    -> Unparser (Doc ann)
unparseDomainValue
    DomainValue { domainValueSort, domainValueChild }
  = do
    let
        sorts = unparseSorts [domainValueSort]
        children = [ domainValueChild ]
    return ("\\dv" <> parameters sorts <> arguments children)

unparseBuiltinDomainValue
    :: DomainValue Object (BuiltinDomain (CommonPurePattern Meta))
    -> Unparser (Doc ann)
unparseBuiltinDomainValue
    DomainValue { domainValueSort, domainValueChild }
  = do
    builtin <-
        unparseCommonBuiltinDomain
            domainValueSort
            domainValueChild
    unparseDomainValue
        DomainValue { domainValueSort, domainValueChild = builtin }

unparseCommonBuiltinDomain
    :: Sort Object
    -> BuiltinDomain (CommonPurePattern Meta)
    -> Unparser (Doc ann)
unparseCommonBuiltinDomain sort =
    traverse unparsePurePattern >=> unparseBuiltinDomain sort

unparseBuiltinDomain
    :: Sort Object
    -> BuiltinDomain (Doc ann)
    -> Unparser (Doc ann)
unparseBuiltinDomain resultSort =
    \case
        BuiltinDomainPattern child -> return child
        BuiltinDomainMap _map -> do
            indexedModule <- Reader.ask
            asPattern <- Except.liftEither $ Builtin.Map.asPattern indexedModule resultSort
            unparsePurePattern (asPattern _map)

unparseEquals :: Equals level (Doc ann) -> Unparser (Doc ann)
unparseEquals
    Equals
        { equalsOperandSort
        , equalsResultSort
        , equalsFirst
        , equalsSecond
        }
  = do
    let
        sorts = unparseSorts [equalsOperandSort, equalsResultSort]
        children = [equalsFirst, equalsSecond]
    return ("\\equals" <> parameters sorts <> arguments children)

unparseExists :: Exists level Variable (Doc ann) -> Unparser (Doc ann)
unparseExists
    Exists { existsSort, existsVariable, existsChild }
  = do
    let
        sorts = unparseSorts [existsSort]
        variable = unparseVariable existsVariable
        children = [variable, existsChild]
    return ("\\exists" <> parameters sorts <> arguments children)

unparseFloor :: Floor level (Doc ann) -> Unparser (Doc ann)
unparseFloor
    Floor { floorOperandSort, floorResultSort, floorChild }
  = do
    let
        sorts = unparseSorts [floorOperandSort, floorResultSort]
        children = [floorChild]
    return ("\\floor" <> parameters sorts <> arguments children)

unparseForall :: Forall level Variable (Doc ann) -> Unparser (Doc ann)
unparseForall
    Forall { forallSort, forallVariable, forallChild }
  = do
    let
        sorts = unparseSorts [forallSort]
        variable = unparseVariable forallVariable
        children = [variable, forallChild]
    return ("\\forall" <> parameters sorts <> arguments children)

unparseIff :: Iff level (Doc ann) -> Unparser (Doc ann)
unparseIff
    Iff { iffSort, iffFirst, iffSecond }
  = do
    let
        sorts = unparseSorts [iffSort]
        children = [iffFirst, iffSecond]
    return ("\\iff" <> parameters sorts <> arguments children)

unparseImplies :: Implies level (Doc ann) -> Unparser (Doc ann)
unparseImplies
    Implies { impliesSort, impliesFirst, impliesSecond }
  = do
    let
        sorts = unparseSorts [impliesSort]
        children = [impliesFirst, impliesSecond]
    return ("\\implies" <> parameters sorts <> arguments children)

unparseIn :: In level (Doc ann) -> Unparser (Doc ann)
unparseIn
    In
        { inOperandSort
        , inResultSort
        , inContainedChild
        , inContainingChild
        }
  = do
    let
        sorts = unparseSorts [inOperandSort, inResultSort]
        children = [inContainedChild, inContainingChild]
    return ("\\in" <> parameters sorts <> arguments children)

unparseNext :: Next level (Doc ann) -> Unparser (Doc ann)
unparseNext Next { nextSort, nextChild }
  = do
    let
        sorts = unparseSorts [nextSort]
        children = [nextChild]
    return ("\\next" <> parameters sorts <> arguments children)

unparseNot :: Not level (Doc ann) -> Unparser (Doc ann)
unparseNot Not { notSort, notChild }
  = do
    let
        sorts = unparseSorts [notSort]
        children = [notChild]
    return ("\\not" <> parameters sorts <> arguments children)

unparseOr :: Or level (Doc ann) -> Unparser (Doc ann)
unparseOr Or { orSort, orFirst, orSecond }
  = do
    let
        sorts = unparseSorts [orSort]
        children = [orFirst, orSecond]
    return ("\\or" <> parameters sorts <> arguments children)

unparseRewrites :: Rewrites level (Doc ann) -> Unparser (Doc ann)
unparseRewrites
    Rewrites { rewritesSort, rewritesFirst, rewritesSecond }
  = do
    let
        sorts = unparseSorts [rewritesSort]
        children = [rewritesFirst, rewritesSecond]
    return ("\\rewrites" <> parameters sorts <> arguments children)

unparseTop :: Top level child -> Unparser (Doc ann)
unparseTop Top { topSort }
  = do
    let sorts = unparseSorts [topSort]
    return ("\\top" <> parameters sorts <> arguments [])

unparsePattern :: Pattern level Variable (Doc ann) -> Unparser (Doc ann)
unparsePattern =
    \case
        AndPattern p           -> unparseAnd p
        ApplicationPattern p   -> unparseApplication p
        BottomPattern p        -> unparseBottom p
        CeilPattern p          -> unparseCeil p
        DomainValuePattern p   -> unparseBuiltinDomainValue p
        EqualsPattern p        -> unparseEquals p
        ExistsPattern p        -> unparseExists p
        FloorPattern p         -> unparseFloor p
        ForallPattern p        -> unparseForall p
        IffPattern p           -> unparseIff p
        ImpliesPattern p       -> unparseImplies p
        InPattern p            -> unparseIn p
        NextPattern p          -> unparseNext p
        NotPattern p           -> unparseNot p
        OrPattern p            -> unparseOr p
        RewritesPattern p      -> unparseRewrites p
        StringLiteralPattern p -> unparseStringLiteral p
        CharLiteralPattern p   -> unparseCharLiteral p
        TopPattern p           -> unparseTop p
        VariablePattern p      -> return (unparseVariable p)

unparsePurePattern
    :: CommonPurePattern level
    -> Unparser (Doc ann)
unparsePurePattern = Functor.Foldable.fold (sequence >=> unparsePattern)

unparseUnifiedPattern
    :: UnifiedPattern Variable (Doc ann)
    -> Unparser (Doc ann)
unparseUnifiedPattern =
    \case
        UnifiedMetaPattern pat -> unparsePattern pat
        UnifiedObjectPattern pat -> unparsePattern pat

unparseKorePattern
    :: CommonKorePattern
    -> Unparser (Doc ann)
unparseKorePattern = Functor.Foldable.fold (sequence >=> unparseUnifiedPattern)

unparseAttributes :: Attributes -> Unparser (Doc ann)
unparseAttributes
    Attributes { getAttributes }
  = do
    attrs <- traverse unparseKorePattern getAttributes
    return (attributes attrs)

unparseAliasPattern :: Pattern level Variable CommonKorePattern -> Unparser (Doc ann)
unparseAliasPattern aliasPattern =
    unparsePattern =<< traverse unparseKorePattern aliasPattern

unparseSentenceAlias :: KoreSentenceAlias level -> Unparser (Doc ann)
unparseSentenceAlias
    SentenceAlias
        { sentenceAliasAlias
        , sentenceAliasSorts
        , sentenceAliasResultSort
        , sentenceAliasLeftPattern
        , sentenceAliasRightPattern
        , sentenceAliasAttributes
        }
  = do
    let
        alias = unparseAlias sentenceAliasAlias
        sorts = unparseSorts sentenceAliasSorts
        resultSort = unparseSort sentenceAliasResultSort
    leftPattern <- unparseAliasPattern sentenceAliasLeftPattern
    rightPattern <- unparseAliasPattern sentenceAliasRightPattern
    attrs <- unparseAttributes sentenceAliasAttributes
    (return . fillSep)
        [ "alias", alias <> arguments sorts, ":", resultSort
        , "where" , leftPattern, ":=", rightPattern
        , attrs
        ]

unparseSentenceSymbol :: KoreSentenceSymbol level -> Unparser (Doc ann)
unparseSentenceSymbol
    SentenceSymbol
        { sentenceSymbolSymbol
        , sentenceSymbolSorts
        , sentenceSymbolResultSort
        , sentenceSymbolAttributes
        }
  = do
    let
        symbol = unparseSymbol sentenceSymbolSymbol
        sorts = unparseSorts sentenceSymbolSorts
        resultSort = unparseSort sentenceSymbolResultSort
    attrs <- unparseAttributes sentenceSymbolAttributes
    (return . fillSep)
        [ "symbol", symbol <> parameters sorts, ":", resultSort
        , attrs
        ]

unparseModuleName :: ModuleName -> Unparser (Doc ann)
unparseModuleName ModuleName { getModuleName } =
    return (fromString getModuleName)

unparseSentenceImport :: KoreSentenceImport -> Unparser (Doc ann)
unparseSentenceImport
    SentenceImport { sentenceImportModuleName, sentenceImportAttributes }
  = do
    name <- unparseModuleName sentenceImportModuleName
    attrs <- unparseAttributes sentenceImportAttributes
    (return . fillSep) [ "import", name, attrs ]

unparseSentenceSort :: KoreSentenceSort level -> Unparser (Doc ann)
unparseSentenceSort
    SentenceSort
        { sentenceSortName
        , sentenceSortParameters
        , sentenceSortAttributes
        }
  = do
    let
        name = unparseId sentenceSortName
        sorts = unparseSortVariables sentenceSortParameters
    attrs <- unparseAttributes sentenceSortAttributes
    (return . fillSep)
        [ "sort", name <> parameters sorts, attrs ]

unparseSentenceAxiom :: KoreSentenceAxiom -> Unparser (Doc ann)
unparseSentenceAxiom
    SentenceAxiom
        { sentenceAxiomParameters
        , sentenceAxiomPattern
        , sentenceAxiomAttributes
        }
  = do
    let sorts = unparseUnifiedSortVariables sentenceAxiomParameters
    axiom <- unparseKorePattern sentenceAxiomPattern
    attrs <- unparseAttributes sentenceAxiomAttributes
    (return . fillSep)
        [ "axiom", parameters sorts, axiom, attrs ]

unparseUnifiedSortVariable :: UnifiedSortVariable -> Doc ann
unparseUnifiedSortVariable =
    \case
        UnifiedMeta sv -> unparseSortVariable sv
        UnifiedObject sv -> unparseSortVariable sv

unparseUnifiedSortVariables :: [UnifiedSortVariable] -> [Doc ann]
unparseUnifiedSortVariables = map unparseUnifiedSortVariable

unparseSentenceHook :: KoreSentenceHook -> Unparser (Doc ann)
unparseSentenceHook =
    \case
        SentenceHookedSort s -> do
            sentence <- unparseSentenceSort s
            return ("hooked-" <> sentence)
        SentenceHookedSymbol s -> do
            sentence <- unparseSentenceSymbol s
            return ("hooked-" <> sentence)

unparseSentence :: KoreSentence -> Unparser (Doc ann)
unparseSentence =
    \case
        UnifiedMetaSentence s -> unparseSentence0 s
        UnifiedObjectSentence s -> unparseSentence0 s
  where
    unparseSentence0
        :: Sentence level UnifiedSortVariable UnifiedPattern Variable
        -> Unparser (Doc ann)
    unparseSentence0 sentence =
        case sentence of
            SentenceAliasSentence s -> unparseSentenceAlias s
            SentenceSymbolSentence s -> unparseSentenceSymbol s
            SentenceImportSentence s -> unparseSentenceImport s
            SentenceAxiomSentence s -> unparseSentenceAxiom s
            SentenceSortSentence s -> unparseSentenceSort s
            SentenceHookSentence s -> unparseSentenceHook s


unparseModule :: KoreModule -> Unparser (Doc ann)
unparseModule
    Module { moduleName, moduleSentences, moduleAttributes }
  = do
    name <- unparseModuleName moduleName
    sentences <- traverse unparseSentence moduleSentences
    attrs <- unparseAttributes moduleAttributes
    (return . vsep . catMaybes)
        [ Just ("module" <+> name)
        , case sentences of
            [] -> Nothing
            _ -> Just $ (indent 4 . vsep) sentences
        , Just "endmodule"
        , Just attrs
        ]

unparseDefinition :: KoreDefinition -> Unparser (Doc ann)
unparseDefinition
    Definition { definitionAttributes, definitionModules }
  = do
    attrs <- unparseAttributes definitionAttributes
    modules <- traverse unparseModule definitionModules
    (return . vsep) (attrs : modules)

-- | Print a list of sort parameters.
parameters :: [Doc ann] -> Doc ann
parameters = list lbrace rbrace

-- | Print a list of documents as arguments.
arguments :: [Doc ann] -> Doc ann
arguments = list lparen rparen

-- | Print a list of documents as attributes.
attributes :: [Doc ann] -> Doc ann
attributes = list lbracket rbracket

-- | Print a list of documents separated by commas in the preferred Kore format.
list
    :: Doc ann  -- ^ opening list delimiter
    -> Doc ann  -- ^ closing list delimiter
    -> [Doc ann]  -- ^ list items
    -> Doc ann
list left right =
    \case
        [] -> left <> right
        xs -> (group . (<> close) . nest 4 . (open <>) . vsep . punctuate between) xs
  where
    open = left <> line'
    close = line' <> right
    between = comma
