{-|
Module      : Kore.ASTVerifier.SentenceVerifier
Description : Tools for verifying the wellformedness of a Kore 'Sentence'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.SentenceVerifier
    ( verifyUniqueNames
    , verifySentences
    ) where

import           Control.Monad
                 ( foldM )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import qualified Data.Text as Text

import           Kore.AST.Error
import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import           Kore.ASTVerifier.PatternVerifier as PatternVerifier
import           Kore.ASTVerifier.SortVerifier
import qualified Kore.Attribute.Parser as Attribute.Parser
import qualified Kore.Builtin as Builtin
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.Resolvers
import           Kore.Parser.Sentence
import qualified Kore.Verified as Verified

{-|'verifyUniqueNames' verifies that names defined in a list of sentences are
unique both within the list and outside, using the provided name set.
-}
verifyUniqueNames
    :: [Sentence Object param pat]
    -> Map.Map Text AstLocation
    -- ^ Names that are already defined.
    -> Either (Error VerifyError) (Map.Map Text AstLocation)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames sentences existingNames =
    foldM verifyUniqueId existingNames definedNames
  where
    definedNames =
        concatMap definedNamesForSentence sentences

data UnparameterizedId = UnparameterizedId
    { unparameterizedIdName     :: String
    , unparameterizedIdLocation :: AstLocation
    }
    deriving (Show)


toUnparameterizedId :: Id -> UnparameterizedId
toUnparameterizedId Id {getId = name, idLocation = location} =
    UnparameterizedId
        { unparameterizedIdName = Text.unpack name
        , unparameterizedIdLocation = location
        }

verifyUniqueId
    :: Map.Map Text AstLocation
    -> UnparameterizedId
    -> Either (Error VerifyError) (Map.Map Text AstLocation)
verifyUniqueId existing (UnparameterizedId name location) =
    case Map.lookup name' existing of
        Just location' ->
            koreFailWithLocations [location, location']
                ("Duplicated name: '" ++ name ++ "'.")
        _ -> Right (Map.insert name' location existing)
  where
    name' = Text.pack name

definedNamesForSentence :: Sentence Object param pat -> [UnparameterizedId]
definedNamesForSentence (SentenceAliasSentence sentenceAlias) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceAlias) ]
definedNamesForSentence (SentenceSymbolSentence sentenceSymbol) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceSymbol) ]
definedNamesForSentence (SentenceImportSentence _) = []
definedNamesForSentence (SentenceAxiomSentence _)  = []
definedNamesForSentence (SentenceClaimSentence _)  = []
definedNamesForSentence (SentenceSortSentence sentenceSort) =
    [ toUnparameterizedId (sentenceSortName sentenceSort) ]
definedNamesForSentence (SentenceHookSentence (SentenceHookedSort sentence)) =
    definedNamesForSentence (SentenceSortSentence sentence)
definedNamesForSentence (SentenceHookSentence (SentenceHookedSymbol sentence)) =
    definedNamesForSentence (SentenceSymbolSentence sentence)

{-|'verifySentences' verifies the welformedness of a list of Kore 'Sentence's.
-}
verifySentences
    :: KoreIndexedModule declAtts axiomAtts
    -- ^ The module containing all definitions which are visible in this
    -- pattern.
    -> AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> [ParsedSentence]
    -> Either (Error VerifyError) [Verified.Sentence]
verifySentences indexedModule attributesVerification builtinVerifiers =
    traverse
        (verifySentence
            builtinVerifiers
            indexedModule
            attributesVerification
        )

verifySentence
    :: Builtin.Verifiers
    -> KoreIndexedModule declAtts axiomAtts
    -> AttributesVerification declAtts axiomAtts
    -> ParsedSentence
    -> Either (Error VerifyError) Verified.Sentence
verifySentence builtinVerifiers indexedModule attributesVerification sentence =
    withSentenceContext sentence verifySentenceWorker
  where
    verifySentenceWorker :: Either (Error VerifyError) Verified.Sentence
    verifySentenceWorker = do
        verified <-
            case sentence of
                SentenceSymbolSentence symbolSentence ->
                    (<$>)
                        SentenceSymbolSentence
                        (verifySymbolSentence
                            indexedModule
                            symbolSentence
                        )
                SentenceAliasSentence aliasSentence ->
                    (<$>)
                        SentenceAliasSentence
                        (verifyAliasSentence
                            builtinVerifiers
                            indexedModule
                            aliasSentence
                        )
                SentenceAxiomSentence axiomSentence ->
                    (<$>)
                        SentenceAxiomSentence
                        (verifyAxiomSentence
                            axiomSentence
                            builtinVerifiers
                            indexedModule
                        )
                SentenceClaimSentence claimSentence ->
                    (<$>)
                        SentenceClaimSentence
                        (verifyAxiomSentence
                            claimSentence
                            builtinVerifiers
                            indexedModule
                        )
                SentenceImportSentence importSentence ->
                    -- Since we have an IndexedModule, we assume that imports
                    -- were already resolved, so there is nothing left to verify
                    -- here.
                    (<$>)
                        SentenceImportSentence
                        (traverse verifyNoPatterns importSentence)
                SentenceSortSentence sortSentence ->
                    (<$>)
                        SentenceSortSentence
                        (verifySortSentence sortSentence)
                SentenceHookSentence hookSentence ->
                    (<$>)
                        SentenceHookSentence
                        (verifyHookSentence
                            builtinVerifiers
                            indexedModule
                            attributesVerification
                            hookSentence
                        )
        verifySentenceAttributes
            attributesVerification
            sentence
        return verified

verifySentenceAttributes
    :: AttributesVerification declAtts axiomAtts
    -> ParsedSentence
    -> Either (Error VerifyError) VerifySuccess
verifySentenceAttributes attributesVerification sentence =
    do
        let attributes = sentenceAttributes sentence
        verifyAttributes attributes attributesVerification
        case sentence of
            SentenceHookSentence _ -> return ()
            _ -> verifyNoHookAttribute attributesVerification attributes
        verifySuccess

verifyHookSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule declAtts axiomAtts
    -> AttributesVerification declAtts axiomAtts
    -> ParsedSentenceHook
    -> Either (Error VerifyError) Verified.SentenceHook
verifyHookSentence
    builtinVerifiers
    indexedModule
    attributesVerification
  =
    \case
        SentenceHookedSort s -> SentenceHookedSort <$> verifyHookedSort s
        SentenceHookedSymbol s -> SentenceHookedSymbol <$> verifyHookedSymbol s
  where
    verifyHookedSort
        sentence@SentenceSort { sentenceSortAttributes }
      = do
        verified <- verifySortSentence sentence
        hook <-
            verifySortHookAttribute
                indexedModule
                attributesVerification
                sentenceSortAttributes
        attrs <-
            Attribute.Parser.liftParser
            $ Attribute.Parser.parseAttributes sentenceSortAttributes
        Builtin.sortDeclVerifier
            builtinVerifiers
            hook
            (makeIndexedModuleAttributesNull indexedModule)
            sentence
            attrs
        return verified

    verifyHookedSymbol
        sentence@SentenceSymbol { sentenceSymbolAttributes }
      = do
        verified <- verifySymbolSentence indexedModule sentence
        hook <-
            verifySymbolHookAttribute
                attributesVerification
                sentenceSymbolAttributes
        Builtin.runSymbolVerifier
            (Builtin.symbolVerifier builtinVerifiers hook)
            findSort
            sentence
        return verified

    findSort = findIndexedSort indexedModule

verifySymbolSentence
    :: KoreIndexedModule declAtts axiomAtts
    -> ParsedSentenceSymbol
    -> Either (Error VerifyError) Verified.SentenceSymbol
verifySymbolSentence indexedModule sentence =
    do
        variables <- buildDeclaredSortVariables sortParams
        mapM_
            (verifySort findSort variables)
            (sentenceSymbolSorts sentence)
        verifySort
            findSort
            variables
            (sentenceSymbolResultSort sentence)
        traverse verifyNoPatterns sentence
  where
    findSort = findIndexedSort indexedModule
    sortParams = (symbolParams . sentenceSymbolSymbol) sentence

verifyAliasSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule declAtts axiomAtts
    -> ParsedSentenceAlias
    -> Either (Error VerifyError) Verified.SentenceAlias
verifyAliasSentence builtinVerifiers indexedModule sentence =
    do
        variables <- buildDeclaredSortVariables sortParams
        mapM_ (verifySort findSort variables) sentenceAliasSorts
        verifySort findSort variables sentenceAliasResultSort
        let context =
                PatternVerifier.Context
                    { builtinDomainValueVerifiers =
                        Builtin.domainValueVerifiers builtinVerifiers
                    , indexedModule =
                        makeIndexedModuleAttributesNull indexedModule
                    , declaredSortVariables = variables
                    , declaredVariables = emptyDeclaredVariables
                    }
        runPatternVerifier context $ do
            (declaredVariables, verifiedLeftPattern) <-
                verifyAliasLeftPattern leftPattern
            verifiedRightPattern <-
                withDeclaredVariables declaredVariables
                $ verifyPattern (Just expectedSort) rightPattern
            return sentence
                { sentenceAliasLeftPattern = verifiedLeftPattern
                , sentenceAliasRightPattern = verifiedRightPattern
                }
  where
    SentenceAlias { sentenceAliasLeftPattern = leftPattern } = sentence
    SentenceAlias { sentenceAliasRightPattern = rightPattern } = sentence
    SentenceAlias { sentenceAliasSorts } = sentence
    SentenceAlias { sentenceAliasResultSort } = sentence
    findSort         = findIndexedSort indexedModule
    sortParams       = (aliasParams . sentenceAliasAlias) sentence
    expectedSort = sentenceAliasResultSort

verifyAxiomSentence
    :: ParsedSentenceAxiom
    -> Builtin.Verifiers
    -> KoreIndexedModule declAtts axiomAtts
    -> Either (Error VerifyError) Verified.SentenceAxiom
verifyAxiomSentence axiom builtinVerifiers indexedModule =
    do
        variables <- buildDeclaredSortVariables $ sentenceAxiomParameters axiom
        let context =
                PatternVerifier.Context
                    { builtinDomainValueVerifiers =
                        Builtin.domainValueVerifiers builtinVerifiers
                    , indexedModule =
                        makeIndexedModuleAttributesNull indexedModule
                    , declaredSortVariables = variables
                    , declaredVariables = emptyDeclaredVariables
                    }
        verifiedAxiomPattern <- runPatternVerifier context $ do
            verifyStandalonePattern Nothing sentenceAxiomPattern
        return axiom { sentenceAxiomPattern = verifiedAxiomPattern }
  where
    SentenceAxiom { sentenceAxiomPattern } = axiom

verifySortSentence
    :: ParsedSentenceSort
    -> Either (Error VerifyError) Verified.SentenceSort
verifySortSentence sentenceSort = do
    _ <- buildDeclaredSortVariables (sentenceSortParameters sentenceSort)
    traverse verifyNoPatterns sentenceSort

buildDeclaredSortVariables
    :: [SortVariable]
    -> Either (Error VerifyError) (Set.Set SortVariable)
buildDeclaredSortVariables [] = Right Set.empty
buildDeclaredSortVariables (unifiedVariable : list) = do
    variables <- buildDeclaredSortVariables list
    koreFailWithLocationsWhen
        (unifiedVariable `Set.member` variables)
        [unifiedVariable]
        (  "Duplicated sort variable: '"
        ++ extractVariableName unifiedVariable
        ++ "'.")
    return (Set.insert unifiedVariable variables)
  where
    extractVariableName variable = getIdForError (getSortVariable variable)
