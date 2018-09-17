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

import           Kore.AST.Common
import           Kore.AST.Error
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.AST.Sentence
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.PatternVerifier
import           Kore.ASTVerifier.SortVerifier
import           Kore.ASTVerifier.Verifier
import qualified Kore.Builtin as Builtin
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.Resolvers

{-|'verifyUniqueNames' verifies that names defined in a list of sentences are
unique both within the list and outside, using the provided name set.
-}
verifyUniqueNames
    :: [KoreSentence]
    -> Map.Map String AstLocation
    -- ^ Names that are already defined.
    -> Verifier (Map.Map String AstLocation)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames sentences existingNames =
    foldM verifyUniqueId existingNames
        (concatMap definedNamesForSentence sentences)

verifyUniqueId
    :: Map.Map String AstLocation
    -> UnparameterizedId
    -> Verifier (Map.Map String AstLocation)
verifyUniqueId existing (UnparameterizedId name location) =
    case Map.lookup name existing of
        Just location' ->
            koreFailWithLocations [location, location']
                ("Duplicated name: '" ++ name ++ "'.")
        _ -> return (Map.insert name location existing)

data UnparameterizedId = UnparameterizedId
    { unparameterizedIdName     :: String
    , unparameterizedIdLocation :: AstLocation
    }
toUnparameterizedId :: Id level -> UnparameterizedId
toUnparameterizedId Id {getId = name, idLocation = location} =
    UnparameterizedId
        { unparameterizedIdName = name
        , unparameterizedIdLocation = location
        }

definedNamesForSentence :: KoreSentence -> [UnparameterizedId]
definedNamesForSentence =
    applyUnifiedSentence
        definedNamesForMetaSentence
        definedNamesForObjectSentence

definedNamesForMetaSentence
    :: Sentence Meta sortParam pat variable -> [UnparameterizedId]
definedNamesForMetaSentence (SentenceAliasSentence sentenceAlias) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceAlias) ]
definedNamesForMetaSentence (SentenceSymbolSentence sentenceSymbol) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceSymbol) ]
definedNamesForMetaSentence (SentenceImportSentence _) = []
definedNamesForMetaSentence (SentenceAxiomSentence _)  = []
definedNamesForMetaSentence (SentenceSortSentence _)   = []

definedNamesForObjectSentence
    :: Sentence Object sortParam pat variable -> [UnparameterizedId]
definedNamesForObjectSentence (SentenceAliasSentence sentenceAlias) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceAlias) ]
definedNamesForObjectSentence (SentenceSymbolSentence sentenceSymbol) =
    [ toUnparameterizedId (getSentenceSymbolOrAliasConstructor sentenceSymbol) ]
definedNamesForObjectSentence (SentenceSortSentence sentenceSort) =
    [ sentenceName
    , UnparameterizedId
        { unparameterizedIdName =
            metaNameForObjectSort (unparameterizedIdName sentenceName)
        , unparameterizedIdLocation =
            AstLocationLifted (unparameterizedIdLocation sentenceName)
        }
    ]
  where
    SentenceSort { sentenceSortName } = sentenceSort
    sentenceName = toUnparameterizedId sentenceSortName
definedNamesForObjectSentence
    (SentenceHookSentence (SentenceHookedSort sentence))
  = definedNamesForObjectSentence (SentenceSortSentence sentence)
definedNamesForObjectSentence
    (SentenceHookSentence (SentenceHookedSymbol sentence))
  = definedNamesForObjectSentence (SentenceSymbolSentence sentence)

{-|'verifySentences' verifies the welformedness of a list of Kore 'Sentence's.
-}
verifySentences
    :: KoreIndexedModule atts
    -- ^ The module containing all definitions which are visible in this
    -- pattern.
    -> AttributesVerification atts
    -> Builtin.Verifiers
    -> [KoreSentence]
    -> Verifier ()
verifySentences
    indexedModule attributesVerification builtinVerifiers sentences
  =
    mapM_
        (verifySentence
            builtinVerifiers
            indexedModule
            attributesVerification
        )
        sentences

verifySentence
    :: Builtin.Verifiers
    -> KoreIndexedModule atts
    -> AttributesVerification atts
    -> KoreSentence
    -> Verifier VerifySuccess
verifySentence builtinVerifiers indexedModule attributesVerification =
    applyUnifiedSentence verifyMeta verifyObject
  where
    verifyMeta sentence =
        verifyMetaSentence builtinVerifiers indexedModule attributesVerification sentence
    verifyObject sentence =
        verifyObjectSentence
            builtinVerifiers
            indexedModule
            attributesVerification
            sentence

verifyMetaSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule atts
    -> AttributesVerification atts
    -> Sentence Meta UnifiedSortVariable UnifiedPattern Variable
    -> Verifier VerifySuccess
verifyMetaSentence
    builtinVerifiers
    indexedModule
    attributesVerification
    sentence
  =
    withSentenceContext sentence verifyMetaSentence0
  where
    verifyMetaSentence0 = do
        _ <- case sentence of
            SentenceSymbolSentence symbolSentence ->
                verifySymbolSentence
                    indexedModule
                    symbolSentence
            SentenceAliasSentence aliasSentence ->
                verifyAliasSentence
                    builtinVerifiers
                    indexedModule
                    aliasSentence
            SentenceAxiomSentence axiomSentence ->
                verifyAxiomSentence
                    builtinVerifiers
                    indexedModule
                    attributesVerification
                    axiomSentence
            SentenceSortSentence sortSentence -> do
                koreFailWhen
                    (sortParams /= [])
                    ("Malformed meta sort '" ++ getId sortId
                        ++ "' with non-empty Parameter sorts.")
                verifySuccess
              where
                sortId     = sentenceSortName sortSentence
                sortParams = sentenceSortParameters sortSentence
            SentenceImportSentence _ ->
                -- Since we have an IndexedModule, we assume that imports were
                -- already resolved, so there is nothing left to verify here.
                verifySuccess
        verifySentenceAttributes
            attributesVerification
            sentence

verifyObjectSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule atts
    -> AttributesVerification atts
    -> Sentence Object UnifiedSortVariable UnifiedPattern Variable
    -> Verifier VerifySuccess
verifyObjectSentence
    builtinVerifiers
    indexedModule
    attributesVerification
    sentence
  =
    withSentenceContext sentence verifyObjectSentence1
  where
    verifyObjectSentence1 = do
        _ <- case sentence of
            SentenceAliasSentence aliasSentence ->
                verifyAliasSentence
                    builtinVerifiers
                    indexedModule
                    aliasSentence
            SentenceSymbolSentence symbolSentence ->
                verifySymbolSentence
                    indexedModule
                    symbolSentence
            SentenceSortSentence sortSentence ->
                verifySortSentence attributesVerification sortSentence
            SentenceHookSentence hookSentence ->
                verifyHookSentence
                    builtinVerifiers
                    indexedModule
                    attributesVerification
                    hookSentence
        verifySentenceAttributes
            attributesVerification
            sentence

verifySentenceAttributes
    :: AttributesVerification atts
    -> Sentence level UnifiedSortVariable UnifiedPattern Variable
    -> Verifier VerifySuccess
verifySentenceAttributes attributesVerification sentence =
    do
        let attributes = sentenceAttributes sentence
        _ <- verifyAttributes attributesVerification attributes
        case sentence of
            SentenceHookSentence _ -> return ()
            _ -> verifyNoHookAttribute attributesVerification attributes
        verifySuccess

verifyHookSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule atts
    -> AttributesVerification atts
    -> SentenceHook Object UnifiedPattern Variable
    -> Verifier VerifySuccess
verifyHookSentence
    builtinVerifiers
    indexedModule
    attributesVerification
  =
    \case
        SentenceHookedSort s -> verifyHookedSort s
        SentenceHookedSymbol s -> verifyHookedSymbol s
  where
    verifyHookedSort
        sentence@SentenceSort { sentenceSortAttributes }
      = do
        _ <- verifySortSentence attributesVerification sentence
        hook <-
            verifyHookAttribute
                indexedModule
                attributesVerification
                sentenceSortAttributes
        _ <- Builtin.sortDeclVerifier builtinVerifiers hook sentence
        verifySuccess

    verifyHookedSymbol
        sentence@SentenceSymbol { sentenceSymbolAttributes }
      = do
        _ <- verifySymbolSentence indexedModule sentence
        hook <-
            verifyHookAttribute
                indexedModule
                attributesVerification
                sentenceSymbolAttributes
        _ <- Builtin.symbolVerifier builtinVerifiers hook findSort sentence
        verifySuccess

    findSort = findIndexedSort indexedModule

verifySymbolSentence
    :: (MetaOrObject level)
    => KoreIndexedModule atts
    -> KoreSentenceSymbol level
    -> Verifier VerifySuccess
verifySymbolSentence
    indexedModule
    SentenceSymbol
        { sentenceSymbolSymbol =
            Symbol { symbolParams }
        , sentenceSymbolSorts
        , sentenceSymbolResultSort
        }
  =
    do
        variables <- buildDeclaredSortVariables symbolParams
        mapM_ (verifySort findSort variables) sentenceSymbolSorts
        _ <- verifySort findSort variables sentenceSymbolResultSort
        verifySuccess
  where
    findSort = findIndexedSort indexedModule

verifyAliasSentence
    :: (MetaOrObject level)
    => Builtin.Verifiers
    -> KoreIndexedModule atts
    -> KoreSentenceAlias level
    -> Verifier VerifySuccess
verifyAliasSentence
    builtinVerifiers
    indexedModule
    SentenceAlias
        { sentenceAliasAlias =
            Alias { aliasParams }
        , sentenceAliasSorts
        , sentenceAliasResultSort
        , sentenceAliasLeftPattern
        , sentenceAliasRightPattern
        }
  =
    do
        variables <- buildDeclaredSortVariables aliasParams
        mapM_
            (verifySort findSort variables)
            sentenceAliasSorts
        _ <- verifySort
            findSort
            variables
            sentenceAliasResultSort
        (leftPatternSort, _) <-
            verifyAliasLeftPattern
                (Builtin.domainVerifier builtinVerifiers)
                indexedModule
                variables
                sentenceAliasLeftPattern
        (rightPatternSort, _) <-
            verifyAliasRightPattern
                (Builtin.domainVerifier builtinVerifiers)
                indexedModule
                variables
                sentenceAliasRightPattern
        koreFailWhen (leftPatternSort /= rightPatternSort)
            "Left and Right sorts do not match"
        verifySuccess
  where
    findSort = findIndexedSort indexedModule

verifyAxiomSentence
    :: Builtin.Verifiers
    -> KoreIndexedModule atts
    -> AttributesVerification atts
    -> KoreSentenceAxiom
    -> Verifier VerifySuccess
verifyAxiomSentence
    builtinVerifiers
    indexedModule
    attributesVerification
    SentenceAxiom
        { sentenceAxiomParameters
        , sentenceAxiomPattern
        , sentenceAxiomAttributes
        }
  =
    do
        variables <- buildDeclaredUnifiedSortVariables sentenceAxiomParameters
        _ <-
            verifyPattern
                (Builtin.domainVerifier builtinVerifiers)
                indexedModule
                variables
                sentenceAxiomPattern
        _ <- verifyAttributes attributesVerification sentenceAxiomAttributes
        verifySuccess

verifySortSentence
    :: AttributesVerification atts
    -> KoreSentenceSort Object
    -> Verifier VerifySuccess
verifySortSentence
    attributesVerification
    SentenceSort
        { sentenceSortParameters
        , sentenceSortAttributes
        }
  =
    do
        _ <- buildDeclaredSortVariables sentenceSortParameters
        _ <- verifyAttributes attributesVerification sentenceSortAttributes
        verifySuccess

buildDeclaredSortVariables
    :: MetaOrObject level
    => [SortVariable level]
    -> Verifier (Set.Set UnifiedSortVariable)
buildDeclaredSortVariables variables =
    buildDeclaredUnifiedSortVariables
        (map asUnified variables)

buildDeclaredUnifiedSortVariables
    :: [UnifiedSortVariable]
    -> Verifier (Set.Set UnifiedSortVariable)
buildDeclaredUnifiedSortVariables [] = return Set.empty
buildDeclaredUnifiedSortVariables (unifiedVariable : list) = do
    variables <- buildDeclaredUnifiedSortVariables list
    koreFailWithLocationsWhen
        (unifiedVariable `Set.member` variables)
        [unifiedVariable]
        (  "Duplicated sort variable: '"
        ++ extractVariableName unifiedVariable
        ++ "'.")
    return (Set.insert unifiedVariable variables)
  where
    extractVariableName (UnifiedObject variable) =
        getId (getSortVariable variable)
    extractVariableName (UnifiedMeta variable) =
        getId (getSortVariable variable)
