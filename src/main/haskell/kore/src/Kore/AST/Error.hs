{-|
Module      : Kore.AST.Error
Description : Extensions for errors related to the AST.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.AST.Error
    ( koreFailWithLocations
    , koreFailWithLocationsWhen
    , withLocationAndContext
    , withLocationsContext
    , withSentenceAliasContext
    , withSentenceAxiomContext
    , withSentenceHookContext
    , withSentenceImportContext
    , withSentenceSortContext
    , withSentenceSymbolContext
    , withSentenceContext
    ) where

import Data.List
       ( intercalate )

import Kore.AST.AstWithLocation
import Kore.AST.Common
import Kore.AST.Kore
import Kore.AST.Sentence
import Kore.Error

{-|'koreFailWithLocations' produces an error result with a context containing
the provided locations. -}
koreFailWithLocations
    :: AstWithLocation astWithLocation
    => [astWithLocation] -> String -> Either (Error a) b
koreFailWithLocations locations errorMessage =
    withLocationsContext locations (koreFail errorMessage)

{-|'koreFailWithLocationsWhen' produces an error result with a context
containing the provided locations whenever the provided flag is true.
-}
koreFailWithLocationsWhen
    :: AstWithLocation astWithLocation
    => Bool -> [astWithLocation] -> String -> Either (Error a) ()
koreFailWithLocationsWhen condition locations errorMessage =
    withLocationsContext locations (koreFailWhen condition errorMessage)

{-|'withLocationsContext' prepends the given location to the error context
whenever the given action fails.
-}
withLocationsContext
    :: AstWithLocation astWithLocation
    => [astWithLocation] -> Either (Error a) result -> Either (Error a) result
withLocationsContext locations =
    withContext
        (  "("
        ++ intercalate ", " (map prettyPrintLocationFromAst locations)
        ++ ")"
        )

{-|'withLocationsContext' prepends the given message, associated with the
location, to the error context whenever the given action fails.
-}
withLocationAndContext
    :: (AstWithLocation astWithLocation, MonadError (Error e) m)
    => astWithLocation
    -> String
    -> m result
    -> m result
withLocationAndContext location message =
    withContext (message ++ " (" ++ prettyPrintLocationFromAst location ++ ")")

withSentenceSymbolContext
    :: MonadError (Error e) m
    => KoreSentenceSymbol level
    -> m a
    -> m a
withSentenceSymbolContext
    SentenceSymbol { sentenceSymbolSymbol = Symbol { symbolConstructor } }
  =
    withLocationAndContext symbolConstructor
        ("symbol '" ++ getId symbolConstructor ++ "' declaration")

withSentenceAliasContext
    :: KoreSentenceAlias level
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceAliasContext
    SentenceAlias { sentenceAliasAlias = Alias { aliasConstructor } }
  =
    withLocationAndContext aliasConstructor
        ("alias '" ++ getId aliasConstructor ++ "' declaration")

withSentenceAxiomContext
    :: KoreSentenceAxiom
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceAxiomContext _ = withContext "axiom declaration"

withSentenceSortContext
    :: KoreSentenceSort level
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceSortContext
    SentenceSort { sentenceSortName }
  =
    withLocationAndContext sentenceSortName
        ("sort '" ++ getId sentenceSortName ++ "' declaration")

withSentenceHookContext
    :: KoreSentenceHook
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceHookContext =
    \case
        SentenceHookedSort SentenceSort { sentenceSortName }->
            withLocationAndContext sentenceSortName
                ("hooked-sort '" ++ getId sentenceSortName ++ "' declaration")
        SentenceHookedSymbol SentenceSymbol
            { sentenceSymbolSymbol = Symbol { symbolConstructor } }
          ->
            withLocationAndContext symbolConstructor
                ("hooked-symbol '" ++ getId symbolConstructor ++ "' declaration")

withSentenceImportContext
    :: KoreSentenceImport
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceImportContext _ = \go -> go

withSentenceContext
    :: Sentence level UnifiedSortVariable UnifiedPattern Variable
    -> Either (Error e) a
    -> Either (Error e) a
withSentenceContext =
    \case
        SentenceAliasSentence s -> withSentenceAliasContext s
        SentenceAxiomSentence s -> withSentenceAxiomContext s
        SentenceHookSentence s -> withSentenceHookContext s
        SentenceImportSentence s -> withSentenceImportContext s
        SentenceSortSentence s -> withSentenceSortContext s
        SentenceSymbolSentence s -> withSentenceSymbolContext s
