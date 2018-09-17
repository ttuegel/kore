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
    , withCommonKorePatternContext
    , withPatternContext
    , withSentenceAliasContext
    , withSentenceAxiomContext
    , withSentenceHookContext
    , withSentenceImportContext
    , withSentenceSortContext
    , withSentenceSymbolContext
    , withSentenceContext
    ) where

import Control.Monad.Except
       ( MonadError )
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
    :: (AstWithLocation astWithLocation, MonadError (Error a) m)
    => [astWithLocation] -> String -> m b
koreFailWithLocations locations errorMessage =
    withLocationsContext locations (koreFail errorMessage)

{-|'koreFailWithLocationsWhen' produces an error result with a context
containing the provided locations whenever the provided flag is true.
-}
koreFailWithLocationsWhen
    :: (AstWithLocation astWithLocation, MonadError (Error a) m)
    => Bool -> [astWithLocation] -> String -> m ()
koreFailWithLocationsWhen condition locations errorMessage =
    withLocationsContext locations (koreFailWhen condition errorMessage)

{-|'withLocationsContext' prepends the given location to the error context
whenever the given action fails.
-}
withLocationsContext
    :: (AstWithLocation astWithLocation, MonadError (Error a) m)
    => [astWithLocation] -> m result -> m result
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

withCommonKorePatternContext
    :: MonadError (Error e) m
    => CommonKorePattern -> m result -> m result
withCommonKorePatternContext pat go =
    applyKorePattern
        (\p -> withPatternContext p go)
        (\p -> withPatternContext p go)
        pat

withPatternContext
    :: MonadError (Error e) m
    => Pattern level Variable child -> m result -> m result
withPatternContext pat =
    withLocationAndContext pat (patternNameForContext pat)
  where
    patternNameForContext :: Pattern level Variable p -> String
    patternNameForContext (AndPattern _) = "\\and"
    patternNameForContext (ApplicationPattern application) =
        let Application { applicationSymbolOrAlias } = application
            SymbolOrAlias { symbolOrAliasConstructor } = applicationSymbolOrAlias
        in
            "symbol or alias '"
            ++ getId symbolOrAliasConstructor
            ++ "'"
    patternNameForContext (BottomPattern _) = "\\bottom"
    patternNameForContext (CeilPattern _) = "\\ceil"
    patternNameForContext (DomainValuePattern _) = "\\dv"
    patternNameForContext (EqualsPattern _) = "\\equals"
    patternNameForContext (ExistsPattern exists) =
        let Exists { existsVariable } = exists
        in
            "\\exists '"
            ++ variableNameForContext existsVariable
            ++ "'"
    patternNameForContext (FloorPattern _) = "\\floor"
    patternNameForContext (ForallPattern forall) =
        let Forall { forallVariable } = forall
        in
            "\\forall '"
            ++ variableNameForContext forallVariable
            ++ "'"
    patternNameForContext (IffPattern _) = "\\iff"
    patternNameForContext (ImpliesPattern _) = "\\implies"
    patternNameForContext (InPattern _) = "\\in"
    patternNameForContext (NextPattern _) = "\\next"
    patternNameForContext (NotPattern _) = "\\not"
    patternNameForContext (OrPattern _) = "\\or"
    patternNameForContext (RewritesPattern _) = "\\rewrites"
    patternNameForContext (StringLiteralPattern _) = "<string>"
    patternNameForContext (CharLiteralPattern _) = "<char>"
    patternNameForContext (TopPattern _) = "\\top"
    patternNameForContext (VariablePattern variable) =
        "variable '" ++ variableNameForContext variable ++ "'"

    variableNameForContext :: Variable level -> String
    variableNameForContext Variable { variableName } = getId variableName

{- | Identify and locate the given symbol declaration in the error context.
 -}
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

{- | Identify and locate the given alias declaration in the error context.
 -}
withSentenceAliasContext
    :: MonadError (Error e) m
    => KoreSentenceAlias level
    -> m result
    -> m result
withSentenceAliasContext
    SentenceAlias { sentenceAliasAlias = Alias { aliasConstructor } }
  =
    withLocationAndContext aliasConstructor
        ("alias '" ++ getId aliasConstructor ++ "' declaration")

{- | Identify and locate the given axiom declaration in the error context.
 -}
withSentenceAxiomContext
    :: MonadError (Error e) m
    => KoreSentenceAxiom
    -> m result
    -> m result
withSentenceAxiomContext _ = withContext "axiom declaration"

{- | Identify and locate the given sort declaration in the error context.
 -}
withSentenceSortContext
    :: MonadError (Error e) m
    => KoreSentenceSort level
    -> m result
    -> m result
withSentenceSortContext
    SentenceSort { sentenceSortName }
  =
    withLocationAndContext sentenceSortName
        ("sort '" ++ getId sentenceSortName ++ "' declaration")

{- | Identify and locate the given hooked declaration in the error context.
 -}
withSentenceHookContext
    :: MonadError (Error e) m
    => KoreSentenceHook
    -> m result
    -> m result
withSentenceHookContext =
    \case
        SentenceHookedSort SentenceSort { sentenceSortName } ->
            withLocationAndContext sentenceSortName
                ("hooked-sort '" ++ getId sentenceSortName ++ "' declaration")

        SentenceHookedSymbol SentenceSymbol
            { sentenceSymbolSymbol = Symbol { symbolConstructor } } ->
            withLocationAndContext symbolConstructor
                ("hooked-symbol '" ++ getId symbolConstructor ++ "' declaration")

{- | Locate the given import declaration in the error context.
 -}
withSentenceImportContext
    :: MonadError (Error e) m
    => KoreSentenceImport
    -> m result
    -> m result
withSentenceImportContext _ = \go -> go

{- | Identify and  locate the given sentence in the error context.
 -}
withSentenceContext
    :: MonadError (Error e) m
    => Sentence level UnifiedSortVariable UnifiedPattern Variable
    -> m result
    -> m result
withSentenceContext =
    \case
        SentenceAliasSentence s -> withSentenceAliasContext s
        SentenceAxiomSentence s -> withSentenceAxiomContext s
        SentenceHookSentence s -> withSentenceHookContext s
        SentenceImportSentence s -> withSentenceImportContext s
        SentenceSortSentence s -> withSentenceSortContext s
        SentenceSymbolSentence s -> withSentenceSymbolContext s
