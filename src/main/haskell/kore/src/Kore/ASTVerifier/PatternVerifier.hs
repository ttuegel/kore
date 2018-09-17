{-|
Module      : Kore.ASTVerifier.PatternVerifier
Description : Tools for verifying the wellformedness of a Kore 'Pattern'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.PatternVerifier
    ( verifyPattern
    , verifyAliasLeftPattern
    , verifyStandalonePattern
    ) where

import           Control.Monad
                 ( foldM, zipWithM_ )
import           Control.Monad.Except
                 ( catchError, throwError )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text.Prettyprint.Doc
                 ( (<+>) )
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Data.Text.Prettyprint.Doc.Render.String
                 ( renderString )

import           Kore.AST.Common
import           Kore.AST.Error
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.AST.MLPatterns
import           Kore.AST.Sentence
import           Kore.ASTHelpers
import           Kore.ASTUtils.SmartPatterns
import           Kore.ASTVerifier.SortVerifier
import           Kore.ASTVerifier.Verifier
import qualified Kore.Builtin as Builtin
import           Kore.Implicit.ImplicitSorts
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.Resolvers
import           Kore.Unparser
import           Kore.Variables.Free
                 ( freeVariables )

data DeclaredVariables = DeclaredVariables
    { objectDeclaredVariables :: !(Map.Map (Id Object) (Variable Object))
    , metaDeclaredVariables   :: !(Map.Map (Id Meta) (Variable Meta))
    }

emptyDeclaredVariables :: DeclaredVariables
emptyDeclaredVariables = DeclaredVariables
    { objectDeclaredVariables = Map.empty
    , metaDeclaredVariables = Map.empty
    }

data VerifyHelpers level = VerifyHelpers
    { verifyHelpersFindSort
        :: !(Id level -> Verifier (SortDescription level))
    , verifyHelpersLookupAliasDeclaration
        :: !(Id level -> Verifier (KoreSentenceAlias level))
    , verifyHelpersLookupSymbolDeclaration
        :: !(Id level -> Verifier (KoreSentenceSymbol level))
    , verifyHelpersFindDeclaredVariables
        :: !(Id level -> Maybe (Variable level))
    }

metaVerifyHelpers
    :: KoreIndexedModule atts -> DeclaredVariables -> VerifyHelpers Meta
metaVerifyHelpers indexedModule declaredVariables =
    VerifyHelpers
        { verifyHelpersFindSort =
            fmap getIndexedSentence . resolveSort indexedModule
        , verifyHelpersLookupAliasDeclaration =
            fmap getIndexedSentence . resolveAlias indexedModule
        , verifyHelpersLookupSymbolDeclaration =
            fmap getIndexedSentence . resolveSymbol indexedModule
        , verifyHelpersFindDeclaredVariables =
            flip Map.lookup (metaDeclaredVariables declaredVariables)
        }

objectVerifyHelpers
    :: KoreIndexedModule atts -> DeclaredVariables -> VerifyHelpers Object
objectVerifyHelpers indexedModule declaredVariables =
    VerifyHelpers
        { verifyHelpersFindSort =
            fmap getIndexedSentence . resolveSort indexedModule
        , verifyHelpersLookupAliasDeclaration =
            fmap getIndexedSentence . resolveAlias indexedModule
        , verifyHelpersLookupSymbolDeclaration =
            fmap getIndexedSentence . resolveSymbol indexedModule
        , verifyHelpersFindDeclaredVariables =
            flip Map.lookup (objectDeclaredVariables declaredVariables)
        }

addDeclaredVariable
    :: Unified Variable -> DeclaredVariables -> DeclaredVariables
addDeclaredVariable
    (UnifiedMeta variable)
    variables@DeclaredVariables{ metaDeclaredVariables = variablesDict }
  =
    variables
        { metaDeclaredVariables =
            Map.insert (variableName variable) variable variablesDict
        }
addDeclaredVariable
    (UnifiedObject variable)
    variables@DeclaredVariables{ objectDeclaredVariables = variablesDict }
  =
    variables
        { objectDeclaredVariables =
            Map.insert (variableName variable) variable variablesDict
        }

verifyAliasLeftPattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -- ^ The module containing all definitions which are visible in this
    -- pattern.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables which are visible in this pattern.
    -> Maybe UnifiedSort
    -- ^ If present, represents the expected sort of the pattern.
    -> CommonKorePattern
    -> Verifier CommonKorePattern
verifyAliasLeftPattern = verifyPattern
    -- TODO: check that the left pattern is the alias symbol applied to
    -- non-repeating variables

verifyStandalonePattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> CommonKorePattern
    -> Verifier CommonKorePattern
verifyStandalonePattern builtinVerifier indexedModule =
    verifyPattern builtinVerifier indexedModule Set.empty Nothing

{- | Verify that a 'CommonKorePattern' is well-formed Kore pattern.

    The pattern is updated during verification so that:

    - Domain values belonging to builtin domains are decoded from strings to the
      internal representation.

 -}
verifyPattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -- ^ The module containing all definitions which are visible in this
    -- pattern.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables which are visible in this pattern.
    -> Maybe UnifiedSort
    -- ^ If present, represents the expected sort of the pattern.
    -> CommonKorePattern
    -> Verifier CommonKorePattern
verifyPattern
    builtinVerifier
    indexedModule
    sortVariables
    maybeExpectedSort
    unifiedPattern
  = do
    freeVariables1 <- verifyFreeVariables unifiedPattern
    internalVerifyPattern
        builtinVerifier
        indexedModule
        sortVariables
        freeVariables1
        maybeExpectedSort
        unifiedPattern

internalVerifyPattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Maybe UnifiedSort
    -> CommonKorePattern
    -> Verifier CommonKorePattern
internalVerifyPattern
    builtinVerifier
    indexedModule
    sortParamsSet
    declaredVariables
    mUnifiedSort
  =
    applyKorePattern
        (internalVerifyMetaPattern
            builtinVerifier
            indexedModule
            sortParamsSet
            declaredVariables
            mUnifiedSort
        )
        (internalVerifyObjectPattern
            builtinVerifier
            indexedModule
            sortParamsSet
            declaredVariables
            mUnifiedSort
        )

internalVerifyMetaPattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Maybe UnifiedSort
    -> Pattern Meta Variable CommonKorePattern
    -> Verifier CommonKorePattern
internalVerifyMetaPattern
    builtinVerifier
    indexedModule
    sortVariables
    declaredVariables
    maybeExpectedSort
    unverified
  =
    withLocationAndContext unverified (patternNameForContext unverified)
        (do
            (sort, verified) <-
                verifyParameterizedPattern
                    builtinVerifier
                    indexedModule
                    (metaVerifyHelpers indexedModule declaredVariables)
                    sortVariables
                    declaredVariables
                    unverified
            case maybeExpectedSort of
                Just expectedSort ->
                    assertSameSort expectedSort (UnifiedMeta sort)
                Nothing ->
                    return ()
            return (asKorePattern verified)
        )

internalVerifyObjectPattern
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Maybe UnifiedSort
    -> Pattern Object Variable CommonKorePattern
    -> Verifier CommonKorePattern
internalVerifyObjectPattern
    builtinVerifier
    indexedModule
    sortVariables
    declaredVariables
    maybeExpectedSort
    unverified
  =
    withLocationAndContext unverified (patternNameForContext unverified)
        (do
            (sort, verified1) <-
                verifyParameterizedPattern
                    builtinVerifier
                    indexedModule
                    verifyHelpers
                    sortVariables
                    declaredVariables
                    unverified
            case maybeExpectedSort of
                Just expectedSort ->
                    assertSameSort expectedSort (UnifiedObject sort)
                Nothing ->
                    return ()
            verified2 <-
                Builtin.verifyPattern builtinVerifier findSort verified1
            return (asKorePattern verified2)
        )
  where
    findSort = fmap getIndexedSentence . resolveSort indexedModule
    verifyHelpers = objectVerifyHelpers indexedModule declaredVariables

newtype VerifierSortedPattern level =
    VerifierSortedPattern
    { getVerifierSortedPattern
        :: Verifier (Sort level, Pattern level Variable CommonKorePattern)
    }

verifyParameterizedPattern
    :: forall atts level. MetaOrObject level
    => Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Pattern level Variable CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyParameterizedPattern builtinVerifier indexedModule helpers sortParams vars =
    getVerifierSortedPattern
        . applyPatternLeveledFunction patternLeveledFunction
  where
    patternLeveledFunction =
        PatternLeveledFunction
            { patternLeveledFunctionML
            , patternLeveledFunctionMLBinder
            , stringLeveledFunction
            , charLeveledFunction
            , applicationLeveledFunction
            , variableLeveledFunction
            , domainValueLeveledFunction
            }

    patternLeveledFunctionML
        :: MLPatternClass patt level
        => patt level CommonKorePattern
        -> VerifierSortedPattern level
    patternLeveledFunctionML pat =
        VerifierSortedPattern
        $ verifyMLPattern
            builtinVerifier
            indexedModule
            helpers
            sortParams
            vars
            pat

    patternLeveledFunctionMLBinder
        :: MLBinderPatternClass patt
        => patt level Variable CommonKorePattern
        -> VerifierSortedPattern level
    patternLeveledFunctionMLBinder pat =
        VerifierSortedPattern
        $ verifyBinder
            builtinVerifier
            indexedModule
            helpers
            sortParams
            vars
            pat

    stringLeveledFunction
        :: StringLiteral
        -> VerifierSortedPattern Meta
    stringLeveledFunction lit =
        VerifierSortedPattern (verifyStringPattern lit)

    charLeveledFunction
        :: CharLiteral
        -> VerifierSortedPattern Meta
    charLeveledFunction lit =
        VerifierSortedPattern (verifyCharPattern lit)

    applicationLeveledFunction pat =
        VerifierSortedPattern
        $ verifyApplication
            builtinVerifier
            indexedModule
            helpers
            sortParams
            vars
            pat

    variableLeveledFunction pat =
        VerifierSortedPattern
        $ verifyVariableUsage
            indexedModule
            helpers
            sortParams
            vars
            pat

    domainValueLeveledFunction dv =
        VerifierSortedPattern
        $ verifyDomainValue helpers sortParams dv

verifyMLPattern
    :: (MLPatternClass p level, MetaOrObject level)
    => Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> p level CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyMLPattern
    builtinVerifier
    indexedModule
    verifyHelpers
    declaredSortVariables
    declaredVariables
    mlPattern
  = do
    mapM_
        (verifySort
            (verifyHelpersFindSort verifyHelpers)
            declaredSortVariables
        )
        (getPatternSorts mlPattern)
    _ <- verifyPatternsWithSorts
        builtinVerifier
        indexedModule
        declaredSortVariables
        declaredVariables
        operandSorts
        (getPatternChildren mlPattern)
    return (returnSort, mlPatternToPattern mlPattern)
  where
    returnSort = getMLPatternResultSort mlPattern
    operandSorts = getMLPatternOperandSorts mlPattern


verifyPatternsWithSorts
    :: Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> [UnifiedSort]
    -> [CommonKorePattern]
    -> Verifier VerifySuccess
verifyPatternsWithSorts
    builtinVerifier
    indexedModule
    declaredSortVariables
    declaredVariables
    sorts
    operands
  = do
    koreFailWhen (declaredOperandCount /= actualOperandCount)
        (  "Expected "
        ++ show declaredOperandCount
        ++ " operands, but got "
        ++ show actualOperandCount
        ++ "."
        )
    zipWithM_
        (\sort ->
            internalVerifyPattern
                builtinVerifier
                indexedModule
                declaredSortVariables
                declaredVariables
                (Just sort)
        )
        sorts
        operands
    verifySuccess
  where
    declaredOperandCount = length sorts
    actualOperandCount = length operands

verifyApplication
    :: MetaOrObject level
    => Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Application level CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyApplication
    builtinVerifier
    indexedModule
    verifyHelpers
    declaredSortVariables
    declaredVariables
    application
  = do
    applicationSorts <-
        verifySymbolOrAlias
            (applicationSymbolOrAlias application)
            verifyHelpers
            declaredSortVariables
    _ <- verifyPatternsWithSorts
        builtinVerifier
        indexedModule
        declaredSortVariables
        declaredVariables
        (map asUnified (applicationSortsOperands applicationSorts))
        (applicationChildren application)
    return
        ( applicationSortsResult applicationSorts
        , ApplicationPattern application
        )

verifyBinder
    :: (MLBinderPatternClass p, MetaOrObject level)
    => Builtin.PatternVerifier
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> p level Variable CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyBinder
    builtinVerifier
    indexedModule
    verifyHelpers
    declaredSortVariables
    declaredVariables
    binder
  = do
    _ <- verifyVariableDeclaration
        quantifiedVariable indexedModule declaredSortVariables
    _ <- verifySort
        (verifyHelpersFindSort verifyHelpers)
        declaredSortVariables
        binderSort
    _ <- internalVerifyPattern
        builtinVerifier
        indexedModule
        declaredSortVariables
        (addDeclaredVariable (asUnified quantifiedVariable) declaredVariables)
        (Just (asUnified binderSort))
        (getBinderPatternChild binder)
    return (binderSort, mlBinderPatternToPattern binder)
  where
    quantifiedVariable = getBinderPatternVariable binder
    binderSort = getBinderPatternSort binder

verifyVariableUsage
    :: (MetaOrObject level)
    => KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Variable level
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyVariableUsage _ verifyHelpers _ _ variable = do
    declaredVariable <-
        findVariableDeclaration
            (variableName variable) verifyHelpers
    koreFailWithLocationsWhen
        (variableSort variable /= variableSort declaredVariable)
        [ variable, declaredVariable ]
        "The declared sort is different."
    return (variableSort variable, VariablePattern variable)

verifyDomainValue
    :: (MetaOrObject level)
    => VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DomainValue Object (BuiltinDomain CommonKorePattern)
    -> Verifier (Sort Object, Pattern Object Variable CommonKorePattern)
verifyDomainValue
    verifyHelpers
    declaredSortVariables
    domainValue@DomainValue
        { domainValueSort
        , domainValueChild
        }
  =
    case isMetaOrObject verifyHelpers of
        IsMeta -> error "Domain Values are object-only. Should not happen."
        IsObject -> do
            _ <- verifySort
                (verifyHelpersFindSort verifyHelpers)
                declaredSortVariables
                domainValueSort
            case domainValueChild of
                BuiltinDomainPattern (StringLiteral_ _) -> return ()
                _ -> koreFail "Domain value argument must be a literal string."
            return (domainValueSort, DomainValuePattern domainValue)

verifyStringPattern
    :: StringLiteral
    -> Verifier (Sort Meta, Pattern Meta Variable CommonKorePattern)
verifyStringPattern lit = return (charListMetaSort, StringLiteralPattern lit)

verifyCharPattern
    :: CharLiteral
    -> Verifier (Sort Meta, Pattern Meta Variable CommonKorePattern)
verifyCharPattern lit = return (charMetaSort, CharLiteralPattern lit)

verifyVariableDeclaration
    :: MetaOrObject level
    => Variable level
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> Verifier VerifySuccess
verifyVariableDeclaration
    variable indexedModule declaredSortVariables
  = verifyVariableDeclarationUsing
        declaredSortVariables
        (fmap getIndexedSentence . resolveSort indexedModule)
        variable

verifyVariableDeclarationUsing
    :: MetaOrObject level
    => Set.Set UnifiedSortVariable
    -> (Id level -> Verifier (SortDescription level))
    -> Variable level
    -> Verifier VerifySuccess
verifyVariableDeclarationUsing declaredSortVariables f v =
    verifySort f
        declaredSortVariables
        (variableSort v)

findVariableDeclaration
    :: (MetaOrObject level)
    => Id level
    -> VerifyHelpers level
    -> Verifier (Variable level)
findVariableDeclaration variableId verifyHelpers =
    case findVariables variableId of
        Nothing ->
            koreFailWithLocations
                [variableId]
                ("Unquantified variable: '" ++ getId variableId ++ "'.")
        Just variable -> return variable
  where
    findVariables = verifyHelpersFindDeclaredVariables verifyHelpers

verifySymbolOrAlias
    :: MetaOrObject level
    => SymbolOrAlias level
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> Verifier (ApplicationSorts level)
verifySymbolOrAlias symbolOrAlias verifyHelpers declaredSortVariables =
    do
        maybeSentenceSymbol <- try (symbolLookup applicationId)
        maybeSentenceAlias <- try (aliasLookup applicationId)
        case (maybeSentenceSymbol, maybeSentenceAlias) of
            (Right sentenceSymbol, Left _) ->
                applicationSortsFromSymbolOrAliasSentence
                    symbolOrAlias
                    sentenceSymbol
                    verifyHelpers
                    declaredSortVariables
            (Left _, Right sentenceAlias) ->
                applicationSortsFromSymbolOrAliasSentence
                    symbolOrAlias
                    sentenceAlias
                    verifyHelpers
                    declaredSortVariables
            (Left err, Left _) -> throwError err
            (Right _, Right _) -> error
                "The (Right, Right) match should be caught by the unique names check."
  where
    applicationId = symbolOrAliasConstructor symbolOrAlias
    symbolLookup = verifyHelpersLookupSymbolDeclaration verifyHelpers
    aliasLookup = verifyHelpersLookupAliasDeclaration verifyHelpers
    try :: Verifier a -> Verifier (Either (Error VerifyError) a)
    try f = catchError (Right <$> f) (return . Left)

applicationSortsFromSymbolOrAliasSentence
    :: (MetaOrObject level, SentenceSymbolOrAlias sa)
    => SymbolOrAlias level
    -> sa level pat variable
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> Verifier (ApplicationSorts level)
applicationSortsFromSymbolOrAliasSentence
    symbolOrAlias sentence verifyHelpers declaredSortVariables
  = do
    mapM_
        ( verifySort
            (verifyHelpersFindSort verifyHelpers)
            declaredSortVariables
        )
        (symbolOrAliasParams symbolOrAlias)
    symbolOrAliasSorts (symbolOrAliasParams symbolOrAlias) sentence

{- | Produce an error message if the expected and actual sort differ.

    @assertSameSort@ is purely an assertion; it does no work beyond checking the
    equality of the provided sorts.

 -}
assertSameSort
    :: Unified Sort
    -- ^ expected sort
    -> Unified Sort
    -- ^ actual sort
    -> Verifier ()
assertSameSort (UnifiedObject expectedSort) (UnifiedObject actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= actualSort)
        [expectedSort, actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedMeta expectedSort) (UnifiedMeta actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= actualSort)
        [expectedSort, actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedMeta expectedSort) (UnifiedObject actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= patternMetaSort)
        [asUnified expectedSort, asUnified actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting meta sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got object sort"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedObject expectedSort) (UnifiedMeta actualSort) =
    koreFailWithLocationsWhen
        (actualSort /= patternMetaSort)
        [asUnified expectedSort, asUnified actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting object sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got meta sort"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )

verifyFreeVariables
    :: CommonKorePattern -> Verifier DeclaredVariables
verifyFreeVariables unifiedPattern =
    foldM
        addFreeVariable
        emptyDeclaredVariables
        (Set.toList (freeVariables unifiedPattern))

addFreeVariable
    :: DeclaredVariables
    -> Unified Variable
    -> Verifier DeclaredVariables
addFreeVariable
    vars@DeclaredVariables { metaDeclaredVariables = metaVars }
    (UnifiedMeta v)
  = do
    _ <- checkVariable v metaVars
    return vars
        { metaDeclaredVariables = Map.insert (variableName v) v metaVars }
addFreeVariable
    vars@DeclaredVariables { objectDeclaredVariables = objectVars }
    (UnifiedObject v)
  = do
    _ <- checkVariable v objectVars
    return vars
        { objectDeclaredVariables = Map.insert (variableName v) v objectVars }

checkVariable
    :: Variable a
    -> Map.Map (Id a) (Variable a)
    -> Verifier VerifySuccess
checkVariable var vars =
    case Map.lookup (variableName var) vars of
        Nothing -> verifySuccess
        Just v ->
            koreFailWithLocations
                [v, var]
                ( (renderString . Pretty.layoutCompact)
                  ("Inconsistent free variable usage:"
                     <+> unparse v
                     <+> "and"
                     <+> unparse var
                     <> Pretty.dot
                  )
                )

patternNameForContext :: Pattern level Variable p -> String
patternNameForContext (AndPattern _) = "\\and"
patternNameForContext (ApplicationPattern application) =
    "symbol or alias '"
    ++ getId (symbolOrAliasConstructor (applicationSymbolOrAlias application))
    ++ "'"
patternNameForContext (BottomPattern _) = "\\bottom"
patternNameForContext (CeilPattern _) = "\\ceil"
patternNameForContext (DomainValuePattern _) = "\\dv"
patternNameForContext (EqualsPattern _) = "\\equals"
patternNameForContext (ExistsPattern exists) =
    "\\exists '"
    ++ variableNameForContext (existsVariable exists)
    ++ "'"
patternNameForContext (FloorPattern _) = "\\floor"
patternNameForContext (ForallPattern forall) =
    "\\forall '"
    ++ variableNameForContext (forallVariable forall)
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
variableNameForContext variable = getId (variableName variable)
