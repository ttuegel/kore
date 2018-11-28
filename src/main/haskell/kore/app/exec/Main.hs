module Main (main) where

import           Control.Applicative
                 ( Alternative (..), optional )
import qualified Control.Lens as Lens
import           Data.Function
                 ( (&) )
import qualified Data.Functor.Foldable as Recursive
import           Data.List
                 ( intercalate )
import qualified Data.Map as Map
import           Data.Proxy
                 ( Proxy (..) )
import           Data.Reflection
                 ( give )
import           Data.Semigroup
                 ( (<>) )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc.Render.Text
                 ( hPutDoc, putDoc )
import           Options.Applicative
                 ( InfoMod, Parser, argument, auto, fullDesc, header, help,
                 long, metavar, option, progDesc, readerError, str, strOption,
                 value )
import           System.IO
                 ( IOMode (WriteMode), withFile )

import           Data.Limit
                 ( Limit (..) )
import           Kore.AST.Kore
                 ( CommonKorePattern )
import           Kore.AST.Pure
import           Kore.AST.PureToKore
                 ( patternKoreToPure )
import           Kore.AST.Sentence
import           Kore.ASTUtils.SmartPatterns
import           Kore.ASTVerifier.DefinitionVerifier
                 ( AttributesVerification (DoNotVerifyAttributes),
                 defaultAttributesVerification, verifyAndIndexDefinition )
import           Kore.ASTVerifier.PatternVerifier
                 ( verifyStandalonePattern )
import qualified Kore.Builtin as Builtin
import           Kore.Error
                 ( printError )
import           Kore.Exec
import           Kore.IndexedModule.IndexedModule
                 ( IndexedModule (..), KoreIndexedModule )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), extractMetadataTools )
import           Kore.Parser.Parser
                 ( fromKore, fromKorePattern )
import           Kore.Predicate.Predicate
                 ( makePredicate )
import           Kore.Step.AxiomPatterns
                 ( RewriteRule )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, Predicated (..) )
import           Kore.Step.Pattern
import           Kore.Step.Search
                 ( SearchType (..) )
import qualified Kore.Step.Search as Search
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import           Kore.Step.Step
import           Kore.Step.StepperAttributes
import           Kore.Unparser
                 ( unparse )
import           Kore.Variables.Free
                 ( pureAllVariables )
import           Kore.Variables.Fresh
                 ( freshVariablePrefix )
import qualified SMT

import GlobalMain
       ( MainOptions (..), clockSomething, clockSomethingIO, mainGlobal )


{-
Main module to run kore-exec
TODO: add command line argument tab-completion
-}

data KoreSearchOptions =
    KoreSearchOptions
        { searchFileName :: !FilePath
        -- ^ Name of file containing a pattern to match during execution
        , bound :: !(Limit Natural)
        -- ^ The maximum bound on the number of search matches
        , searchType :: !SearchType
        -- ^ The type of search to perform
        }

parseKoreSearchOptions :: Parser KoreSearchOptions
parseKoreSearchOptions =
    KoreSearchOptions
    <$> strOption
        (  metavar "SEARCH_FILE"
        <> long "search"
        <> help "Kore source file representing pattern to search for.\
                \Needs --module."
        )
    <*> parseBound
    <*> parseSearchType
  where
    parseBound = Limit <$> bound <|> pure Unlimited
    bound =
        option auto
            (  metavar "BOUND"
            <> long "bound"
            <> help "Maximum number of solutions."
            )
    parseSearchType =
        parseSum
            "SEARCH_TYPE"
            "searchType"
            "Search type (selects potential solutions)"
            (map (\s -> (show s, s)) [ ONE, FINAL, STAR, PLUS ])

    parseSum
        :: Eq value
        => String -> String -> String -> [(String,value)] -> Parser value
    parseSum metaName longName helpMsg options =
        option readSum
            (  metavar metaName
            <> long longName
            <> help helpMsg
            )
      where
        readSum = do
            opt <- str
            case lookup opt options of
                Just val -> pure val
                _ ->
                    let
                        unknown = "Unknown " ++  longName ++ " '" ++ opt ++ "'. "
                        known = "Known " ++ longName ++ "s are: " ++
                            intercalate ", " (map fst options) ++ "."
                    in
                        readerError (unknown ++ known)

applyKoreSearchOptions
    :: Maybe KoreSearchOptions
    -> KoreExecOptions
    -> KoreExecOptions
applyKoreSearchOptions koreSearchOptions koreExecOpts =
    case koreSearchOptions of
        Nothing -> koreExecOpts
        Just koreSearchOpts ->
            koreExecOpts
                { koreSearchOptions = Just koreSearchOpts
                , strategy =
                    -- Search relies on exploring the entire space of states.
                    allRewrites
                , stepLimit = min stepLimit searchTypeStepLimit
                }
          where
            KoreSearchOptions { searchType } = koreSearchOpts
            KoreExecOptions { stepLimit } = koreExecOpts
            searchTypeStepLimit =
                case searchType of
                    ONE -> Limit 1
                    _ -> Unlimited

-- | Main options record
data KoreExecOptions = KoreExecOptions
    { definitionFileName  :: !FilePath
    -- ^ Name for a file containing a definition to verify and use for execution
    , patternFileName     :: !FilePath
    -- ^ Name for file containing a pattern to verify and use for execution
    , outputFileName      :: !(Maybe FilePath)
    -- ^ Name for file to contain the output pattern
    , mainModuleName      :: !ModuleName
    -- ^ The name of the main module in the definition
    , smtTimeOut          :: !SMT.TimeOut
    , stepLimit           :: !(Limit Natural)
    , strategy
        :: !([RewriteRule Object] -> Strategy (Prim (RewriteRule Object)))
    , koreSearchOptions   :: !(Maybe KoreSearchOptions)
    }

-- | Command Line Argument Parser
parseKoreExecOptions :: Parser KoreExecOptions
parseKoreExecOptions =
    applyKoreSearchOptions
        <$> optional parseKoreSearchOptions
        <*> parseKoreExecOptions0
  where
    parseKoreExecOptions0 =
        KoreExecOptions
        <$> argument str
            (  metavar "DEFINITION_FILE"
            <> help "Kore definition file to verify and use for execution" )
        <*> strOption
            (  metavar "PATTERN_FILE"
            <> long "pattern"
            <> help "Verify and execute the Kore pattern found in PATTERN_FILE."
            )
        <*> optional
            (strOption
                (  metavar "PATTERN_OUTPUT_FILE"
                <> long "output"
                <> help "Output file to contain final Kore pattern."
                )
            )
        <*> parseMainModuleName
        <*> option readSMTTimeOut
            ( metavar "SMT_TIMEOUT"
            <> long "smt-timeout"
            <> help "Timeout for calls to the SMT solver, in milliseconds"
            <> value defaultTimeOut
            )
        <*> parseStepLimit
        <*> parseStrategy
        <*> pure Nothing
    SMT.Config { timeOut = defaultTimeOut } = SMT.defaultConfig
    readSMTTimeOut = do
        i <- auto
        if i <= 0
            then readerError "smt-timeout must be a positive integer."
            else return $ SMT.TimeOut $ Limit i
    parseStepLimit = Limit <$> depth <|> pure Unlimited
    parseStrategy =
        option readStrategy
            (  metavar "STRATEGY"
            <> long "strategy"
            -- TODO (thomas.tuegel): Make defaultStrategy the default when it
            -- works correctly.
            <> value anyRewrite
            <> help "Select rewrites using STRATEGY."
            )
      where
        strategies =
            [ ("any", anyRewrite)
            , ("all", allRewrites)
            , ("any-heating-cooling", heatingCooling anyRewrite)
            , ("all-heating-cooling", heatingCooling allRewrites)
            ]
        readStrategy = do
            strat <- str
            let found = lookup strat strategies
            case found of
                Just strategy -> pure strategy
                Nothing ->
                    let
                        unknown = "Unknown strategy '" ++ strat ++ "'. "
                        names = intercalate ", " (fst <$> strategies)
                        known = "Known strategies are: " ++ names
                    in
                        readerError (unknown ++ known)
    depth =
        option auto
            (  metavar "DEPTH"
            <> long "depth"
            <> help "Execute up to DEPTH steps."
            )
    parseMainModuleName =
        fmap ModuleName $ strOption info
      where
        info =
            mconcat
                [ metavar "MODULE"
                , long "module"
                , help "The name of the main module in the Kore definition."
                ]

-- | modifiers for the Command line parser description
parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
    <> progDesc "Uses Kore definition in DEFINITION_FILE to execute pattern \
                \in PATTERN_FILE."
    <> header "kore-exec - an interpreter for Kore definitions"

externalizeFreshVars :: CommonStepPattern level -> CommonStepPattern level
externalizeFreshVars pat = Recursive.fold renameFreshLocal pat
  where
    allVarsIds :: Set.Set Text
    allVarsIds = Set.map (getId . variableName) (pureAllVariables pat)
    freshVarsIds :: Set.Set Text
    freshVarsIds = Set.filter (Text.isPrefixOf freshVariablePrefix) allVarsIds
    computeFreshPrefix :: Text -> (Set.Set Text) -> Text
    computeFreshPrefix pref strings
      | Set.null matchingStrings = pref
      -- TODO(traiansf): if executing multiple times (like in stepping),
      -- names for generated fresh variables will grow longer and longer.
      -- Consider a mechanism to avoid this.
      | otherwise = computeFreshPrefix (pref <> "-") matchingStrings
      where
        matchingStrings :: Set.Set Text
        matchingStrings = Set.filter (Text.isPrefixOf pref) strings
    freshPrefix :: Text
    freshPrefix =
        computeFreshPrefix "var"
            (Set.filter (not . (Text.isPrefixOf freshVariablePrefix)) allVarsIds)
    renameFreshLocal
        :: Base (CommonStepPattern level) (CommonStepPattern level)
        -> CommonStepPattern level
    renameFreshLocal (_ :< VariablePattern v@(Variable {variableName}))
      | name `Set.member` freshVarsIds =
        Var_ v {
            variableName = variableName
                { getId =
                    freshPrefix <> Text.filter (/= '_') name
                }
        }
      where
        name :: Text
        name = getId variableName
    renameFreshLocal pat' = asPurePattern pat'

-- TODO(virgil): Maybe add a regression test for main.
-- | Loads a kore definition file and uses it to execute kore programs
main :: IO ()
main = do
    options <- mainGlobal parseKoreExecOptions parserInfoModifiers
    case localOptions options of
        Nothing ->
            -- global options parsed, but local failed; exit gracefully
            return ()
        Just koreExecOpts -> mainWithOptions koreExecOpts

mainWithOptions :: KoreExecOptions -> IO ()
mainWithOptions
    KoreExecOptions
        { definitionFileName
        , patternFileName
        , outputFileName
        , mainModuleName
        , smtTimeOut
        , stepLimit
        , strategy
        , koreSearchOptions
        }
  = do
        let smtConfig =
                SMT.defaultConfig
                    { SMT.timeOut = smtTimeOut }
        parsedDefinition <- parseDefinition definitionFileName
        indexedModules <- verifyDefinition True parsedDefinition
        indexedModule <-
            constructorFunctions <$> mainModule mainModuleName indexedModules
        purePattern <-
            mainPatternParseAndVerify indexedModule patternFileName
        searchParameters <-
            case koreSearchOptions of
                Nothing -> return Nothing
                Just KoreSearchOptions { searchFileName, bound, searchType } ->
                    do
                        searchPattern <-
                            mainParseSearchPattern indexedModule searchFileName
                        let searchConfig = Search.Config { bound, searchType }
                        (return . Just) (searchPattern, searchConfig)
        finalPattern <-
            clockSomethingIO "Executing"
            $ SMT.runSMT smtConfig
            $ evalSimplifier
            $ case searchParameters of
                Nothing -> exec indexedModule purePattern stepLimit strategy
                Just (searchPattern, searchConfig) ->
                    search
                        indexedModule
                        purePattern
                        stepLimit
                        strategy
                        searchPattern
                        searchConfig
        let
            finalExternalPattern =
                either (error . printError) id
                (Builtin.externalizePattern indexedModule finalPattern)
            unparsed =
                (unparse . externalizeFreshVars) finalExternalPattern
        case outputFileName of
            Nothing ->
                putDoc unparsed
            Just outputFile ->
                withFile outputFile WriteMode (\h -> hPutDoc h unparsed)

mainModule
    :: ModuleName
    -> Map.Map ModuleName (KoreIndexedModule StepperAttributes)
    -> IO (KoreIndexedModule StepperAttributes)
mainModule name modules =
    case Map.lookup name modules of
        Nothing ->
            error
                (  "The main module, '"
                ++ getModuleNameForError name
                ++ "', was not found. Check the --module flag."
                )
        Just m -> return m

{- | Parse a Kore definition from a filename.

Also prints timing information; see 'mainParse'.

 -}
parseDefinition :: FilePath -> IO KoreDefinition
parseDefinition = mainParse fromKore

-- | IO action that parses a kore pattern from a filename and prints timing
-- information.
mainPatternParse :: String -> IO CommonKorePattern
mainPatternParse = mainParse fromKorePattern

-- | IO action that parses a kore pattern from a filename, verifies it,
-- converts it to a pure patterm, and prints timing information.
mainPatternParseAndVerify
    :: KoreIndexedModule StepperAttributes
    -> String
    -> IO (CommonStepPattern Object)
mainPatternParseAndVerify indexedModule patternFileName
  = do
    parsedPattern <- mainPatternParse patternFileName
    mainPatternVerify indexedModule parsedPattern
    return (makePurePattern parsedPattern)

mainParseSearchPattern
    :: KoreIndexedModule StepperAttributes
    -> String
    -> IO (CommonExpandedPattern Object)
mainParseSearchPattern indexedModule patternFileName
  = do
    let
        metadataTools :: MetadataTools Object StepperAttributes
        metadataTools = extractMetadataTools indexedModule
        MetadataTools { symbolOrAliasSorts } = metadataTools
    purePattern <- mainPatternParseAndVerify indexedModule patternFileName
    case purePattern of
        And_ _ term predicateTerm -> return
            Predicated
                { term
                , predicate =
                    either (error . printError) id
                        (give symbolOrAliasSorts makePredicate predicateTerm)
                , substitution = mempty
                }
        _ -> error "Unexpected non-conjunctive pattern"

-- | IO action that parses a kore AST entity from a filename and prints timing
-- information.
mainParse
    :: (FilePath -> String -> Either String a)
    -> String
    -> IO a
mainParse parser fileName = do
    contents <-
        clockSomethingIO "Reading the input file" (readFile fileName)
    parseResult <-
        clockSomething "Parsing the file" (parser fileName contents)
    case parseResult of
        Left err         -> error err
        Right definition -> return definition

{- | Verify the well-formedness of a Kore definition.

Also prints timing information; see 'mainParse'.

 -}
verifyDefinition
    :: Bool -- ^ whether to check (True) or ignore attributes during verification
    -> KoreDefinition -- ^ Parsed definition to check well-formedness
    -> IO (Map.Map ModuleName (KoreIndexedModule StepperAttributes))
verifyDefinition willChkAttr definition =
    let attributesVerification =
            if willChkAttr
            then defaultAttributesVerification Proxy
            else DoNotVerifyAttributes
    in do
      verifyResult <-
        clockSomething "Verifying the definition"
            (verifyAndIndexDefinition
                attributesVerification
                Builtin.koreVerifiers
                definition
            )
      case verifyResult of
        Left err1            -> error (printError err1)
        Right indexedModules -> return indexedModules


-- | IO action verifies well-formedness of Kore patterns and prints
-- timing information.
mainPatternVerify
    :: KoreIndexedModule StepperAttributes
    -- ^ Module containing definitions visible in the pattern
    -> CommonKorePattern -- ^ Parsed pattern to check well-formedness
    -> IO ()
mainPatternVerify indexedModule patt =
    do
      verifyResult <-
        clockSomething "Verifying the pattern"
            (verifyStandalonePattern patternVerifier indexedModule patt)
      case verifyResult of
        Left err1 -> error (printError err1)
        Right _   -> return ()
  where
    Builtin.Verifiers { patternVerifier } = Builtin.koreVerifiers

makePurePattern
    :: CommonKorePattern
    -> CommonStepPattern Object
makePurePattern pat =
    case patternKoreToPure Object pat of
        Left err -> error (printError err)
        Right objPat -> objPat

-- TODO (traiansf): Get rid of this.
-- The function below works around several limitations of
-- the current tool by tricking the tool into believing that
-- functions are constructors (so that function patterns can match)
-- and that @kseq@ and @dotk@ are both functional and constructor.
constructorFunctions
    :: KoreIndexedModule StepperAttributes
    -> KoreIndexedModule StepperAttributes
constructorFunctions ixm =
    ixm
        { indexedModuleObjectSymbolSentences =
            Map.mapWithKey
                constructorFunctions1
                (indexedModuleObjectSymbolSentences ixm)
        , indexedModuleObjectAliasSentences =
            Map.mapWithKey
                constructorFunctions1
                (indexedModuleObjectAliasSentences ixm)
        , indexedModuleImports = recurseIntoImports <$> indexedModuleImports ixm
        }
  where
    constructorFunctions1 ident (atts, defn) =
        ( atts
            & lensConstructor Lens.<>~ Constructor isCons
            & lensFunctional Lens.<>~ Functional (isCons || isInj)
            & lensInjective Lens.<>~ Injective (isCons || isInj)
            & lensSortInjection Lens.<>~ SortInjection isInj
        , defn
        )
      where
        isInj = getId ident == "inj"
        isCons = elem (getId ident) ["kseq", "dotk"]

    recurseIntoImports (attrs, attributes, importedModule) =
        (attrs, attributes, constructorFunctions importedModule)
