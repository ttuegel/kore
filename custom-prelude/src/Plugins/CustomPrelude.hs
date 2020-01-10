module Plugins.CustomPrelude
    ( plugin
    ) where

import Control.Applicative
import Data.Semigroup
import GhcPlugins hiding
    ( parseModuleName
    , (<>)
    )
import HsSyn
import qualified Options.Applicative as Options

plugin :: Plugin
plugin =
    defaultPlugin
        { pluginRecompile = purePlugin
        , parsedResultAction = addCustomPrelude
        }

data Options =
    Options
        { enabled :: !Enabled
        , modules :: ![ModuleName]
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
    <$> parseEnabled
    <*> Options.many parseModuleName

newtype Enabled = Enabled { getEnabled :: Last Bool }
    deriving (Semigroup)

instance Monoid Enabled where
    mempty = Enabled (Last True)

isEnabled :: Enabled -> Bool
isEnabled = getLast . getEnabled

parseEnabled :: Options.Parser Enabled
parseEnabled =
    mconcat <$> Options.many parseEnabled1
  where
    parseEnabled1 =
        fmap
            (Enabled . Last)
            (Options.flag' True enabled <|> Options.flag' False disabled)
    enabled =
        Options.long "enable" <> Options.help "Enable plugin"
    disabled =
        Options.long "disable" <> Options.help "Disable plugin"

parseModuleName :: Options.Parser ModuleName
parseModuleName =
    mkModuleName <$> Options.strArgument info
  where
    info = Options.help "Import MODULE" <> Options.metavar "MODULE"

addCustomPrelude
    :: [CommandLineOption]
    -> ModSummary
    -> HsParsedModule
    -> Hsc HsParsedModule
addCustomPrelude arguments _modSummary parsedModule = do
    let infoMod = Options.briefDesc
    options <- execParser parseOptions infoMod arguments
    let Options { enabled, modules } = options
    if (not . isEnabled) enabled
        then return parsedModule
        else return (importModules modules parsedModule)

execParser
    :: Options.Parser a     -- ^ parser
    -> Options.InfoMod a
    -> [CommandLineOption]  -- ^ arguments
    -> Hsc a
execParser parser infoMod arguments =
    case Options.execParserPure prefs info arguments of
        Options.Success options' -> return options'
        Options.Failure failure -> do
            let (message, _) =
                    Options.renderFailure failure "Plugins.CustomPrelude"
            error message
        Options.CompletionInvoked _ ->
            error "no command-line completion"
  where
    prefs = Options.prefs mempty
    info = Options.info parser infoMod

importModules :: [ModuleName] -> HsParsedModule -> HsParsedModule
importModules moduleNames hsParsedModule =
    hsParsedModule { hpm_module = hpm_module' }
  where
    HsParsedModule { hpm_module } = hsParsedModule
    hpm_module' =
        flip fmap hpm_module $ \hsModule ->
            let
                HsModule { hsmodImports } = hsModule
                isImported moduleName = any (isNamed moduleName) hsmodImports
                moduleNames' = filter (not . isImported) moduleNames
                importDecls = noLoc . simpleImportDecl <$> moduleNames'
                hsmodImports' = importDecls <> hsmodImports
            in hsModule { hsmodImports = hsmodImports' }
    isNamed moduleName =
        any $ \case
            XImportDecl _ -> False
            ImportDecl { ideclName } -> any (== moduleName) ideclName
