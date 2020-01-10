module Import.Options
    ( Options (..)
    , parseOptions
    , execParser
    , Enabled (..), isEnabled
    ) where

import Control.Applicative
import Data.Semigroup
import GhcPlugins
    ( ModuleName
    , mkModuleName
    )
import qualified Options.Applicative as Options

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

execParser
    :: Monad monad
    => Options.Parser a     -- ^ parser
    -> [String]  -- ^ arguments
    -> monad a
execParser parser arguments =
    case Options.execParserPure prefs info arguments of
        Options.Success options' -> return options'
        Options.Failure failure ->
            let (message, _) = Options.renderFailure failure "Import"
            in error message
        Options.CompletionInvoked _ ->
            error "no command-line completion"
  where
    prefs = Options.prefs mempty
    info = Options.info parser Options.briefDesc
