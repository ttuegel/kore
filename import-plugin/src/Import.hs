module Import
    ( plugin
    , addImportsAction
    , addImportsAux
    , addImports
    ) where

import Control.Lens
import GhcPlugins hiding
    ( parseModuleName
    , (<>)
    )
import HsSyn

import Import.Options
    ( Options (..)
    )
import qualified Import.Options as Options

plugin :: Plugin
plugin =
    defaultPlugin
        { pluginRecompile = purePlugin
        , parsedResultAction = addImportsAction
        }

addImportsAction
    :: [CommandLineOption]
    -> ModSummary
    -> HsParsedModule
    -> Hsc HsParsedModule
addImportsAction arguments _modSummary hsParsedModule = do
    options <- Options.execParser Options.parseOptions arguments
    return $ addImportsAux options hsParsedModule

addImportsAux :: Options -> HsParsedModule -> HsParsedModule
addImportsAux Options { enabled, modules }
  | Options.isEnabled enabled = addImports modules
  | otherwise                 = id

addImports :: [ModuleName] -> HsParsedModule -> HsParsedModule
addImports moduleNames hsParsedModule =
    (<>~) (lens_HsModule . lens_hsmodImports) importDecls hsParsedModule
  where
    hsModule = view lens_HsModule hsParsedModule
    moduleNames' = filter (not . isImported hsModule) moduleNames
    importDecls = noLoc . simpleImportDecl <$> moduleNames'

isNamedImportDecl :: ModuleName -> LImportDecl pass -> Bool
isNamedImportDecl moduleName (L _ importDecl) =
    case importDecl of
        XImportDecl _            -> False
        ImportDecl { ideclName } -> any (== moduleName) ideclName

isImported :: HsModule pass -> ModuleName -> Bool
isImported HsModule { hsmodImports } moduleName =
    any (isNamedImportDecl moduleName) hsmodImports

lens_hsmodImports :: Lens' (HsModule pass) [LImportDecl pass]
lens_hsmodImports f hsModule@HsModule { hsmodImports } =
    fmap
        (\hsmodImports' -> hsModule { hsmodImports = hsmodImports' })
        (f hsmodImports)

lens_HsModule :: Lens' HsParsedModule (HsModule GhcPs)
lens_HsModule = lens_hpm_module . lens_Located

lens_hpm_module :: Lens' HsParsedModule (Located (HsModule GhcPs))
lens_hpm_module f hsParsedModule@HsParsedModule { hpm_module } =
    fmap
        (\hpm_module' -> hsParsedModule { hpm_module = hpm_module' })
        (f hpm_module)

lens_Located :: Lens (Located a) (Located b) a b
lens_Located f (L src a) = fmap (L src) (f a)
