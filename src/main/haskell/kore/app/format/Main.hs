module Main where

import         Data.Semigroup ((<>))
import         Data.Text.Prettyprint.Doc ( LayoutOptions(..), PageWidth(..)
                                         , defaultLayoutOptions, layoutPretty
                                         , pretty )
import         Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import         Options.Applicative
import         System.IO (stdout)

import         Data.Kore.Parser.Parser (fromKore)

import         GlobalMain


data KoreFormatOptions =
    KoreFormatOptions
    { fileName :: FilePath  -- ^ file to unparse
    , width :: Int  -- ^ line width
    }

commandLine :: Parser KoreFormatOptions
commandLine =
    KoreFormatOptions
    <$> argument str
        (  metavar "FILE"
        <> help "Kore source file to parse" )
    <*> option auto
        (  metavar "WIDTH"
        <> long "width"
        <> value 80
        <> help "Line width [default: 80]" )

infoMod :: InfoMod options
infoMod =
    fullDesc
    <> progDesc "Parse a Kore definition and render it in standard format"
    <> header "kore-format - parse and render Kore definitions"

main :: IO ()
main =
  do
    options <- mainGlobal commandLine infoMod
    case localOptions options of
        Nothing -> return () -- global options parsed, but local failed; exit gracefully
        Just KoreFormatOptions {..} ->
          do
            defn <- readFile fileName >>= either error return . fromKore fileName
            let
              layoutOptions
                | width > 0 =
                    defaultLayoutOptions
                    { layoutPageWidth = AvailablePerLine width 1.0 }
                | otherwise =
                    defaultLayoutOptions { layoutPageWidth = Unbounded }
            renderIO stdout (layoutPretty layoutOptions $ pretty defn)
