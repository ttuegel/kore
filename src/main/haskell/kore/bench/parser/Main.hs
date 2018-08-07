module Main where

import Criterion.Main

import Data.Proxy ( Proxy (Proxy) )
import System.FilePath
       ( takeFileName )

import Kore.AST.Annotated.Sentence ( unannotateDefinition )
import Kore.ASTVerifier.DefinitionVerifier
       ( defaultAttributesVerification, verifyDefinition )
import Kore.Parser.Parser
       ( fromKore )

import qualified Paths

main :: IO ()
main =
    defaultMain
    [ parse "kore.kore" (Paths.dataFileName "../../kore/kore.kore")
    , parse "bool.kore" (Paths.dataFileName "../../../test/resources/bool.kore")
    , parse "imp.kore" (Paths.dataFileName "../../../test/resources/imp.kore")
    , parse "imp2.kore" (Paths.dataFileName "../../../test/resources/imp2.kore")
    , parse "lambda.kore" (Paths.dataFileName "../../../test/resources/lambda.kore")
    , parse "list.kore" (Paths.dataFileName "../../../test/resources/list.kore")
    , parse "nat.kore" (Paths.dataFileName "../../../test/resources/nat.kore")
    , parse "user-meta-nat.kore" (Paths.dataFileName "../../../test/resources/user-meta-nat.kore")
    ]

{- | Declare a parser benchmark

The benchmark will parse the contents of the file. The file is read only once
before the benchmark is run because Criterion may repeat a benchmark many times
to gather timing statistics.
-}
parse
    :: String  -- ^ benchmark name (for the report)
    -> FilePath  -- ^ name of file to parse
    -> Benchmark
parse name filename =
    env (readFile filename)  -- Read Kore definition once before benchmark
    (bench name . nf (fromKore filename))  -- Benchmark parsing step only
  where
    name = takeFileName filename


{- | Declare a parser benchmark

The benchmark will parse the contents of the file. This benchmark includes the
overhead of reading the file, in contrast to 'parse' above.
-}
readAndParse
    :: FilePath  -- ^ name of file to parse
    -> Benchmark
readAndParse filename =
    bench name $ nfIO (fromKore filename <$> readFile filename)
  where
    name = takeFileName filename

{- | Declare a verifier benchmark

The benchmark will verify the contents of the file. The file is read and parsed
only once before the benchmark is run because Criterion may repeat a benchmark
many times to gather timing statistics.
-}
verify
    :: FilePath
    -> Benchmark
verify filename =
    env parse1 (bench name . nf verify1)
  where
    name = takeFileName filename
    -- | Read and parse the file once before the benchmark
    parse1 = do
        parsed <- fromKore filename <$> readFile filename
        case parsed of
            Left err -> error err
            Right defn -> pure (unannotateDefinition defn)
    -- | Verify the Kore definition.
    -- Runs once per benchmark iteration.
    verify1 defn =
        case verifyDefinition attributesVerification defn of
            Left err -> error (show err)
            Right _ -> ()
      where
        attributesVerification =
            defaultAttributesVerification
            (Proxy @StepperAttributes)
