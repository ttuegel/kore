import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Reflection
import qualified Data.Set as Set
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char

import Logic.Matching.Pattern
import Logic.Matching.Prover.Repl
import Logic.Matching.Rules.Minimal
import Logic.Matching.Rules.Minimal.Syntax (parseMLRuleSig)
import Logic.Matching.Signature.Simple
import Logic.Matching.Syntax
import Logic.Proof.Hilbert

-- Todo: Parsing Formula as Text. Hook to Kore Parser
parseName :: Parser Text
parseName = takeWhile1P Nothing isAlphaNum <* space

pCommand ::
       (Reifies s ValidatedSignature)
    => Parser (Command Text (MLRuleSig (SimpleSignature s) Text) (WFPattern (SimpleSignature s) Text))
pCommand = parseCommand parseName parseFormula parseRule
  where
    parseFormula = simpleSigPattern parseName parseName parseName
    parseLabel = simpleSigLabel parseName
    parseSort = simpleSigSort parseName
    parseRule = parseMLRuleSig parseSort parseLabel parseName parseName

proveCommand ::
       (Reifies sig ValidatedSignature)
    => proxy (SimpleSignature sig)
    -> IO (ProverState Text (MLRuleSig (SimpleSignature sig) Text) (WFPattern (SimpleSignature sig) Text))
proveCommand _ =
    runProver dummyFormulaVerifier pCommand (ProverState emptyProof)

testSignature :: SignatureInfo
testSignature =
    SignatureInfo
        { sorts = Set.fromList ["Nat", "Bool"]
        , labels =
              Map.fromList
                  [ ("plus", ("Nat", ["Nat", "Nat"]))
                  , ("succ", ("Nat", ["Nat"]))
                  , ("zero", ("Nat", []))
                  ]
        }

main :: IO ()
main =
    case validate testSignature of
        Nothing -> return ()
        Just validSig ->
            reifySignature validSig (\proxy -> proveCommand proxy >> return ())
