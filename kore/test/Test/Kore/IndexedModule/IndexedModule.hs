module Test.Kore.IndexedModule.IndexedModule
    ( test_stableAxiomOrder ) where

import Test.Tasty

import qualified Data.Default as Default
import qualified Data.List as List
import qualified Data.Map as Map
import           GHC.Stack
                 ( HasCallStack )

import           Kore.AST.Sentence
import           Kore.AST.Valid
import qualified Kore.Attribute.Null as Attribute
import           Kore.IndexedModule.IndexedModule
import           Kore.Sort
import           Kore.Step.TermLike
import           Kore.Syntax

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Terse as Terse

test_stableAxiomOrder :: TestTree
test_stableAxiomOrder =
    Terse.same (indexAxioms <$> List.permutations axioms) "axiom order is independent of input"

axioms :: [SentenceAxiom SortVariable (TermLike Variable)]
axioms =
    [ soleConstructorAxiom "x"
    , soleConstructorAxiom "y"
    , soleConstructorAxiom "z"
    ]
  where
    soleConstructorAxiom name =
        mkAxiom [sortVariableR]
        $ mkForall x
        $ mkEquals sortR (mkVar x) (applySymbol_ symbolA [])
      where
        x = varS name sortS
        sortVariableR = SortVariable (testId "R")
        sortR = SortVariableSort sortVariableR

sortS :: Sort
sortS =
    SortActualSort SortActual
        { sortActualName = testId "S"
        , sortActualSorts = []
        }

symbolA :: SentenceSymbol Object (TermLike Variable)
symbolA = mkSymbol_ (testId "a") [] sortS

-- | Insert the given axioms into a module, index the module, and retrieve the
-- indexed axioms. Throws if the module cannot be indexed.
indexAxioms
    :: HasCallStack
    => [SentenceAxiom SortVariable (TermLike Variable)]
    -> [SentenceAxiom SortVariable (TermLike Variable)]
indexAxioms axioms =
    snd <$> indexedModuleAxioms indexedModule
  where
    Just indexedModule = Map.lookup "TEST" indexedModules
    indexedModule :: VerifiedModule Attribute.Null Attribute.Null
    Right indexedModules =
        indexModuleIfNeeded
            (Just defaultModule)
            allDefinedModules
            alreadyIndexed
            sourceModule
    defaultModule = ImplicitIndexedModule implicitIndexedModule
    allDefinedModules = Map.empty
    alreadyIndexed = Map.empty
    sourceModule =
        Module
            { moduleName = "TEST"
            , moduleAttributes = Default.def
            , moduleSentences =
                declareSortS
                : asSentence symbolA
                : (asSentence <$> axioms)
            }
    declareSortS =
        SentenceSortSentence SentenceSort
            { sentenceSortName = testId "S"
            , sentenceSortParameters = []
            , sentenceSortAttributes = Default.def
            }
