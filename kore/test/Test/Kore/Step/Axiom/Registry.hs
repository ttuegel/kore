module Test.Kore.Step.Axiom.Registry
    ( test_functionRegistry
    ) where

import Prelude.Kore

import Test.Tasty
    ( TestTree
    )
import Test.Tasty.HUnit
    ( assertEqual
    , assertFailure
    , testCase
    )

import qualified Data.Default as Default
import qualified Data.Map.Strict as Map
import Data.Text
    ( Text
    )

import Kore.ASTVerifier.DefinitionVerifier
import qualified Kore.Attribute.Owise as Attribute
import Kore.Attribute.Priority
    ( defaultPriority
    , owisePriority
    )
import qualified Kore.Attribute.Priority as Attribute
import Kore.Attribute.Simplification
    ( simplificationAttribute
    )
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin as Builtin
import qualified Kore.Equation as Equation
import Kore.Equation.Registry
import Kore.Error
    ( printError
    )
import Kore.IndexedModule.IndexedModule
    ( VerifiedModule
    )
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    )
import qualified Kore.IndexedModule.MetadataToolsBuilder as MetadataTools
    ( build
    )
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.Pattern as Pattern
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.Symbol as Symbol
import Kore.Internal.TermLike
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
    ( AxiomIdentifier (..)
    )
import Kore.Step.Axiom.Registry
import Kore.Step.Rule
    ( extractRewriteAxioms
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
import Kore.Step.Simplification.Simplify
import Kore.Syntax.Definition hiding
    ( Symbol
    )

import Test.Kore
import Test.Kore.ASTVerifier.DefinitionVerifier
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification

type PartitionedEquationsMap =
    Map.Map AxiomIdentifier.AxiomIdentifier PartitionedEquations

updateAttributes :: Attributes -> Sentence patternType -> Sentence patternType
updateAttributes attrs = updateAttrs
  where
    updateAttrs :: Sentence patternType -> Sentence patternType
    updateAttrs (SentenceSymbolSentence ss) =
        SentenceSymbolSentence
            (ss { sentenceSymbolAttributes = attrs })
    updateAttrs _ = error "unsupported non-symbol sentence"

sortVar :: SortVariable
sortVar = SortVariable (testId "R")

sortVar1 :: SortVariable
sortVar1 = SortVariable (testId "R1")

sortVarS :: Sort
sortVarS = SortVariableSort sortVar

sortVar1S :: Sort
sortVar1S = SortVariableSort sortVar1

sortS :: Sort
sortS = SortActualSort (SortActual (testId "S") [])

testSymbol :: Text -> Symbol
testSymbol name =
    Symbol
        { symbolConstructor = testId name
        , symbolParams = []
        , symbolAttributes = Default.def
        , symbolSorts = applicationSorts [] sortS
        }
    & Symbol.function

fHead, gHead, pHead, qHead, sHead, tHead :: Symbol
fHead = testSymbol "f"
gHead = testSymbol "g"
sHead = testSymbol "s"
tHead = testSymbol "t"
pHead = testSymbol "p"
qHead = testSymbol "q"

injHead :: Sort -> Sort -> Symbol
injHead s1 s2 =
    (testSymbol "inj")
        { symbolParams = [s1, s2]
        , symbolSorts = applicationSorts [s1] s2
        }

testDef :: Definition ParsedSentence
testDef =
    simpleDefinitionFromSentences
        (ModuleName "test")
        [ simpleSortSentence (SortName "S")
        , simpleSymbolSentence (SymbolName "s") (SortName "S")
            & (updateAttributes . Attributes)
                [ Attribute.functionalAttribute
                , Attribute.constructorAttribute
                ]
        , simpleSymbolSentence (SymbolName "t") (SortName "S")
            & (updateAttributes . Attributes)
                [ Attribute.functionalAttribute
                , Attribute.constructorAttribute
                ]
        , symbolSentenceWithParametersAndArguments
            (SymbolName "inj")
            [sortVar, sortVar1]
            sortVar1S
            [sortVarS]
            & updateAttributes (Attributes [Attribute.functionAttribute])
        , updateAttributes
            (Attributes
                [ Attribute.functionAttribute
                , Attribute.constructorAttribute
                ]
            )
            (simpleSymbolSentence (SymbolName "f") (SortName "S"))
        , updateAttributes
            (Attributes
                [ Attribute.functionAttribute
                , Attribute.constructorAttribute]
            )
            (simpleSymbolSentence (SymbolName "g") (SortName "S"))
        , updateAttributes
            (Attributes
                [ Attribute.functionAttribute
                , Attribute.constructorAttribute
                ]
            )
            (simpleSymbolSentence (SymbolName "p") (SortName "S"))
        , updateAttributes
            (Attributes
                [ Attribute.functionAttribute
                , Attribute.constructorAttribute]
            )
            (simpleSymbolSentence (SymbolName "q") (SortName "S"))
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes []
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals
                            sortVarS
                            (mkApplySymbol (injHead sortS sortS)
                                [mkApplySymbol tHead []]
                            )
                            (mkApplySymbol sHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes []
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals
                            sortVarS
                            (mkApplySymbol gHead [])
                            (mkApplySymbol sHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes []
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkTop sortS)
                            (mkApplySymbol fHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes [Attribute.priorityAttribute 2]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol sHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes [Attribute.priorityAttribute 3]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol sHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes [Attribute.owiseAttribute]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol tHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes [Attribute.priorityAttribute 1]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol tHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes []
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol tHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes [simplificationAttribute]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol fHead [])
                            (mkApplySymbol gHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes
                    [ simplificationAttribute
                    , Attribute.priorityAttribute 3
                    ]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol pHead [])
                            (mkApplySymbol qHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes
                    [ simplificationAttribute
                    , Attribute.priorityAttribute 1
                    ]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol pHead [])
                            (mkApplySymbol tHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes =
                Attributes
                    [ simplificationAttribute
                    , Attribute.priorityAttribute 2
                    ]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkApplySymbol pHead [])
                            (mkApplySymbol qHead [])
                        )
                        (mkTop sortVarS)
                    )
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar]
            , sentenceAxiomAttributes = Attributes []
            , sentenceAxiomPattern =
                Builtin.externalize $ mkRewrites
                    (mkAnd mkTop_ (mkApplySymbol fHead []))
                    (mkAnd mkTop_ (mkApplySymbol tHead []))
            }
        , SentenceAxiomSentence SentenceAxiom
            { sentenceAxiomParameters = [sortVar, sortVar1]
            , sentenceAxiomAttributes = Attributes [simplificationAttribute]
            , sentenceAxiomPattern =
                Builtin.externalize $ mkImplies
                    (mkTop sortVarS)
                    (mkAnd
                        (mkEquals sortVarS
                            (mkCeil sortVar1S (mkApplySymbol fHead []))
                            mkTop_
                        )
                        (mkTop sortVarS)
                    )
            }
        ]

testIndexedModule :: VerifiedModule Attribute.Symbol
testIndexedModule =
    let
        verifyResult = verifyAndIndexDefinition Builtin.koreVerifiers testDef
    in
        case verifyResult of
            Left err1            -> error (printError err1)
            Right indexedModules ->
                fromMaybe
                    (error "Module not found. Should not be possible.")
                    (Map.lookup (ModuleName "test") indexedModules)

testEvaluators :: BuiltinAndAxiomSimplifierMap
testEvaluators =
    mkEvaluatorRegistry $ extractEquations testIndexedModule

testProcessedAxiomPatterns :: PartitionedEquationsMap
testProcessedAxiomPatterns =
    partitionEquations <$> extractEquations testIndexedModule

testMetadataTools :: SmtMetadataTools Attribute.Symbol
testMetadataTools = MetadataTools.build testIndexedModule

testEnv :: Env Simplifier
testEnv =
    Mock.env
        { metadataTools = testMetadataTools
        , simplifierAxioms = testEvaluators
        }

test_functionRegistry :: [TestTree]
test_functionRegistry =
    [ testCase "Checking that a simplifier is found for f"
        (let axiomId = AxiomIdentifier.Application (testId "f")
          in
            (case Map.lookup axiomId testEvaluators of
                Just _ -> return ()
                _ -> assertFailure "Should find a simplifier for f"
            )
        )
    , testCase "Checking that a simplifier is found for parametric inj"
        (let axiomId = AxiomIdentifier.Application (testId "inj")
          in
            (case Map.lookup axiomId testEvaluators of
                Just _ -> return ()
                _ -> assertFailure "Should find a simplifier for inj"
            )
        )
    , testCase "Checking that a simplifier is found for ceil(f)"
        (let
            axiomId =
                AxiomIdentifier.Ceil (AxiomIdentifier.Application (testId "f"))
          in
            (case Map.lookup axiomId testEvaluators of
                Just _ -> return ()
                _ -> assertFailure "Should find a simplifier for ceil(f)"
            )
        )
    , testCase "Checking that evaluator map has size 4"
        (assertEqual ""
            5
            (Map.size testEvaluators)
        )
    , testCase "Checking that the indexed module contains a rewrite axiom"
        (assertEqual ""
            (1::Int)
            (length (extractRewriteAxioms testIndexedModule))
        )
    , testCase "Checking that evaluator simplifies correctly" $ do
        let expect = mkApplySymbol sHead []
        simplified <-
            runSimplifier testEnv
            $ Pattern.simplify SideCondition.top
            $ makePattern $ mkApplySymbol gHead []
        let actual =
                Pattern.term $ head
                $ MultiOr.extractPatterns simplified
        assertEqual "" expect actual
    , testCase "Checking that evaluator simplifies correctly" $ do
        let expect = mkApplySymbol tHead []
        simplified <-
            runSimplifier testEnv
            $ Pattern.simplify SideCondition.top
            $ makePattern $ mkApplySymbol pHead []
        let actual =
                Pattern.term $ head
                $ MultiOr.extractPatterns simplified
        assertEqual "" expect actual
    , testCase "Function rules sorted in order of priorities"
        (let axiomId = AxiomIdentifier.Application (testId "f")
          in
           (case Map.lookup axiomId testProcessedAxiomPatterns of
                Just PartitionedEquations { functionRules } ->
                    assertEqual ""
                        [1, 2, 3, defaultPriority, owisePriority]
                        (fmap Equation.equationPriority functionRules)
                _ -> assertFailure "Should find function rules for f"
            )
        )
    , testCase "Simplification rules sorted in order of priorities"
        (let axiomId = AxiomIdentifier.Application (testId "p")
          in
           (case Map.lookup axiomId testProcessedAxiomPatterns of
                Just PartitionedEquations { simplificationRules } ->
                    assertEqual ""
                        [1, 2, 3]
                        (fmap Equation.equationPriority simplificationRules)
                _ -> assertFailure "Should find simplification rules for p"
            )
        )
    ]
  where
    makePattern :: TermLike VariableName -> Pattern VariableName
    makePattern = Pattern.fromTermLike
