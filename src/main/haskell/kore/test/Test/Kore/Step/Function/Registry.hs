module Test.Kore.Step.Function.Registry (test_functionRegistry) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( assertEqual, testCase )

import qualified Data.Map as Map
import           Data.Maybe
                 ( fromMaybe )
import           Data.Proxy
                 ( Proxy (..) )
import           Data.Text
                 ( Text )
import           Data.These
                 ( These (That) )

import           Kore.AST.Kore
import           Kore.AST.Pure
import           Kore.AST.PureToKore
                 ( patternPureToKore )
import           Kore.AST.Sentence
import           Kore.ASTUtils.SmartPatterns
import           Kore.ASTVerifier.DefinitionVerifier
import qualified Kore.Builtin as Builtin
import qualified Kore.Domain.Builtin as Domain
import           Kore.Error
                 ( printError )
import           Kore.IndexedModule.IndexedModule
                 ( KoreIndexedModule )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), extractMetadataTools )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.AxiomPatterns
                 ( extractRewriteAxioms )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.Function.Data
import           Kore.Step.Function.Registry
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.Simplification.Simplifier as Simplifier
                 ( create )
import           Kore.Step.StepperAttributes
import qualified SMT

import           Test.Kore.ASTVerifier.DefinitionVerifier
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSimplifiers as Mock

updateAttributes :: Attributes -> KoreSentence -> KoreSentence
updateAttributes attrs = applyUnifiedSentence updateAttrs updateAttrs
  where
    updateAttrs
        :: MetaOrObject level
        => Sentence
            level
            UnifiedSortVariable
            KorePattern
            Domain.Builtin
            Variable
        -> KoreSentence
    updateAttrs (SentenceSymbolSentence ss) =
        constructUnifiedSentence SentenceSymbolSentence
            (ss { sentenceSymbolAttributes = attrs })
    updateAttrs _ = error "unsupported non-symbol sentence"

sortVar :: SortVariable Object
sortVar = SortVariable (testId "R")

sortVarS :: Sort Object
sortVarS = SortVariableSort sortVar

sortS :: Sort level
sortS = SortActualSort (SortActual (testId "S") [])

fHead, gHead, sHead, tHead :: SymbolOrAlias level
fHead = groundHead "f" AstLocationTest
gHead = groundHead "g" AstLocationTest
sHead = groundHead "s" AstLocationTest
tHead = groundHead "t" AstLocationTest

testDef :: KoreDefinition
testDef = simpleDefinitionFromSentences
            (ModuleName "test")
            [ simpleSortSentence (SortName "S")
            , simpleObjectSymbolSentence (SymbolName "s") (SortName "S")
            , simpleObjectSymbolSentence (SymbolName "t") (SortName "S")
            , updateAttributes
                (Attributes [functionAttribute, constructorAttribute])
                (simpleObjectSymbolSentence (SymbolName "f") (SortName "S"))
            , updateAttributes
                (Attributes [functionAttribute, constructorAttribute])
                (simpleObjectSymbolSentence (SymbolName "g") (SortName "S"))
            , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            ( Implies_ sortVarS
                                (Top_ sortVarS)
                                (And_ sortVarS
                                    (Equals_ sortS sortVarS
                                        (App_ gHead [])
                                        (App_ sHead [])
                                    )
                                    (Top_ sortVarS)
                                )
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
            , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            ( Implies_ sortVarS
                                (Top_ sortVarS)
                                (And_ sortVarS
                                    (Equals_ sortS sortVarS
                                        (Top_ sortS)
                                        (App_ fHead [])
                                    )
                                    (Top_ sortVarS)
                                )
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
            , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            ( Implies_ sortVarS
                                (Top_ sortVarS)
                                (And_ sortVarS
                                    (Equals_ sortS sortVarS
                                        (App_ fHead [])
                                        (App_ sHead [])
                                    )
                                    (Top_ sortVarS)
                                )
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
             , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            ( Implies_ sortVarS
                                (Top_ sortVarS)
                                (And_ sortVarS
                                    (Equals_ sortS sortVarS
                                        (App_ fHead [])
                                        (App_ tHead [])
                                    )
                                    (Top_ sortVarS)
                                )
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
            , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            (Top_ sortS
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
            , asSentence
                (SentenceAxiom
                    { sentenceAxiomParameters = [asUnified sortVar]
                    , sentenceAxiomAttributes = Attributes []
                    , sentenceAxiomPattern =
                        (patternPureToKore
                            (And_ sortS (Top_ sortS)
                                (And_ sortS (Top_ sortS)
                                    (Rewrites_ sortS
                                        (App_ fHead [])
                                        (App_ tHead [])
                                    )
                                )
                            :: CommonStepPattern Object)
                        )
                    }
                ::KoreSentenceAxiom)
            ]

testIndexedModule :: KoreIndexedModule StepperAttributes
testIndexedModule =
    let
        attributesVerification = defaultAttributesVerification Proxy
        verifyResult = verifyAndIndexDefinition
            attributesVerification
            Builtin.koreVerifiers
            testDef
    in
        case verifyResult of
            Left err1            -> error (printError err1)
            Right indexedModules ->
                fromMaybe
                    (error "Module not found. Should not be possible.")
                    (Map.lookup (ModuleName "test") indexedModules)

testId :: Text -> Id level
testId name =
    Id
        { getId = name
        , idLocation = AstLocationTest
        }

testEvaluators
    :: BuiltinAndAxiomsFunctionEvaluatorMap Object
testEvaluators =
    Map.map That
    $ axiomPatternsToEvaluators
    $ extractFunctionAxioms Object testIndexedModule

testMetadataTools :: MetadataTools Object StepperAttributes
testMetadataTools = extractMetadataTools testIndexedModule

test_functionRegistry :: [TestTree]
test_functionRegistry =
    [ testCase "Checking that two axioms are found for f"
        (assertEqual ""
            2
            (case Map.lookup (testId "f") testEvaluators of
                Just (That axioms) -> length axioms
                _ -> error "Should find precisely two axioms for f"
            )
        )
     , testCase "Checking that evaluator map has size 2"
        (assertEqual ""
            2
            (Map.size testEvaluators)
        )
    , testCase "Checking that the indexed module contains a rewrite axiom"
        (assertEqual ""
            (1::Int)
            (length (extractRewriteAxioms Object testIndexedModule))
        )
    , testCase "Checking that evaluator simplifies correctly" $ do
        let expect = App_ sHead []
        (simplified, _) <-
            SMT.runSMT SMT.defaultConfig
            $ evalSimplifier
            $ ExpandedPattern.simplify
                testMetadataTools
                (Mock.substitutionSimplifier testMetadataTools)
                (Simplifier.create testMetadataTools testEvaluators)
                (makeExpandedPattern (App_ gHead []))
        let actual =
                ExpandedPattern.term $ head
                $ OrOfExpandedPattern.extractPatterns simplified
        assertEqual "" expect actual
    ]
  where
    makeExpandedPattern
        :: CommonStepPattern Object
        -> CommonExpandedPattern Object
    makeExpandedPattern pat =
        Predicated
        { term = pat
        , predicate = makeTruePredicate
        , substitution = mempty
        }
