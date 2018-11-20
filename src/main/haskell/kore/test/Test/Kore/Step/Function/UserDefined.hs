module Test.Kore.Step.Function.UserDefined (test_userDefinedFunction) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import           Data.Default
                 ( def )
import           Data.List
                 ( sort )
import qualified Data.Set as Set

import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock

import           Kore.AST.Common
                 ( Application (..), AstLocation (..), CommonPurePattern,
                 Id (..), Pattern (..), SymbolOrAlias (..) )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( fromPurePattern )
import           Kore.AST.PureToKore
                 ( patternKoreToPure )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.ASTUtils.SmartConstructors
                 ( mkTop )
import           Kore.Building.AsAst
import           Kore.Building.Patterns
import           Kore.Building.Sorts
import           Kore.Error
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SymbolOrAliasSorts )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.MetaML.AST
                 ( CommonMetaPattern )
import           Kore.Predicate.Predicate
                 ( makeFalsePredicate, makeTruePredicate )
import           Kore.Step.BaseStep
                 ( AxiomPattern (..) )
import           Kore.Step.ExpandedPattern as ExpandedPattern
                 ( Predicated (..), bottom )
import           Kore.Step.Function.Data as AttemptedFunction
                 ( AttemptedFunction (..) )
import           Kore.Step.Function.Data
                 ( CommonAttemptedFunction )
import           Kore.Step.Function.UserDefined
                 ( axiomFunctionEvaluator )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.Data
                 ( CommonPureMLPatternSimplifier, SimplificationProof (..),
                 evalSimplifier )
import           Kore.Step.StepperAttributes
import qualified SMT

import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSimplifiers as Mock
import           Test.Kore.Step.Simplifier
                 ( mockSimplifier )
import           Test.Tasty.HUnit.Extensions

test_userDefinedFunction :: [TestTree]
test_userDefinedFunction =
    [ testCase "Applies one step" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term = asPureMetaPattern (metaG (x PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern (metaF (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier [])
                (asApplication (metaF (x PatternSort)))
        assertEqualWithExplanation "f(x) => g(x)" expect actual

    , testCase "Cannot apply step with unsat axiom pre-condition" $ do
        let expect =
                AttemptedFunction.Applied (OrOfExpandedPattern.make [])
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern (metaF (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeFalsePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier [])
                (asApplication (metaF (x PatternSort)))
        assertEqualWithExplanation "f(x) => g(x) requires false" expect actual

    , testCase "Cannot apply step with unsat condition" $ do
        let expect =
                AttemptedFunction.Applied
                $ OrOfExpandedPattern.make [ ExpandedPattern.bottom ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern (metaF (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier
                    -- Evaluate Top to Bottom.
                    [ (mkTop, ([], SimplificationProof)) ]
                )
                (asApplication (metaF (x PatternSort)))
        assertEqualWithExplanation "" expect actual

    , testCase "Reevaluates the step application" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term = asPureMetaPattern (metaH (x PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern (metaF (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier
                    [   ( asPureMetaPattern (metaG (x PatternSort))
                        ,   (   [ Predicated
                                    { term =
                                        asPureMetaPattern
                                        $ metaH (x PatternSort)
                                    , predicate = makeTruePredicate
                                    , substitution = []
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (asApplication (metaF (x PatternSort)))
        assertEqualWithExplanation "f(x) => g(x) and g(x) => h(x)" expect actual

    , testCase "Does not reevaluate the step application with incompatible condition" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ExpandedPattern.bottom]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern (metaF (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier
                    [   ( asPureMetaPattern (metaG (x PatternSort))
                        ,   (   [ Predicated
                                    { term =
                                        asPureMetaPattern
                                        $ metaH (x PatternSort)
                                    , predicate = makeFalsePredicate
                                    , substitution = []
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (asApplication (metaF (x PatternSort)))
        assertEqualWithExplanation
            "f(x) => g(x) and g(x) => h(x) + false"
            expect
            actual

    , testCase "Preserves step substitution" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term = asPureMetaPattern (metaG (b PatternSort))
                        , predicate = makeTruePredicate
                        , substitution =
                            [   ( asVariable (a PatternSort)
                                , asPureMetaPattern (b PatternSort)
                                )
                            ]
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern
                            (metaSigma (x PatternSort) (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier [])
                (asApplication (metaSigma (a PatternSort) (b PatternSort)))
        assertEqualWithExplanation "sigma(x,x) => g(x) vs sigma(a, b)" expect actual

    , testCase "Merges the step substitution with the reevaluation one" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term = asPureMetaPattern (metaH (c PatternSort))
                        , predicate = makeTruePredicate
                        , substitution =
                            [   ( asVariable (a PatternSort)
                                -- TODO(virgil): Do we want normalization here?
                                , asPureMetaPattern (c PatternSort)
                                )
                            ,   ( asVariable (b PatternSort)
                                , asPureMetaPattern (c PatternSort)
                                )
                            ]
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                AxiomPattern
                    { axiomPatternLeft  =
                        asPureMetaPattern
                            (metaSigma (x PatternSort) (x PatternSort))
                    , axiomPatternRight =
                        asPureMetaPattern (metaG (x PatternSort))
                    , axiomPatternRequires = makeTruePredicate
                    , axiomPatternAttributes = def
                    }
                (mockSimplifier
                    [   ( asPureMetaPattern (metaG (b PatternSort))
                        ,   (   [ Predicated
                                    { term =
                                        asPureMetaPattern
                                        $ metaH (c PatternSort)
                                    , predicate = makeTruePredicate
                                    , substitution =
                                        [   ( asVariable (b PatternSort)
                                            , asPureMetaPattern (c PatternSort)
                                            )
                                        ]
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (asApplication (metaSigma (a PatternSort) (b PatternSort)))
        assertEqualWithExplanation
            "sigma(x,x) => g(x) vs sigma(a, b) and g(b) => h(c) + a=c,b=c"
            expect
            actual
    -- TODO: Add a test for StepWithAxiom returning a condition.
    -- TODO: Add a test for the stepper giving up
    ]

mockSymbolOrAliasSorts :: SymbolOrAliasSorts Meta
mockSymbolOrAliasSorts = const ApplicationSorts
    { applicationSortsOperands = [asAst PatternSort, asAst PatternSort]
    , applicationSortsResult = asAst PatternSort
    }

mockMetadataTools :: MetadataTools Meta StepperAttributes
mockMetadataTools = MetadataTools
    { symAttributes = const Mock.constructorFunctionalAttributes
    , symbolOrAliasType = const HeadType.Symbol
    , sortAttributes = const Mock.constructorFunctionalAttributes
    , symbolOrAliasSorts  = mockSymbolOrAliasSorts
    , isSubsortOf = const $ const False
    , subsorts = Set.singleton
    }

x :: MetaSort sort => sort -> MetaVariable sort
x = metaVariable "#x" AstLocationTest

a :: MetaSort sort => sort -> MetaVariable sort
a = metaVariable "#a" AstLocationTest

b :: MetaSort sort => sort -> MetaVariable sort
b = metaVariable "#b" AstLocationTest

c :: MetaSort sort => sort -> MetaVariable sort
c = metaVariable "#c" AstLocationTest

fSymbol :: SymbolOrAlias Meta
fSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#f" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaF p1 = MetaF p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaF p1)
  where
    asProperPattern (MetaF p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = fSymbol
            , applicationChildren = [asAst p1]
            }
metaF
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaF p1
metaF = MetaF


gSymbol :: SymbolOrAlias Meta
gSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#g" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaG p1 = MetaG p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaG p1)
  where
    asProperPattern (MetaG p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = gSymbol
            , applicationChildren = [asAst p1]
            }
metaG
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaG p1
metaG = MetaG


hSymbol :: SymbolOrAlias Meta
hSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#h" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaH p1 = MetaH p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaH p1)
  where
    asProperPattern (MetaH p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = hSymbol
            , applicationChildren = [asAst p1]
            }
metaH
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaH p1
metaH = MetaH


sigmaSymbol :: SymbolOrAlias Meta
sigmaSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#sigma" AstLocationTest
    , symbolOrAliasParams = []
    }

data MetaSigma p1 p2 = MetaSigma p1 p2
instance (MetaPattern PatternSort p1, MetaPattern PatternSort p2)
    => ProperPattern Meta PatternSort (MetaSigma p1 p2)
  where
    asProperPattern (MetaSigma p1 p2) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = sigmaSymbol
            , applicationChildren = [asAst p1, asAst p2]
            }
metaSigma
    :: (MetaPattern PatternSort p1, MetaPattern PatternSort p2)
    => p1 -> p2 -> MetaSigma p1 p2
metaSigma = MetaSigma

asPureMetaPattern
    :: ProperPattern Meta sort patt => patt -> CommonMetaPattern
asPureMetaPattern patt =
    case patternKoreToPure Meta (asAst patt) of
        Left err  -> error (printError err)
        Right pat -> pat

asApplication
    :: ProperPattern Meta sort patt => patt
    -> Application Meta (CommonPurePattern Meta)
asApplication patt =
    case fromPurePattern (asPureMetaPattern patt) of
        ApplicationPattern app -> app
        _                      -> error "Expected an Application pattern."

evaluateWithAxiom
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -> AxiomPattern level
    -> CommonPureMLPatternSimplifier level
    -> Application level (CommonPurePattern level)
    -> IO (CommonAttemptedFunction level)
evaluateWithAxiom
    metadataTools
    axiom
    simplifier
    app
  =
    evaluated >>= return . \case
        AttemptedFunction.Applied orPattern ->
            AttemptedFunction.Applied (fmap sortSubstitution orPattern)
        result -> result
  where
    sortSubstitution Predicated {term, predicate, substitution} =
        Predicated
            { term = term
            , predicate = predicate
            , substitution = sort substitution
            }
    evaluated =
        (<$>) fst
        $ SMT.runSMT SMT.defaultConfig
        $ evalSimplifier
        $ axiomFunctionEvaluator
            axiom
            metadataTools
            (Mock.substitutionSimplifier metadataTools)
            simplifier
            app
