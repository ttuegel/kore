module Test.Kore.Step.Step (test_multiple, test_single) where

import Test.Tasty
       ( TestTree, testGroup )
import Test.Tasty.HUnit
       ( testCase )

import           Data.Default
                 ( def )
import qualified Data.Map as Map

import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock

import           Kore.AST.Common
                 ( Application (..), AstLocation (..), Id (..),
                 Pattern (ApplicationPattern), SymbolOrAlias (..), Variable )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureToKore
                 ( patternKoreToPure )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.Building.AsAst
import           Kore.Building.Patterns
import           Kore.Building.Sorts
import           Kore.Error
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SortTools )
import           Kore.MetaML.AST
                 ( CommonMetaPattern )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.BaseStep
import           Kore.Step.ExpandedPattern as ExpandedPattern
                 ( CommonExpandedPattern, ExpandedPattern (..) )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.PatternAttributes
                 ( FunctionalProof (..) )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..), evalSimplifier )
import           Kore.Step.Step
import           Kore.Step.StepperAttributes
import           Kore.Unification.Unifier
                 ( UnificationProof (..) )

import Test.Kore.Comparators ()
import Test.Tasty.HUnit.Extensions

v1 :: MetaSort sort => sort -> MetaVariable sort
v1 = metaVariable "#v1" AstLocationTest
a1 :: MetaSort sort => sort -> MetaVariable sort
a1 = metaVariable "#a1" AstLocationTest
b1 :: MetaSort sort => sort -> MetaVariable sort
b1 = metaVariable "#b1" AstLocationTest
x1 :: MetaSort sort => sort -> MetaVariable sort
x1 = metaVariable "#x1" AstLocationTest
var_0 :: MetaSort sort => sort -> MetaVariable sort
var_0 = metaVariable "#var_0" AstLocationTest
var_1 :: MetaSort sort => sort -> MetaVariable sort
var_1 = metaVariable "#var_1" AstLocationTest
var_2 :: MetaSort sort => sort -> MetaVariable sort
var_2 = metaVariable "#var_2" AstLocationTest
var_3 :: MetaSort sort => sort -> MetaVariable sort
var_3 = metaVariable "#var_3" AstLocationTest

test_single :: TestTree
test_single =
    testGroup
        "Single step Tests"
        [ testCase "Applies a simple axiom"
            -- Axiom: X1 => X1
            -- Start pattern: V1
            -- Expected: V1
            (assertEqualWithExplanation ""
                ( OrOfExpandedPattern.make
                    [ ExpandedPattern
                        { term = asPureMetaPattern (v1 PatternSort)
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
                , mconcat
                    (map stepProof
                        [ StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            ( proposition_5_24_3
                                [ functionalVariable (v1 PatternSort) ]
                                (var_0 PatternSort)
                                (v1 PatternSort)
                            )
                        ]
                    )
                )
                (runStep
                    mockMetadataTools
                    ExpandedPattern
                        { term = asPureMetaPattern (v1 PatternSort)
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft = asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRight = asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "Applies two simple axioms"
            -- Axiom: X1 => X1
            -- Axiom: X1 => implies(X1, X1)
            -- Start pattern: V1
            -- Expected: V1
            -- Expected: implies(V1, V1)
            (assertEqualWithExplanation ""
                ( OrOfExpandedPattern.make
                    [ ExpandedPattern
                        { term = asPureMetaPattern (v1 PatternSort)
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    , ExpandedPattern
                        { term =
                            asPureMetaPattern
                                (metaImplies PatternSort
                                    (v1 PatternSort)
                                    (v1 PatternSort)
                                )
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
                , mconcat
                    (map stepProof
                        [ StepProofSimplification SimplificationProof
                        , StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            ( proposition_5_24_3
                                [ functionalVariable (v1 PatternSort) ]
                                (var_0 PatternSort)
                                (v1 PatternSort)
                            )
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_1 PatternSort)
                            ]
                        , StepProofUnification
                            ( proposition_5_24_3
                                [ functionalVariable (v1 PatternSort) ]
                                (var_1 PatternSort)
                                (v1 PatternSort)
                            )
                        ]
                    )
                )
                (runStep
                    mockMetadataTools
                    ExpandedPattern
                        { term = asPureMetaPattern (v1 PatternSort)
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft = asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRight = asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    , AxiomPattern
                        { axiomPatternLeft = asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRight =
                            asPureMetaPattern
                                (metaImplies PatternSort
                                    (x1 PatternSort)
                                    (x1 PatternSort)
                                )
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "Fails to apply a simple axiom"
            -- Axiom: sigma(X1, X1) => X1
            -- Start pattern: sigma(f(A1), g(B1))
            -- Expected: empty result list
            (assertEqualWithExplanation ""
                ( OrOfExpandedPattern.make []
                , mconcat
                    (map stepProof
                        [ StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification EmptyUnificationProof
                        ]
                    )
                )
                (runStep
                    mockMetadataTools
                    ExpandedPattern
                        { term =
                            asPureMetaPattern
                                ( metaSigma
                                    (metaG (a1 PatternSort))
                                    (metaF (b1 PatternSort))
                                )
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern
                                (metaSigma (x1 PatternSort) (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "Fails to apply a simple axiom due to cycle."
            -- Axiom: sigma(f(X1), X1) => X1
            -- Start pattern: sigma(A1, A1)
            -- Expected: empty result list
            (assertEqualWithExplanation ""
                ( OrOfExpandedPattern.make []
                , mconcat
                    (map stepProof
                        [ StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            ( AndDistributionAndConstraintLifting
                                sigmaSymbol
                                [ proposition_5_24_3
                                    [ FunctionalHead fSymbol
                                    , functionalVariable (var_0 PatternSort)
                                    ]
                                    (a1 PatternSort)
                                    (metaF (var_0 PatternSort))
                                , proposition_5_24_3
                                    [ functionalVariable (a1 PatternSort)]
                                    (var_0 PatternSort)
                                    (a1 PatternSort)
                                ]
                            )
                        ]
                    )
                )
                (runStep
                    mockMetadataTools
                    ExpandedPattern
                        { term =
                            asPureMetaPattern
                                ( metaSigma
                                    (a1 PatternSort)
                                    (a1 PatternSort)
                                )
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern
                                (metaSigma
                                    (metaF (x1 PatternSort))
                                    (x1 PatternSort)
                                )
                        , axiomPatternRight =
                            asPureMetaPattern (x1 PatternSort)
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        ]

test_multiple :: TestTree
test_multiple =
    testGroup
        "Pick-first stepper Tests"
        [ testCase "Runs one step"
            -- Axiom: f(X1) => g(X1)
            -- Start pattern: f(V1)
            -- Expected: g(V1)
            (assertEqualWithExplanation ""
                ( ExpandedPattern
                    { term = asPureMetaPattern (metaG (v1 PatternSort))
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                , mconcat
                    (map stepProof
                        [ StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            (AndDistributionAndConstraintLifting
                                fSymbol
                                [ proposition_5_24_3
                                    [ functionalVariable (v1 PatternSort) ]
                                    (var_0 PatternSort)
                                    (v1 PatternSort)
                                ]
                            )
                        ]
                    )
                )
                (runStepsPickFirst
                    mockMetadataTools
                    AnyStepCount
                    ExpandedPattern
                        { term = asPureMetaPattern (metaF (v1 PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaF (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "Runs two steps"
            -- Axiom: f(X1) => g(X1)
            -- Axiom: g(X1) => h(X1)
            -- Start pattern: f(V1)
            -- Expected: h(V1)
            (assertEqualWithExplanation ""
                ( ExpandedPattern
                    { term = asPureMetaPattern (metaH (v1 PatternSort))
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                , mconcat
                    (map stepProof
                        [ StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            (AndDistributionAndConstraintLifting
                                fSymbol
                                [ proposition_5_24_3
                                    [ functionalVariable (v1 PatternSort) ]
                                    (var_0 PatternSort)
                                    (v1 PatternSort)
                                ]
                            )
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_1 PatternSort)
                            ]
                        , StepProofUnification EmptyUnificationProof
                        , StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_2 PatternSort)
                            ]
                        , StepProofUnification EmptyUnificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_3 PatternSort)
                            ]
                        , StepProofUnification
                            (AndDistributionAndConstraintLifting
                                gSymbol
                                [ proposition_5_24_3
                                    [ functionalVariable (v1 PatternSort) ]
                                    (var_3 PatternSort)
                                    (v1 PatternSort)
                                ]
                            )
                        ]
                    )
                )
                (runStepsPickFirst
                    mockMetadataTools
                    AnyStepCount
                    ExpandedPattern
                        { term = asPureMetaPattern (metaF (v1 PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaF (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    , AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaH (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "Obeys step limit"
            -- Axiom: f(X1) => g(X1)
            -- Axiom: g(X1) => h(X1)
            -- Start pattern: f(V1)
            -- Expected: g(V1)
            (assertEqualWithExplanation ""
                ( ExpandedPattern
                    { term = asPureMetaPattern (metaG (v1 PatternSort))
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                , mconcat
                    (map stepProof
                        [ StepProofSimplification SimplificationProof
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_0 PatternSort)
                            ]
                        , StepProofUnification
                            (AndDistributionAndConstraintLifting
                                fSymbol
                                [ proposition_5_24_3
                                    [ functionalVariable (v1 PatternSort) ]
                                    (var_0 PatternSort)
                                    (v1 PatternSort)
                                ]
                            )
                        , StepProofVariableRenamings
                            [ variableRenaming
                                (x1 PatternSort) (var_1 PatternSort)
                            ]
                        , StepProofUnification EmptyUnificationProof
                        ]
                    )
                )
                (runStepsPickFirst
                    mockMetadataTools
                    (MaxStepCount 1)
                    ExpandedPattern
                        { term = asPureMetaPattern (metaF (v1 PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaF (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    , AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaH (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        , testCase "0 step limit"
            -- Axiom: f(X1) => g(X1)
            -- Axiom: g(X1) => h(X1)
            -- Start pattern: f(V1)
            -- Expected: f(V1)
            (assertEqualWithExplanation ""
                ( ExpandedPattern
                    { term = asPureMetaPattern (metaF (v1 PatternSort))
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                , mempty
                )
                (runStepsPickFirst
                    mockMetadataTools
                    (MaxStepCount 0)
                    ExpandedPattern
                        { term = asPureMetaPattern (metaF (v1 PatternSort))
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    [ AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaF (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    , AxiomPattern
                        { axiomPatternLeft =
                            asPureMetaPattern (metaG (x1 PatternSort))
                        , axiomPatternRight =
                            asPureMetaPattern (metaH (x1 PatternSort))
                        , axiomPatternRequires = makeTruePredicate
                        , axiomPatternAttributes = def
                        }
                    ]
                )
            )
        ]

mockSortTools :: SortTools Meta
mockSortTools = const ApplicationSorts
    { applicationSortsOperands = [asAst PatternSort, asAst PatternSort]
    , applicationSortsResult = asAst PatternSort
    }

mockMetadataTools :: MetadataTools Meta StepperAttributes
mockMetadataTools = MetadataTools
    { symAttributes = const Mock.constructorFunctionalAttributes
    , sortAttributes = const Mock.constructorFunctionalAttributes
    , sortTools = mockSortTools
    }


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

asPureMetaPattern
    :: ProperPattern Meta sort patt => patt -> CommonMetaPattern
asPureMetaPattern patt =
    case patternKoreToPure Meta (asAst patt) of
        Left err  -> error (printError err)
        Right pat -> pat

variableRenaming
    :: MetaSort sort
    => MetaVariable sort -> MetaVariable sort -> VariableRenaming Meta
variableRenaming from to =
    VariableRenaming
        { variableRenamingOriginal = AxiomVariable (asMetaVariable from)
        , variableRenamingRenamed =
            ConfigurationVariable (asMetaVariable to)
        }

proposition_5_24_3
    :: (MetaSort sort1, ProperPattern Meta sort2 patt)
    => [FunctionalProof Meta Variable]
    -> MetaVariable sort1
    -> patt
    -> UnificationProof Meta Variable
proposition_5_24_3 functionalProof variable patt =
    Proposition_5_24_3
        functionalProof
        (asMetaVariable variable)
        (asPureMetaPattern patt)

functionalVariable
    :: MetaSort sort => MetaVariable sort -> FunctionalProof Meta Variable
functionalVariable = FunctionalVariable . asMetaVariable

runStep
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> CommonExpandedPattern level
    -- ^left-hand-side of unification
    -> [AxiomPattern level]
    -> (CommonOrOfExpandedPattern level, StepProof level)
runStep metadataTools configuration axioms =
    evalSimplifier
        $ step
            metadataTools
            Map.empty
            axioms
            (OrOfExpandedPattern.make [configuration])

runStepsPickFirst
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> MaxStepCount
    -> CommonExpandedPattern level
    -- ^left-hand-side of unification
    -> [AxiomPattern level]
    -> (CommonExpandedPattern level, StepProof level)
runStepsPickFirst metadataTools maxStepCount configuration axioms =
    evalSimplifier
        $ pickFirstStepper
            metadataTools Map.empty axioms maxStepCount (configuration, mempty)
