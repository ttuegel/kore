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

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.Implicit.ImplicitSorts
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SymbolOrAliasSorts )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.Predicate.Predicate
                 ( makeFalsePredicate, makeTruePredicate )
import           Kore.Step.AxiomPatterns
                 ( EqualityRule (EqualityRule), RulePattern (RulePattern) )
import           Kore.Step.AxiomPatterns as RulePattern
                 ( RulePattern (..) )
import           Kore.Step.ExpandedPattern as ExpandedPattern
                 ( Predicated (..), bottom )
import           Kore.Step.Function.Data as AttemptedFunction
                 ( AttemptedFunction (..) )
import           Kore.Step.Function.Data
                 ( CommonAttemptedFunction )
import           Kore.Step.Function.UserDefined
                 ( ruleFunctionEvaluator )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( CommonStepPatternSimplifier, SimplificationProof (..),
                 evalSimplifier )
import           Kore.Step.StepperAttributes
import qualified Kore.Unification.Substitution as Substitution
import qualified SMT

import           Test.Kore
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
                        { term =
                            asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left =
                        asApplicationPattern $ metaF (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier [])
                (metaF (mkVar $ x patternMetaSort))
        assertEqualWithExplanation "f(x) => g(x)" expect actual

    , testCase "Cannot apply step with unsat axiom pre-condition" $ do
        let expect =
                AttemptedFunction.Applied (OrOfExpandedPattern.make [])
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left =
                        asApplicationPattern $ metaF (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeFalsePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier [])
                (metaF (mkVar $ x patternMetaSort))
        assertEqualWithExplanation "f(x) => g(x) requires false" expect actual

    , testCase "Cannot apply step with unsat condition" $ do
        let expect =
                (AttemptedFunction.Applied . OrOfExpandedPattern.make)
                    [ ExpandedPattern.bottom patternMetaSort ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left =
                        asApplicationPattern $ metaF (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier
                    -- Evaluate Top to Bottom.
                    [ (mkTop predicateSort, ([], SimplificationProof)) ]
                )
                (metaF (mkVar $ x patternMetaSort))
        assertEqualWithExplanation "" expect actual

    , testCase "Reevaluates the step application" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term =
                            asApplicationPattern $ metaH (mkVar $ x patternMetaSort)
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left =
                        asApplicationPattern $ metaF (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier
                    [   (   asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                        ,   (   [ Predicated
                                    { term =
                                        asApplicationPattern
                                        $ metaH (mkVar $ x patternMetaSort)
                                    , predicate = makeTruePredicate
                                    , substitution = mempty
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (metaF (mkVar $ x patternMetaSort))
        assertEqualWithExplanation "f(x) => g(x) and g(x) => h(x)" expect actual

    , testCase "Does not reevaluate the step application with incompatible condition" $ do
        let expect =
                (AttemptedFunction.Applied . OrOfExpandedPattern.make)
                    [ExpandedPattern.bottom patternMetaSort]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left =
                        asApplicationPattern $ metaF (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier
                    [   (   asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                        ,   (   [ Predicated
                                    { term =
                                        asApplicationPattern
                                        $ metaH (mkVar $ x patternMetaSort)
                                    , predicate = makeFalsePredicate
                                    , substitution = mempty
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (metaF (mkVar $ x patternMetaSort))
        assertEqualWithExplanation
            "f(x) => g(x) and g(x) => h(x) + false"
            expect
            actual

    , testCase "Preserves step substitution" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term =
                            asApplicationPattern $ metaG (mkVar $ b patternMetaSort)
                        , predicate = makeTruePredicate
                        , substitution = Substitution.wrap
                            [(a patternMetaSort, mkVar $ b patternMetaSort)]
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left  =
                        asApplicationPattern $ metaSigma
                            (mkVar $ x patternMetaSort)
                            (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier [])
                (metaSigma
                    (mkVar $ a patternMetaSort)
                    (mkVar $ b patternMetaSort)
                )
        assertEqualWithExplanation "sigma(x,x) => g(x) vs sigma(a, b)" expect actual

    , testCase "Merges the step substitution with the reevaluation one" $ do
        let expect =
                AttemptedFunction.Applied $ OrOfExpandedPattern.make
                    [ Predicated
                        { term =
                            asApplicationPattern $ metaH (mkVar $ c patternMetaSort)
                        , predicate = makeTruePredicate
                        , substitution = Substitution.wrap
                            [   ( a patternMetaSort
                                -- TODO(virgil): Do we want normalization here?
                                , mkVar $ c patternMetaSort
                                )
                            ,   ( b patternMetaSort
                                , mkVar $ c patternMetaSort
                                )
                            ]
                        }
                    ]
        actual <-
            evaluateWithAxiom
                mockMetadataTools
                (EqualityRule RulePattern
                    { left  =
                        asApplicationPattern $ metaSigma
                            (mkVar $ x patternMetaSort)
                            (mkVar $ x patternMetaSort)
                    , right =
                        asApplicationPattern $ metaG (mkVar $ x patternMetaSort)
                    , requires = makeTruePredicate
                    , attributes = def
                    }
                )
                (mockSimplifier
                    [   (   asApplicationPattern $ metaG (mkVar $ b patternMetaSort)
                        ,   (   [ Predicated
                                    { term =
                                        asApplicationPattern
                                        $ metaH (mkVar $ c patternMetaSort)
                                    , predicate = makeTruePredicate
                                    , substitution = Substitution.wrap
                                        [   ( b patternMetaSort
                                            , mkVar $ c patternMetaSort
                                            )
                                        ]
                                    }
                                ]
                            , SimplificationProof
                            )
                        )
                    ]
                )
                (metaSigma
                    (mkVar $ a patternMetaSort)
                    (mkVar $ b patternMetaSort)
                )
        assertEqualWithExplanation
            "sigma(x,x) => g(x) vs sigma(a, b) and g(b) => h(c) + a=c,b=c"
            expect
            actual
    -- TODO: Add a test for StepWithAxiom returning a condition.
    -- TODO: Add a test for the stepper giving up
    ]

mockSymbolOrAliasSorts :: SymbolOrAliasSorts Meta
mockSymbolOrAliasSorts = const ApplicationSorts
    { applicationSortsOperands = [patternMetaSort, patternMetaSort]
    , applicationSortsResult = patternMetaSort
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

x, a, b, c :: Sort Meta -> Variable Meta
x = Variable (testId "#x")
a = Variable (testId "#a")
b = Variable (testId "#b")
c = Variable (testId "#c")

fSymbol :: SymbolOrAlias Meta
fSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = testId "#f"
    , symbolOrAliasParams = []
    }

metaF
    :: CommonStepPattern Meta
    -> CofreeF (Application Meta) (Valid Meta) (CommonStepPattern Meta)
metaF p =
    valid :< Application fSymbol [p]
  where
    valid = Valid { patternSort = patternMetaSort }


gSymbol :: SymbolOrAlias Meta
gSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = testId "#g"
    , symbolOrAliasParams = []
    }

metaG
    :: CommonStepPattern Meta
    -> CofreeF (Application Meta) (Valid Meta) (CommonStepPattern Meta)
metaG p =
    valid :< Application gSymbol [p]
  where
    valid = Valid { patternSort = patternMetaSort }

hSymbol :: SymbolOrAlias Meta
hSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = testId "#h"
    , symbolOrAliasParams = []
    }

metaH
    :: CommonStepPattern Meta
    -> CofreeF (Application Meta) (Valid Meta) (CommonStepPattern Meta)
metaH p =
    valid :< Application hSymbol [p]
  where
    valid = Valid { patternSort = patternMetaSort }

sigmaSymbol :: SymbolOrAlias Meta
sigmaSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = testId "#sigma"
    , symbolOrAliasParams = []
    }

metaSigma
    :: CommonStepPattern Meta
    -> CommonStepPattern Meta
    -> CofreeF (Application Meta) (Valid Meta) (CommonStepPattern Meta)
metaSigma p1 p2 =
    valid :< Application sigmaSymbol [p1, p2]
  where
    valid = Valid { patternSort = patternMetaSort }

asApplicationPattern
    :: CofreeF (Application Meta) (Valid Meta) (CommonStepPattern Meta)
    -> CommonStepPattern Meta
asApplicationPattern (valid :< app) =
    asPurePattern (valid :< ApplicationPattern app)

evaluateWithAxiom
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -> EqualityRule level
    -> CommonStepPatternSimplifier level
    -> CofreeF (Application level) (Valid level) (CommonStepPattern level)
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
            , substitution = Substitution.modify sort substitution
            }
    evaluated =
        (<$>) fst
        $ SMT.runSMT SMT.defaultConfig
        $ evalSimplifier
        $ ruleFunctionEvaluator
            axiom
            metadataTools
            (Mock.substitutionSimplifier metadataTools)
            simplifier
            app
