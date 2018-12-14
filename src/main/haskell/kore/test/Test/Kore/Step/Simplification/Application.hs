module Test.Kore.Step.Simplification.Application
    ( test_applicationSimplification
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map
import           Data.Reflection
                 ( give )
import           Data.These
                 ( These (That) )

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeEqualsPredicate, makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( bottom )
import           Kore.Step.Function.Data
import qualified Kore.Step.Function.Data as AttemptedFunction
                 ( AttemptedFunction (..) )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern, OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Application
                 ( simplify )
import           Kore.Step.Simplification.Data
                 ( CommonStepPatternSimplifier, SimplificationProof (..),
                 evalSimplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Variables.Fresh
                 ( freshVariableFromVariable )
import qualified SMT

import           Test.Kore
                 ( testId )
import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( constructorFunctionalAttributes, functionAttributes,
                 makeMetadataTools, makeSymbolOrAliasSorts )
import qualified Test.Kore.Step.MockSimplifiers as Mock
import           Test.Kore.Step.Simplifier
                 ( mockSimplifier )
import           Test.Tasty.HUnit.Extensions

test_applicationSimplification :: [TestTree]
test_applicationSimplification = give mockSymbolOrAliasSorts
    [ testCase "Application - or distribution" $ do
        -- sigma(a or b, c or d) =
        --     sigma(b, d) or sigma(b, c) or sigma(a, d) or sigma(a, c)
        let expect =
                OrOfExpandedPattern.make
                    [ Predicated
                        { term = sigma a c
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    , Predicated
                        { term = sigma a d
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    , Predicated
                        { term = sigma b c
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ,  Predicated
                        { term = sigma b d
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluate
                mockMetadataTools
                (mockSimplifier [])
                Map.empty
                (makeApplication
                    testSort
                    sigmaSymbol
                    [ [aExpanded, bExpanded]
                    , [cExpanded, dExpanded]
                    ]
                )
        assertEqualWithExplanation "" expect actual

    , testCase "Application - bottom child makes everything bottom" $ do
        -- sigma(a or b, bottom) = bottom
        let expect =
                OrOfExpandedPattern.make [ ExpandedPattern.bottom testSort ]
        actual <-
            evaluate
                mockMetadataTools
                (mockSimplifier [])
                Map.empty
                (makeApplication
                    testSort
                    sigmaSymbol
                    [ [aExpanded, bExpanded]
                    , []
                    ]
                )
        assertEqualWithExplanation "" expect actual

    , testCase "Applies functions" $ do
        -- f(a) evaluated to g(a).
        let expect = OrOfExpandedPattern.make [ gOfAExpanded ]
        actual <-
            evaluate
                mockMetadataTools
                (mockSimplifier [])
                (Map.singleton
                    fId
                    (That
                        [ ApplicationFunctionEvaluator
                            (const $ const $ const $ const $ return
                                ( AttemptedFunction.Applied
                                    (OrOfExpandedPattern.make [gOfAExpanded])
                                , SimplificationProof
                                )
                            )
                        ]
                    )
                )
                (makeApplication
                    testSort
                    fSymbol
                    [[aExpanded]]
                )
        assertEqualWithExplanation "" expect actual

    , testGroup "Combines child predicates and substitutions"
        [ testCase "When not applying functions" $ do
            -- sigma(a and f(a)=f(b) and [x=f(a)], b and g(a)=g(b) and [y=g(a)])
            --    = sigma(a, b)
            --        and (f(a)=f(b) and g(a)=g(b))
            --        and [x=f(a), y=g(a)]
            let expect =
                    OrOfExpandedPattern.make
                        [ Predicated
                            { term = sigma a b
                            , predicate =
                                makeAndPredicate
                                    (makeEqualsPredicate fOfA fOfB)
                                    (makeEqualsPredicate gOfA gOfB)
                            , substitution = Substitution.unsafeWrap
                                [ (x, fOfA)
                                , (y, gOfA)
                                ]
                            }
                        ]
            actual <-
                evaluate
                    mockMetadataTools
                    (mockSimplifier [])
                    Map.empty
                    (makeApplication
                        testSort
                        sigmaSymbol
                        [   [ Predicated
                                { term = a
                                , predicate = makeEqualsPredicate fOfA fOfB
                                , substitution = Substitution.wrap [ (x, fOfA) ]
                                }
                            ]
                        ,   [ Predicated
                                { term = b
                                , predicate = makeEqualsPredicate gOfA gOfB
                                , substitution = Substitution.wrap [ (y, gOfA) ]
                                }
                            ]
                        ]
                    )
            assertEqualWithExplanation "" expect actual

        , testCase "When applying functions" $ do
            -- sigma(a and f(a)=f(b) and [x=f(a)], b and g(a)=g(b) and [y=g(a)])
            --    =
            --        f(a) and
            --        (f(a)=f(b) and g(a)=g(b) and f(a)=g(a)) and
            --        [x=f(a), y=g(a), z=f(b)]
            -- if sigma(a, b) => f(a) and f(a)=g(a) and [z=f(b)]
            let expect =
                    OrOfExpandedPattern.make
                        [ Predicated
                            { term = fOfA
                            , predicate =
                                makeAndPredicate
                                    (makeEqualsPredicate fOfA gOfA)
                                    (makeAndPredicate
                                        (makeEqualsPredicate fOfA fOfB)
                                        (makeEqualsPredicate gOfA gOfB)
                                    )
                            , substitution = Substitution.unsafeWrap
                                [ (freshVariableFromVariable z 1, gOfB)
                                , (x, fOfA)
                                , (y, gOfA)
                                ]
                            }
                        ]
            actual <-
                evaluate
                    mockMetadataTools
                    (mockSimplifier [])
                    (Map.singleton
                        sigmaId
                        (That
                            [ ApplicationFunctionEvaluator
                                (const $ const $ const $ const $ do
                                    let zvar = freshVariableFromVariable z 1
                                    return
                                        ( AttemptedFunction.Applied
                                            (OrOfExpandedPattern.make
                                                [ Predicated
                                                    { term = fOfA
                                                    , predicate =
                                                        makeEqualsPredicate
                                                            fOfA
                                                            gOfA
                                                    , substitution =
                                                        Substitution.wrap
                                                            [ (zvar, gOfB) ]
                                                    }
                                                ]
                                            )
                                        , SimplificationProof
                                        )
                                )
                            ]
                        )
                    )
                    (makeApplication
                        testSort
                        sigmaSymbol
                        [   [ Predicated
                                { term = a
                                , predicate = makeEqualsPredicate fOfA fOfB
                                , substitution = Substitution.wrap [ (x, fOfA) ]
                                }
                            ]
                        ,   [ Predicated
                                { term = b
                                , predicate = makeEqualsPredicate gOfA gOfB
                                , substitution = Substitution.wrap [ (y, gOfA) ]
                                }
                            ]
                        ]
                    )
            assertEqualWithExplanation "" expect actual
        ]
    ]
  where
    fId = Id "f" AstLocationTest
    gId = Id "g" AstLocationTest
    sigmaId = Id "sigma" AstLocationTest
    aSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "a" AstLocationTest
        , symbolOrAliasParams      = []
        }
    bSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "b" AstLocationTest
        , symbolOrAliasParams      = []
        }
    cSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "c" AstLocationTest
        , symbolOrAliasParams      = []
        }
    dSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "d" AstLocationTest
        , symbolOrAliasParams      = []
        }
    fSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = fId
        , symbolOrAliasParams      = []
        }
    gSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = gId
        , symbolOrAliasParams      = []
        }
    sigmaSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = sigmaId
        , symbolOrAliasParams      = []
        }
    sigma
        :: StepPattern Object variable
        -> StepPattern Object variable
        -> StepPattern Object variable
    sigma child1 child2 = mkApp testSort sigmaSymbol [child1, child2]
    x = Variable (testId "x") testSort
    y = Variable (testId "y") testSort
    z = Variable (testId "z") testSort

    a = mkApp testSort aSymbol []
    b = mkApp testSort bSymbol []
    c = mkApp testSort cSymbol []
    d = mkApp testSort dSymbol []

    f x' = mkApp testSort fSymbol [x']
    g x' = mkApp testSort gSymbol [x']

    fOfA :: StepPattern Object variable
    fOfA = f a
    fOfB = f b

    gOfA :: StepPattern Object variable
    gOfA = g a
    gOfB :: StepPattern Object variable
    gOfB = g b

    aExpanded = Predicated
        { term = a
        , predicate = makeTruePredicate
        , substitution = mempty
        }
    bExpanded = Predicated
        { term = b
        , predicate = makeTruePredicate
        , substitution = mempty
        }
    cExpanded = Predicated
        { term = c
        , predicate = makeTruePredicate
        , substitution = mempty
        }
    dExpanded = Predicated
        { term = d
        , predicate = makeTruePredicate
        , substitution = mempty
        }
    gOfAExpanded :: ExpandedPattern Object variable
    gOfAExpanded = Predicated
        { term = gOfA
        , predicate = makeTruePredicate
        , substitution = mempty
        }
    symbolOrAliasSortsMapping =
        [   ( aSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( bSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( cSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( dSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( fSymbol
            , ApplicationSorts
                { applicationSortsOperands = [testSort]
                , applicationSortsResult = testSort
                }
            )
        ,   ( gSymbol
            , ApplicationSorts
                { applicationSortsOperands = [testSort]
                , applicationSortsResult = testSort
                }
            )
        ,   ( sigmaSymbol
            , ApplicationSorts
                { applicationSortsOperands = [testSort, testSort]
                , applicationSortsResult = testSort
                }
            )
        ]
    attributesMapping =
        [   ( aSymbol
            , Mock.constructorFunctionalAttributes
            )
        ,   ( bSymbol
            , Mock.constructorFunctionalAttributes
            )
        ,   ( cSymbol
            , Mock.constructorFunctionalAttributes
            )
        ,   ( dSymbol
            , Mock.constructorFunctionalAttributes
            )
        ,   ( fSymbol
            , Mock.functionAttributes
            )
        ,   ( gSymbol
            , Mock.functionAttributes
            )
        ,   ( sigmaSymbol
            , Mock.constructorFunctionalAttributes
            )
        ]
    headTypeMapping =
        [   ( aSymbol
            , HeadType.Symbol
            )
        ,   ( bSymbol
            , HeadType.Symbol
            )
        ,   ( cSymbol
            , HeadType.Symbol
            )
        ,   ( dSymbol
            , HeadType.Symbol
            )
        ,   ( fSymbol
            , HeadType.Symbol
            )
        ,   ( gSymbol
            , HeadType.Symbol
            )
        ,   ( sigmaSymbol
            , HeadType.Symbol
            )
        ]
    mockSymbolOrAliasSorts =
        Mock.makeSymbolOrAliasSorts symbolOrAliasSortsMapping
    mockMetadataTools =
        Mock.makeMetadataTools
            mockSymbolOrAliasSorts attributesMapping headTypeMapping []

makeApplication
    :: (MetaOrObject level, Ord (variable level))
    => Sort level
    -> SymbolOrAlias level
    -> [[ExpandedPattern level variable]]
    -> CofreeF
        (Application level)
        (Valid level)
        (OrOfExpandedPattern level variable)
makeApplication patternSort symbol patterns =
    (:<)
        valid
        Application
            { applicationSymbolOrAlias = symbol
            , applicationChildren = map OrOfExpandedPattern.make patterns
            }
  where
    valid = Valid { patternSort }

testSort :: Sort Object
testSort = predicateSort

evaluate
    ::  ( MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> CommonStepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomsFunctionEvaluatorMap level
    -- ^ Map from symbol IDs to defined functions
    -> CofreeF
        (Application level)
        (Valid level)
        (CommonOrOfExpandedPattern level)
    -> IO (CommonOrOfExpandedPattern level)
evaluate
    tools
    simplifier
    symbolIdToEvaluator
    application
  =
    (<$>) fst
    $ SMT.runSMT SMT.defaultConfig
    $ evalSimplifier
    $ simplify
        tools
        (Mock.substitutionSimplifier tools)
        simplifier
        symbolIdToEvaluator
        application
