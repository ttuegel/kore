module Test.Kore.Step.Simplification.Ceil
    ( test_ceilSimplification
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Data.Map.Strict as Map

import qualified Data.Sup as Sup
import Kore.Internal.Condition as Condition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeCeilPredicate_
    , makeEqualsPredicate_
    , makeTruePredicate_
    )
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( toRepresentation
    , top
    )
import qualified Kore.Internal.SideCondition.SideCondition as SideCondition
    ( Representation
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike as TermLike
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
    ( AxiomIdentifier (..)
    )
import qualified Kore.Step.Simplification.Ceil as Ceil
    ( makeEvaluate
    , simplify
    )
import Kore.Step.Simplification.Simplify
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiomResults
    ( AttemptedAxiomResults (..)
    )
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiom
    ( AttemptedAxiom (..)
    )

import Test.Kore.Step.MockSymbols
    ( testSort
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

type TermLike' = TermLike VariableName
type Pattern' = Pattern VariableName
type OrPattern' = OrPattern VariableName

test_ceilSimplification :: [TestTree]
test_ceilSimplification =
    [ testCase "Ceil - or distribution" $ do
        -- ceil(a or b) = (top and ceil(a)) or (top and ceil(b))
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate = makeCeilPredicate_ somethingOfA
                    , substitution = mempty
                    }
                , Conditional
                    { term = mkTop_
                    , predicate = makeCeilPredicate_ somethingOfB
                    , substitution = mempty
                    }
                ]
        actual <- evaluate
            (makeCeil
                [somethingOfAExpanded, somethingOfBExpanded]
            )
        assertEqual "" expected actual
    , testCase "Ceil - bool operations"
        (do
            -- ceil(top) = top
            actual1 <- evaluate
                (makeCeil
                    [Pattern.top]
                )
            assertEqual "ceil(top)"
                (OrPattern.fromPatterns
                    [ Pattern.top ]
                )
                actual1
            -- ceil(bottom) = bottom
            actual2 <- evaluate
                (makeCeil
                    []
                )
            assertEqual "ceil(bottom)"
                (OrPattern.fromPatterns
                    []
                )
                actual2
        )
    , testCase "Ceil - sorted bool operations"
        (do
            -- ceil(top{testSort}) = top
            actual1 <- evaluate
                (makeCeil
                    [Pattern.fromConditionSorted Mock.testSort Condition.top]
                )
            assertEqual "ceil(top)"
                (OrPattern.fromPatterns
                    [ Pattern.top ]
                )
                actual1
        )
    , testCase "expanded Ceil - bool operations"
        (do
            -- ceil(top) = top
            actual1 <- makeEvaluate Pattern.top
            assertEqual "ceil(top)"
                (OrPattern.fromPatterns
                    [ Pattern.top ]
                )
                actual1
            -- ceil(bottom) = bottom
            actual2 <- makeEvaluate Pattern.bottom
            assertEqual "ceil(bottom)"
                (OrPattern.fromPatterns
                    []
                )
                actual2
        )
    , testCase "ceil with predicates and substitutions" $ do
        -- if term is not functional, then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(term) and predicate) and subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                        (makeCeilPredicate_ somethingOfA)
                        (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = somethingOfA
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual "ceil(something(a) and equals(f(a), g(a)))"
            expected
            actual
    , let
        constructorTerm = Mock.constr20 somethingOfA somethingOfB
      in
        testCase "ceil with constructors" $ do
            -- if term is a non-functional-constructor(params), then
            -- ceil(term and predicate and subst)
            --     = top and (ceil(term) and predicate) and subst
            let
                expected = OrPattern.fromPatterns
                    [ Conditional
                        { term = mkTop_
                        , predicate =
                            makeAndPredicate
                                (makeAndPredicate
                                    (makeCeilPredicate_ somethingOfA)
                                    (makeCeilPredicate_ somethingOfB)
                                )
                                (makeEqualsPredicate_ fOfA gOfA)
                        , substitution =
                            Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                        }
                    ]
            actual <- makeEvaluate
                Conditional
                    { term = constructorTerm
                    , predicate = makeEqualsPredicate_ fOfA gOfA
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, fOfB)]
                    }
            assertEqual
                "ceil(constr(something(a), something(b)) and eq(f(a), g(a)))"
                expected
                actual
    , testCase "ceil of constructors is top" $ do
        let
            expected = OrPattern.fromPatterns [Pattern.top]
        actual <- makeEvaluate
            Conditional
                { term = Mock.constr10 Mock.a
                , predicate = makeTruePredicate_
                , substitution = mempty
                }
        assertEqual "" expected actual
    , testCase "ceil with functional symbols" $ do
        -- if term is a functional(params), then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(params) and predicate) and subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeAndPredicate
                                (makeCeilPredicate_ somethingOfA)
                                (makeCeilPredicate_ somethingOfB)
                            )
                            (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = Mock.functional20 somethingOfA somethingOfB
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(functional(something(a), something(b)) and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with function symbols" $ do
        -- if term is a function(params), then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(term) and predicate) and subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate_ fOfA)
                            (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = fOfA
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(f(a)) and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with functional terms" $ do
        -- if term is functional, then
        -- ceil(term and predicate and subst)
        --     = top and predicate and subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate = makeEqualsPredicate_ fOfA gOfA
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = Mock.a
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(functional and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with evaluated functional terms" $ do
        -- if term is functional, then
        -- ceil(term and predicate and subst)
        --     = top and predicate and subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate = makeEqualsPredicate_ fOfA gOfA
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = mkEvaluated Mock.a
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(functional and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with functional composition" $ do
        -- if term is functional(non-funct, non-funct), then
        -- ceil(term and predicate and subst)
        --     = top and
        --       ceil(non-funct) and ceil(non-funct) and predicate and
        --       subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeAndPredicate
                                (makeCeilPredicate_ fOfA)
                                (makeCeilPredicate_ fOfB)
                            )
                            (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = Mock.functional20 fOfA fOfB
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(functional(non-funct, non-funct) and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with axioms" $ do
        -- if term is functional(non-funct, non-funct), then
        -- ceil(term and predicate and subst)
        --     = top and
        --       ceil(non-funct) and ceil(non-funct) and predicate and
        --       subst
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeEqualsPredicate_ Mock.a Mock.cf)
                            (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.unsafeWrap [(inject Mock.x, fOfB)]
                    }
                ]
        actual <- makeEvaluateWithAxioms
            (Map.singleton
                (AxiomIdentifier.Ceil
                    (AxiomIdentifier.Application Mock.fId)
                )
                (appliedMockEvaluator
                    Conditional
                        { term = mkTop_
                        , predicate = makeEqualsPredicate_ Mock.a Mock.cf
                        , substitution = mempty
                        }
                )
            )
            Conditional
                { term = Mock.functional20 fOfA fOfB
                , predicate = makeEqualsPredicate_ fOfA gOfA
                , substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [(inject Mock.x, fOfB)]
                }
        assertEqual
            "ceil(functional(non-funct, non-funct) and eq(f(a), g(a)))"
            expected
            actual
    , testCase "ceil with normal domain value" $ do
        -- ceil(1) = top
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate = makeTruePredicate_
                    , substitution = mempty
                    }
                ]
        actual <- makeEvaluate
            $ Pattern.fromTermLike
            $ mkDomainValue DomainValue
                { domainValueSort = Mock.testSort
                , domainValueChild = mkStringLiteral "a"
                }
        assertEqual "ceil(1)" expected actual
    , testCase "ceil with list domain value" $ do
        -- ceil([a, b]) = ceil(a) and ceil(b)
        let
            expected = OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate_ fOfA)
                            (makeCeilPredicate_ fOfB)
                    , substitution = mempty
                    }
                ]
        actual <- makeEvaluate
            Conditional
                { term = Mock.builtinList [fOfA, fOfB]
                , predicate = makeTruePredicate_
                , substitution = mempty
                }
        assertEqual "ceil(list)" expected actual
    , testCase "ceil of sort injection" $ do
        let expected =
                OrPattern.fromPattern Conditional
                    { term = mkTop_
                    , predicate = makeCeilPredicate_ fOfA
                    , substitution = mempty
                    }
        actual <- (makeEvaluate . Pattern.fromTermLike)
            (Mock.sortInjection Mock.topSort (TermLike.markSimplified fOfA))
        assertEqual "ceil(f(a))" expected actual
        assertBool "simplified"
            (OrPattern.isSimplified sideRepresentation actual)
    ]
  where
    fOfA :: TermLike'
    fOfA = Mock.f Mock.a
    fOfB :: TermLike'
    fOfB = Mock.f Mock.b
    gOfA = Mock.g Mock.a
    somethingOfA = Mock.plain10 Mock.a
    somethingOfB = Mock.plain10 Mock.b
    somethingOfAExpanded = Conditional
        { term = somethingOfA
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    somethingOfBExpanded = Conditional
        { term = somethingOfB
        , predicate = makeTruePredicate_
        , substitution = mempty
        }

appliedMockEvaluator
    :: Pattern' -> BuiltinAndAxiomSimplifier
appliedMockEvaluator result =
    BuiltinAndAxiomSimplifier
    $ mockEvaluator
    $ AttemptedAxiom.Applied AttemptedAxiomResults
        { results = OrPattern.fromPatterns
            [Test.Kore.Step.Simplification.Ceil.mapVariables result]
        , remainders = OrPattern.fromPatterns []
        }

mockEvaluator
    :: MonadSimplify simplifier
    => AttemptedAxiom variable
    -> TermLike variable
    -> SideCondition variable
    -> simplifier (AttemptedAxiom variable)
mockEvaluator evaluation _ _ = return evaluation

mapVariables
    :: forall variable
    .  InternalVariable variable
    => Pattern'
    -> Pattern variable
mapVariables =
    Pattern.mapVariables (pure worker)
  where
    worker :: VariableName -> variable
    worker v = fromVariableName v { counter = Just (Sup.Element 1) }

makeCeil
    :: InternalVariable variable
    => [Pattern variable]
    -> Ceil Sort (OrPattern variable)
makeCeil patterns =
    Ceil
        { ceilOperandSort = testSort
        , ceilResultSort  = testSort
        , ceilChild       = OrPattern.fromPatterns patterns
        }

evaluate
    :: Ceil Sort (OrPattern')
    -> IO (OrPattern')
evaluate ceil =
    runSimplifier mockEnv
    $ Ceil.simplify SideCondition.top ceil
  where
    mockEnv = Mock.env

makeEvaluate
    :: Pattern'
    -> IO (OrPattern')
makeEvaluate = makeEvaluateWithAxioms Map.empty

makeEvaluateWithAxioms
    :: BuiltinAndAxiomSimplifierMap
    -- ^ Map from symbol IDs to defined functions
    -> Pattern'
    -> IO (OrPattern')
makeEvaluateWithAxioms axiomIdToSimplifier child =
    runSimplifier mockEnv
    $ Ceil.makeEvaluate SideCondition.top child
  where
    mockEnv = Mock.env { simplifierAxioms = axiomIdToSimplifier }

sideRepresentation :: SideCondition.Representation
sideRepresentation =
    SideCondition.toRepresentation
    (SideCondition.top :: SideCondition VariableName)
