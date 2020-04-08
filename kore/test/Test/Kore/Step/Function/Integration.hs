{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Kore.Step.Function.Integration
    ( test_functionIntegration
    , test_Nat
    , test_short_circuit
    , test_List
    , test_lookupMap
    , test_updateMap
    , test_updateList
    , test_Ceil
    ) where

import Prelude.Kore hiding
    ( succ
    )

import Test.Tasty

import qualified Control.Lens as Lens
import Data.Generics.Product
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import qualified Data.Text.Prettyprint.Doc as Pretty

import Data.Sup
import Kore.ASTVerifier.DefinitionVerifier
import Kore.ASTVerifier.Error
    ( VerifyError
    )
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin as Builtin
import qualified Kore.Builtin.AssociativeCommutative as Ac
import qualified Kore.Builtin.Int as Int
    ( builtinFunctions
    )
import qualified Kore.Builtin.Map as Map
    ( builtinFunctions
    )
import Kore.Equation
import qualified Kore.Error
import Kore.IndexedModule.IndexedModule as IndexedModule
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    )
import qualified Kore.IndexedModule.MetadataToolsBuilder as MetadataTools
import qualified Kore.IndexedModule.SortGraph as SortGraph
import qualified Kore.Internal.Condition as Condition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeCeilPredicate
    , makeCeilPredicate_
    , makeEqualsPredicate
    , makeEqualsPredicate_
    , makeTruePredicate
    , makeTruePredicate_
    )
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.Symbol
import Kore.Internal.TermLike
import Kore.Step.Axiom.EvaluationStrategy
    ( builtinEvaluation
    , definitionEvaluation
    , firstFullEvaluation
    , simplificationEvaluation
    , simplifierWithFallback
    )
import Kore.Step.Axiom.Identifier
    ( AxiomIdentifier
    )
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
import qualified Kore.Step.Function.Memo as Memo
import qualified Kore.Step.Simplification.Condition as Simplifier.Condition
import Kore.Step.Simplification.InjSimplifier
import qualified Kore.Step.Simplification.Simplifier as Simplifier
import Kore.Step.Simplification.Simplify
import Kore.Step.Simplification.Simplify as AttemptedAxiom
    ( AttemptedAxiom (..)
    )
import qualified Kore.Step.Simplification.SubstitutionSimplifier as SubstitutionSimplifier
import qualified Kore.Step.Simplification.TermLike as TermLike
import Kore.Syntax.Definition hiding
    ( Symbol (..)
    )
import Kore.Unparser
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
    )

import Test.Kore
import qualified Test.Kore.Builtin.Bool as Bool
import qualified Test.Kore.Builtin.Builtin as Builtin
import qualified Test.Kore.Builtin.Definition as Builtin
import qualified Test.Kore.Builtin.Int as Int
import qualified Test.Kore.Builtin.List as List
import qualified Test.Kore.Builtin.Map as Map
import Test.Kore.Equation.Application
    ( axiom
    , axiom_
    )
import Test.Kore.Step.Axiom.Matcher
    ( doesn'tMatch
    , matches
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

test_functionIntegration :: [TestTree]
test_functionIntegration =
    [ testCase "Simple evaluation" $ do
        let expect =
                Conditional
                    { term = Mock.g Mock.c
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    (axiomEvaluator
                        (Mock.functional10 (mkElemVar Mock.x))
                        (Mock.g (mkElemVar Mock.x))
                    )
                )
                (Mock.functional10 Mock.c)
        assertEqual "" expect actual

    , testCase "Simple evaluation (builtin branch)" $ do
        let expect =
                Conditional
                    { term = Mock.g Mock.c
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    (builtinEvaluation $ axiomEvaluator
                        (Mock.functional10 (mkElemVar Mock.x))
                        (Mock.g (mkElemVar Mock.x))
                    )
                )
                (Mock.functional10 Mock.c)
        assertEqual "" expect actual

    , testCase "Simple evaluation (Axioms & Builtin branch, Builtin works)"
      $ do
        let expect =
                Conditional
                    { term = Mock.g Mock.c
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    (simplifierWithFallback
                        (builtinEvaluation $ axiomEvaluator
                            (Mock.functional10 (mkElemVar Mock.x))
                            (Mock.g (mkElemVar Mock.x))
                        )
                        ( axiomEvaluator
                            (Mock.functional10 (mkElemVar Mock.x))
                            (mkElemVar Mock.x)
                        )
                    )
                )
                (Mock.functional10 Mock.c)
        assertEqual "" expect actual

    , testCase "Simple evaluation (Axioms & Builtin branch, Builtin fails)"
      $ do
        let expect =
                Conditional
                    { term = Mock.g Mock.c
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    (simplifierWithFallback
                        (builtinEvaluation $ BuiltinAndAxiomSimplifier $ \_ _ ->
                            notApplicableAxiomEvaluator
                        )
                        ( axiomEvaluator
                            (Mock.functional10 (mkElemVar Mock.x))
                            (Mock.g (mkElemVar Mock.x))
                        )
                    )
                )
                (Mock.functional10 Mock.c)
        assertEqual "" expect actual

    , testCase "Evaluates inside functions" $ do
        let expect =
                Conditional
                    { term = Mock.functional11 (Mock.functional11 Mock.c)
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    ( axiomEvaluator
                        (Mock.functional10 (mkElemVar Mock.x))
                        (Mock.functional11 (mkElemVar Mock.x))
                    )
                )
                (Mock.functional10 (Mock.functional10 Mock.c))
        assertEqual "" expect actual

    , testCase "Evaluates 'or'" $ do
        let expect =
                Conditional
                    { term =
                        mkOr
                            (Mock.functional11 (Mock.functional11 Mock.c))
                            (Mock.functional11 (Mock.functional11 Mock.d))
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    ( axiomEvaluator
                        (Mock.functional10 (mkElemVar Mock.x))
                        (Mock.functional11 (mkElemVar Mock.x))
                    )
                )
                (Mock.functional10
                    (mkOr
                        (Mock.functional10 Mock.c)
                        (Mock.functional10 Mock.d)
                    )
                )
        assertEqual "" expect actual

    , testCase "Evaluates on multiple branches" $ do
        let expect =
                Conditional
                    { term =
                        Mock.functional11
                            (Mock.functional20
                                (Mock.functional11 Mock.c)
                                (Mock.functional11 Mock.c)
                            )
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.functional10Id)
                    ( axiomEvaluator
                        (Mock.functional10 (mkElemVar Mock.x))
                        (Mock.functional11 (mkElemVar Mock.x))
                    )
                )
                (Mock.functional10
                    (Mock.functional20
                        (Mock.functional10 Mock.c)
                        (Mock.functional10 Mock.c)
                    )
                )
        assertEqual "" expect actual

    , testCase "Returns conditions" $ do
        let expect =
                Conditional
                    { term = Mock.f Mock.d
                    , predicate = makeCeilPredicate Mock.testSort
                        (Mock.plain10 Mock.e)
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.singleton
                    (AxiomIdentifier.Application Mock.cfId)
                    ( appliedMockEvaluator Conditional
                        { term   = Mock.d
                        , predicate =
                            makeCeilPredicate_
                                (Mock.plain10 Mock.e)
                        , substitution = mempty
                        }
                    )
                )
                (Mock.f Mock.cf)
        assertEqual "" expect actual

    , testCase "Merges conditions" $ do
        let expect =
                Conditional
                    { term = Mock.functional11 (Mock.functional20 Mock.e Mock.e)
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate Mock.testSort (Mock.f Mock.a))
                            (makeCeilPredicate_ (Mock.g Mock.a))
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.cfId
                        , appliedMockEvaluator Conditional
                            { term = Mock.e
                            , predicate = makeCeilPredicate_ (Mock.g Mock.a)
                            , substitution = mempty
                            }
                        )
                    ,   ( AxiomIdentifier.Application Mock.cgId
                        , appliedMockEvaluator Conditional
                            { term = Mock.e
                            , predicate = makeCeilPredicate_ (Mock.f Mock.a)
                            , substitution = mempty
                            }
                        )
                    ,   ( AxiomIdentifier.Application Mock.functional10Id
                        , axiomEvaluator
                            (Mock.functional10 (mkElemVar Mock.x))
                            (Mock.functional11 (mkElemVar Mock.x))
                        )
                    ]
                )
                (Mock.functional10 (Mock.functional20 Mock.cf Mock.cg))
        assertEqual "" expect actual

    , testCase "Reevaluates user-defined function results." $ do
        let expect =
                Conditional
                    { term = Mock.f Mock.e
                    , predicate = makeEqualsPredicate Mock.testSort
                        (Mock.f Mock.e) Mock.e
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.cfId
                        , axiomEvaluator Mock.cf Mock.cg
                        )
                    ,   ( AxiomIdentifier.Application Mock.cgId
                        , appliedMockEvaluator Conditional
                            { term = Mock.e
                            , predicate =
                                makeEqualsPredicate_ (Mock.f Mock.e) Mock.e
                            , substitution = mempty
                            }
                        )
                    ]
                )
                (Mock.f Mock.cf)
        assertEqual "" expect actual

    , testCase "Merges substitutions with reevaluation ones." $ do
        let expect =
                Conditional
                    { term = Mock.f Mock.e
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = Substitution.unsafeWrap
                        [   ( ElemVar Mock.var_x_1
                            , Mock.a
                            )
                        ,   ( ElemVar Mock.var_z_1
                            , Mock.a
                            )
                        ]
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.cfId
                        , appliedMockEvaluator Conditional
                            { term = Mock.cg
                            , predicate = makeTruePredicate_
                            , substitution = Substitution.unsafeWrap
                                [   ( ElemVar Mock.x
                                    , mkElemVar Mock.z
                                    )
                                ]
                            }
                        )
                    ,   ( AxiomIdentifier.Application Mock.cgId
                        , appliedMockEvaluator Conditional
                            { term = Mock.e
                            , predicate = makeTruePredicate_
                            , substitution = Substitution.unsafeWrap
                                [   ( ElemVar Mock.x
                                    , Mock.a
                                    )
                                ]
                            }
                        )
                    ]
                )
                (Mock.f Mock.cf)
        assertEqual "" expect actual

    , testCase "Simplifies substitution-predicate." $ do
        -- Mock.plain10 below prevents:
        -- 1. unification without substitution.
        -- 2. Transforming the 'and' in an equals predicate,
        --    as it would happen for functions.
        let expect =
                Conditional
                    { term = Mock.a
                    , predicate = makeAndPredicate
                        (makeCeilPredicate Mock.testSort Mock.cf)
                        (makeCeilPredicate Mock.testSort
                            (Mock.plain10 Mock.cf)
                        )
                    , substitution = Substitution.unsafeWrap
                        [ (ElemVar Mock.var_x_1, Mock.cf)
                        , (ElemVar Mock.var_y_1, Mock.b)
                        ]
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.fId
                        , appliedMockEvaluator Conditional
                            { term = Mock.a
                            , predicate =
                                makeCeilPredicate_
                                    (mkAnd
                                        (Mock.constr20
                                            (Mock.plain10 Mock.cf)
                                            Mock.b
                                        )
                                        (Mock.constr20
                                            (Mock.plain10 (mkElemVar Mock.x))
                                            (mkElemVar Mock.y)
                                        )
                                    )
                            , substitution =
                                Substitution.wrap
                                $ Substitution.mkUnwrappedSubstitution
                                [(ElemVar Mock.x, Mock.cf)]
                            }
                        )
                    ]
                )
                (Mock.f (mkElemVar Mock.x))
        let message =
                (show . Pretty.vsep)
                    [ "Expected:"
                    , Pretty.indent 4 (unparse expect)
                    , "but found:"
                    , Pretty.indent 4 (unparse actual)
                    ]
        assertEqual message expect actual

    , testCase "Evaluates only simplifications." $ do
        let expect =
                Conditional
                    { term = Mock.b
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.fId
                        , simplifierWithFallback
                            (appliedMockEvaluator (Pattern.fromTermLike Mock.b))
                            (definitionEvaluation
                                [axiom_ (Mock.f (mkElemVar Mock.y)) Mock.a]
                            )
                        )
                    ]
                )
                (Mock.f (mkElemVar Mock.x))
        assertEqual "" expect actual

    , testCase "Picks first matching simplification." $ do
        let expect =
                Conditional
                    { term = Mock.b
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.fId
                        , simplifierWithFallback
                            (firstFullEvaluation
                                [ axiomEvaluator
                                    (Mock.f (Mock.g (mkElemVar Mock.x)))
                                    Mock.c
                                ,  appliedMockEvaluator Conditional
                                    { term = Mock.b
                                    , predicate = makeTruePredicate_
                                    , substitution = mempty
                                    }
                                ,  appliedMockEvaluator Conditional
                                    { term = Mock.c
                                    , predicate = makeTruePredicate_
                                    , substitution = mempty
                                    }
                                ]
                            )
                            (definitionEvaluation
                                [ axiom
                                    (Mock.f (mkElemVar Mock.y))
                                    Mock.a
                                    makeTruePredicate_
                                ]
                            )
                        )
                    ]
                )
                (Mock.f (mkElemVar Mock.x))
        assertEqual "" expect actual

    , testCase "Falls back to evaluating the definition." $ do
        let expect =
                Conditional
                    { term = Mock.a
                    , predicate = makeTruePredicate Mock.testSort
                    , substitution = mempty
                    }
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.fId
                        , simplifierWithFallback
                            (axiomEvaluator
                                (Mock.f (Mock.g (mkElemVar Mock.x)))
                                Mock.b
                            )
                            (definitionEvaluation
                                [ axiom
                                    (Mock.f (mkElemVar Mock.y))
                                    Mock.a
                                    makeTruePredicate_
                                ]
                            )
                        )
                    ]
                )
                (Mock.f (mkElemVar Mock.x))
        assertEqual "" expect actual
{-
    Uncomment this if we ever go back to allowing branches for function
    evaluation.

    , testCase "Multiple definition branches." $ do
        let expect =
                Pattern.fromTermLike $ mkOr
                    (mkAnd Mock.a (mkCeil Mock.testSort Mock.cf))
                    (mkAnd Mock.b (mkNot (mkCeil Mock.testSort Mock.cf)))
        actual <-
            evaluate
                (Map.fromList
                    [   ( AxiomIdentifier.Application Mock.fId
                        , simplifierWithFallback
                            (axiomEvaluator
                                (Mock.f (Mock.g (mkElemVar Mock.x)))
                                Mock.c
                            )
                            (definitionEvaluation
                                [ axiom
                                    (Mock.f (mkElemVar Mock.y))
                                    Mock.a
                                    (makeCeilPredicate_ Mock.cf)
                                , axiom_ (Mock.f (mkElemVar Mock.y)) Mock.b
                                ]
                            )
                        )
                    ]
                )
                (Mock.f (mkElemVar Mock.x))
        assertEqual "" expect actual-}
    ]
  where
    evaluate
        :: BuiltinAndAxiomSimplifierMap
        -> TermLike Variable
        -> IO (Pattern Variable)
    evaluate functionIdToEvaluator patt =
        runSimplifier Mock.env { simplifierAxioms = functionIdToEvaluator }
        $ TermLike.simplify patt SideCondition.top

test_Nat :: [TestTree]
test_Nat =
    [ matches "plus(0, N) matches plus(0, 1)"
        (plus zero varN)
        (plus zero one)
        [(ElemVar natN, one)]
    , doesn'tMatch "plus(succ(M), N) doesn't match plus(0, 1)"
        (plus (succ varM) varN)
        (plus zero one)
    , matches "plus(succ(M), N) matches plus(1, 1)"
        (plus (succ varM) varN)
        (plus one one)
        [(ElemVar natM, zero), (ElemVar natN, one)]
    , applies            "plus(0, N) => ... ~ plus (0, 1)"
        [plusZeroRule]
        (plus zero one)
    , notApplies         "plus(0, N) => ... ~ plus (1, 1)"
        [plusZeroRule]
        (plus one one)
    , notApplies         "plus(Succ(M), N) => ... ~ plus (0, 1)"
        [plusSuccRule]
        (plus zero one)
    , applies            "plus(Succ(M), N) => ... ~ plus (1, 1)"
        [plusSuccRule]
        (plus one one)
    , applies            "plus(0, 1) => ..."
        plusRules
        (plus zero one)
    , applies            "plus(1, 1) => ..."
        plusRules
        (plus one one)
    , equals "0 + 1 = 1 : Nat" (plus zero one) [one]
    , equals "0 + 1 = 1 : Nat" (plus one one) [two]
    , equals "0 * 1 = 0 : Nat" (times zero one) [zero]
    , equals "1 * 1 = 1 : Nat" (times one one) [one]
    , equals "1 * 2 = 2 : Nat" (times one two) [two]
    , equals "2 * 1 = 2 : Nat" (times two one) [two]
    , equals "0! = 1 : Nat" (factorial zero) [one]
    , equals "1! = 1 : Nat" (factorial one) [one]
    , equals "2! = 2 : Nat" (factorial two) [two]
    , equals "fibonacci(0) = 1 : Nat" (fibonacci zero) [one]
    , equals "fibonacci(1) = 1 : Nat" (fibonacci one) [one]
    , equals "fibonacci(2) = 2 : Nat" (fibonacci two) [two]
    ]

-- Evaluation tests: check the result of evaluating the term
equals
    :: HasCallStack
    => TestName
    -> TermLike Variable
    -> [TermLike Variable]
    -> TestTree
equals comment term results =
    testCase comment $ do
        actual <- simplify term
        let expect = OrPattern.fromPatterns $ Pattern.fromTermLike <$> results
        assertEqual "" expect actual

simplify :: TermLike Variable -> IO (OrPattern Variable)
simplify = runSimplifier testEnv . TermLike.simplifyToOr SideCondition.top

evaluateWith
    :: BuiltinAndAxiomSimplifier
    -> TermLike Variable
    -> IO CommonAttemptedAxiom
evaluateWith simplifier patt =
    runSimplifier testEnv
    $ runBuiltinAndAxiomSimplifier simplifier patt SideCondition.top

-- Applied tests: check that one or more rules applies or not
withApplied
    :: (CommonAttemptedAxiom -> Assertion)
    -> TestName
    -> [Equation Variable]
    -> TermLike Variable
    -> TestTree
withApplied check comment rules term =
    testCase comment $ do
        actual <- evaluateWith (definitionEvaluation rules) term
        check actual

applies, notApplies
    :: TestName
    -> [Equation Variable]
    -> TermLike Variable
    -> TestTree
applies =
    withApplied $ \attempted -> do
        results <- expectApplied attempted
        expectNoRemainders results
  where
    expectApplied NotApplicable = assertFailure "Expected Applied"
    expectApplied (NotApplicableUntilConditionChanges _) =
        assertFailure "Expected Applied"
    expectApplied (Applied results) = return results
    expectNoRemainders =
        assertBool "Expected no remainders"
        . isBottom
        . Lens.view (field @"remainders")
notApplies =
    withApplied $ \r ->
        assertBool "Expected NotApplicable"
        $ isNotApplicable r || isNotApplicableUntilConditionChanges r

natSort :: Sort
natSort =
    SortActualSort SortActual
        { sortActualName = testId "Nat"
        , sortActualSorts = []
        }

natM, natN :: ElementVariable Variable
natM = elemVarS "M" natSort
natN = elemVarS "N" natSort

varM, varN :: TermLike Variable
varM = mkElemVar natM
varN = mkElemVar natN

zeroSymbol, succSymbol :: Symbol
zeroSymbol = Mock.symbol "Zero" [] natSort & constructor & functional
succSymbol = Mock.symbol "Succ" [natSort] natSort & constructor & functional

plusSymbol, timesSymbol :: Symbol
plusSymbol = Mock.symbol "plus" [natSort, natSort] natSort & function
timesSymbol = Mock.symbol "times" [natSort, natSort] natSort & function

fibonacciSymbol, factorialSymbol :: Symbol
fibonacciSymbol = Mock.symbol "fibonacci" [natSort] natSort & function
factorialSymbol = Mock.symbol "factorial" [natSort] natSort & function

zero :: TermLike Variable
zero = mkApplySymbol zeroSymbol []

one, two :: TermLike Variable
one = succ zero
two = succ one

succ, fibonacci, factorial :: TermLike Variable -> TermLike Variable
succ n = mkApplySymbol succSymbol [n]
fibonacci n = mkApplySymbol fibonacciSymbol [n]
factorial n = mkApplySymbol factorialSymbol [n]

plus, times
    :: TermLike Variable
    -> TermLike Variable
    -> TermLike Variable
plus n1 n2 = mkApplySymbol plusSymbol [n1, n2]
times n1 n2 = mkApplySymbol timesSymbol [n1, n2]

functionEvaluator
    :: Symbol
    -> [Equation Variable]  -- ^ Function definition rules
    -> (AxiomIdentifier, BuiltinAndAxiomSimplifier)
functionEvaluator symb rules =
    (AxiomIdentifier.Application ident, definitionEvaluation rules)
  where
    ident = symbolConstructor symb

functionSimplifier
    :: Symbol
    -> [Equation Variable]  -- ^ Function simplification rule
    -> (AxiomIdentifier, BuiltinAndAxiomSimplifier)
functionSimplifier symb rules =
    ( AxiomIdentifier.Application ident
    , firstFullEvaluation (simplificationEvaluation <$> rules)
    )
  where
    ident = symbolConstructor symb

plusZeroRule, plusSuccRule :: Equation Variable
plusZeroRule = axiom_ (plus zero varN) varN
plusSuccRule = axiom_ (plus (succ varM) varN) (succ (plus varM varN))


plusRules :: [Equation Variable]
plusRules = [plusZeroRule, plusSuccRule]

plusEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
plusEvaluator = functionEvaluator plusSymbol plusRules

timesEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
timesEvaluator =
    functionEvaluator timesSymbol
        [ axiom_ (times zero varN) zero
        , axiom_ (times (succ varM) varN) (plus varN (times varM varN))
        ]

fibonacciEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
fibonacciEvaluator =
    functionEvaluator fibonacciSymbol
        [ axiom_ (fibonacci zero) one
        , axiom_ (fibonacci one)  one
        , axiom_
            (fibonacci (succ (succ varN)))
            (plus (fibonacci (succ varN)) (fibonacci varN))
        ]

factorialEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
factorialEvaluator =
    functionEvaluator factorialSymbol
        [ axiom_ (factorial zero)        (succ zero)
        , axiom_ (factorial (succ varN)) (times (succ varN) (factorial varN))
        ]

natSimplifiers :: BuiltinAndAxiomSimplifierMap
natSimplifiers =
    Map.fromList
        [ plusEvaluator
        , timesEvaluator
        , fibonacciEvaluator
        , factorialEvaluator
        ]

-- | Add an unsatisfiable requirement to the 'Equation'.
requiresBottom :: Equation Variable -> Equation Variable
requiresBottom equation = equation { requires = makeEqualsPredicate_ zero one }

{- | Add an unsatisfiable @\\equals@ requirement to the 'Equation'.

In contrast to 'requiresBottom', @requiresFatalEquals@ also includes a
requirement which results in a fatal error when evaluated.

 -}
requiresFatalEquals :: Equation Variable -> Equation Variable
requiresFatalEquals equation =
    equation
        { requires =
            makeAndPredicate
                (makeEqualsPredicate_ (fatal zero) one)
                (makeEqualsPredicate_ zero         one)
        }

{- | Add an unsatisfiable @\\in@ requirement to the 'Equation'.

In contrast to 'requiresBottom', @requiresFatalEquals@ also includes a
requirement which results in a fatal error when evaluated.

 -}
requiresFatalIn :: Equation Variable -> Equation Variable
requiresFatalIn equation =
    equation
        { requires =
            makeAndPredicate
                (makeEqualsPredicate_ (fatal zero) one)
                (makeCeilPredicate_ (mkAnd zero one))
        }

{- | Test short-circuiting evaluation of function requirements.

We want to check that functions are not evaluated in an 'Equation'
requirement if the pre-condition is known to be unsatisfiable without function
evaluation. We check this by including a 'requires' clause with one
unsatisfiable condition and one "fatal" condition (a condition producing a fatal
error if evaluated). If we do function evaluation on the unsatisfiable
requirement, a fatal error will be produced.

 -}
test_short_circuit :: [TestTree]
test_short_circuit =
    [ notApplies  "requires 0 = 1 does not apply"
        [requiresBottom plusZeroRule]
        (plus zero one)
    , notApplies  "requires fatal(0) = 1 ∧ 0 = 1 does not apply"
        [requiresFatalEquals plusZeroRule]
        (plus zero one)
    , notApplies  "requires fatal(0) = 1 ∧ 0 ∈ 1 does not apply"
        [requiresFatalIn plusZeroRule]
        (plus zero one)
    ]

{- | A symbol which throws a fatal error when evaluated.

@fatalSymbol@ is useful for checking that symbols in certain positions are never
evaluated.

 -}
fatalSymbol :: Symbol
fatalSymbol = Mock.symbol "fatal" [natSort] natSort & function

fatal :: TermLike Variable -> TermLike Variable
fatal x = mkApplySymbol fatalSymbol [x]

fatalEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
fatalEvaluator =
    ( AxiomIdentifier.Application ident
    , BuiltinAndAxiomSimplifier $ \_ _ -> error "fatal error"
    )
  where
    ident = symbolConstructor fatalSymbol

fatalSimplifiers :: BuiltinAndAxiomSimplifierMap
fatalSimplifiers = uncurry Map.singleton fatalEvaluator

test_List :: [TestTree]
test_List =
    [ applies                  "lengthList([]) => ... ~ lengthList([])"
        [lengthListUnitRule]
        (lengthList unitList)
    , notApplies               "lengthList([]) => ... ~ lengthList(L)"
        [lengthListUnitRule]
        (lengthList varL)
    , notApplies               "lengthList([]) => ... !~ lengthList([1])"
        [lengthListUnitRule]
        (lengthList (mkList [mkInt 1]))
    , notApplies               "lengthList([]) => ... !~ lengthList([1, 2])"
        [lengthListUnitRule]
        (lengthList (mkList [mkInt 1, mkInt 2]))
    , notApplies               "lengthList(x : xs) => ... !~ lengthList([])"
        [lengthListConsRule]
        (lengthList unitList)
    , notApplies               "lengthList(x : xs) => ... !~ lengthList(L)"
        [lengthListConsRule]
        (lengthList varL)
    , applies                  "lengthList(x : xs) => ... ~ lengthList([1])"
        [lengthListConsRule]
        (lengthList (mkList [mkInt 1]))
    , applies                  "lengthList(x : xs) => ... ~ lengthList([1, 2])"
        [lengthListConsRule]
        (lengthList (mkList [mkInt 1, mkInt 2]))
    , applies                  "lengthList([]) => ..."
        lengthListRules
        (lengthList unitList)
    , applies                  "lengthList([1]) => ..."
        lengthListRules
        (lengthList (mkList [mkInt 1]))
    , applies                  "lengthList([12]) => ..."
        lengthListRules
        (lengthList (mkList [mkInt 1, mkInt 2]))
    , equals                   "lengthList([]) = 0 : Int"
        (lengthList (mkList []))
        [mkInt 0]
    , equals                   "lengthList([1]) = 1 : Int"
        (lengthList (mkList [mkInt 1]))
        [mkInt 1]
    , equals                   "lengthList([1, 2]) = 2 : Int"
        (lengthList (mkList [mkInt 1, mkInt 2]))
        [mkInt 2]

    , applies                  "removeList([], M) => ... ~ removeList([], [(0, 1)])"
        [removeListUnitRule]
        (removeList unitList (mkMap [(mkInt 0, mkInt 1)] []))
    , equals "removeList([1], [(0, 1)]) = [(0, 1)]"
        (removeList (mkList [mkInt 1]) (mkMap [(mkInt 0, mkInt 1)] []))
        [mkMap [(mkInt 0, mkInt 1)] []]
    ]

listSort, intSort, mapSort :: Sort
listSort = Builtin.listSort
intSort = Builtin.intSort
mapSort = Builtin.mapSort

mkList :: [TermLike Variable] -> TermLike Variable
mkList = List.asInternal

mkInt :: Integer -> TermLike Variable
mkInt = Int.asInternal

mkMap
    :: [(TermLike Variable, TermLike Variable)]
    -> [TermLike Variable]
    -> TermLike Variable
mkMap elements opaques =
    Ac.asInternal Builtin.testMetadataTools Builtin.mapSort
    $ Map.normalizedMap elements opaques

removeMap :: TermLike Variable -> TermLike Variable -> TermLike Variable
removeMap = Builtin.removeMap

addInt :: TermLike Variable -> TermLike Variable -> TermLike Variable
addInt = Builtin.addInt

unitList :: TermLike Variable
unitList = mkList []

varX, varY, varL, mMapTerm :: TermLike Variable
varX = mkElemVar xInt
varY = mkElemVar yInt
varL = mkElemVar (elemVarS (testId "lList") listSort)
mMapTerm = mkElemVar mMap

mMap :: ElementVariable Variable
mMap = elemVarS (testId "mMap") mapSort

lengthListSymbol :: Symbol
lengthListSymbol = Mock.symbol "lengthList" [listSort] intSort & function

lengthList :: TermLike Variable -> TermLike Variable
lengthList l = mkApplySymbol lengthListSymbol [l]

concatList :: TermLike Variable -> TermLike Variable -> TermLike Variable
concatList = Builtin.concatList

consList :: TermLike Variable -> TermLike Variable -> TermLike Variable
consList x xs = concatList (mkList [x]) xs

lengthListUnitRule, lengthListConsRule :: Equation Variable
lengthListUnitRule = axiom_ (lengthList unitList) (mkInt 0)
lengthListConsRule =
    axiom_
        (lengthList (consList varX varL))
        (addInt (mkInt 1) (lengthList varL))

lengthListRules :: [Equation Variable]
lengthListRules = [ lengthListUnitRule , lengthListConsRule ]

lengthListEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
lengthListEvaluator = functionEvaluator lengthListSymbol lengthListRules

removeListSymbol :: Symbol
removeListSymbol =
    Mock.symbol "removeList" [listSort, mapSort] mapSort & function

removeList :: TermLike Variable -> TermLike Variable -> TermLike Variable
removeList l m = mkApplySymbol removeListSymbol [l, m]

removeListUnitRule, removeListConsRule :: Equation Variable
removeListUnitRule = axiom_ (removeList unitList mMapTerm) mMapTerm
removeListConsRule =
    axiom_
        (removeList (consList varX varL) mMapTerm)
        (removeMap mMapTerm varX)

removeListRules :: [Equation Variable]
removeListRules = [removeListUnitRule, removeListConsRule]

removeListEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
removeListEvaluator = functionEvaluator removeListSymbol removeListRules

listSimplifiers :: BuiltinAndAxiomSimplifierMap
listSimplifiers =
    Map.fromList
        [ lengthListEvaluator
        , removeListEvaluator
        , (addIntId, builtinEvaluation addIntEvaluator)
        , (removeMapId, builtinEvaluation removeMapEvaluator)
        ]
  where
    Just addIntEvaluator = Map.lookup "INT.add" Int.builtinFunctions
    addIntId =
        AxiomIdentifier.Application
        $ symbolConstructor Builtin.addIntSymbol
    Just removeMapEvaluator = Map.lookup "MAP.remove" Map.builtinFunctions
    removeMapId =
        AxiomIdentifier.Application
        $ symbolConstructor Builtin.removeMapSymbol


test_updateList :: [TestTree]
test_updateList =
    [ notApplies "different concrete indices"
        [updateListSimplifier]
        (updateList
            (updateList singletonList (mkInt 0) (mkInt 1))
            (mkInt 1)
            (mkInt 2)
        )
    , applies "same concrete indices"
        [updateListSimplifier]
        (updateList
            (updateList singletonList (mkInt 0) (mkInt 1))
            (mkInt 0)
            (mkInt 2)
        )
    , notApplies "different abstract keys; evaluates requires with SMT"
        [updateListSimplifier]
        (updateList
            (updateList varL (mkElemVar xInt) (mkInt 1))
            (addInt (mkElemVar xInt) (mkInt 1))
            (mkInt 2)
        )
    , notApplies "different keys; evaluates requires with function rule"
        [updateListSimplifier]
        (updateList
            (updateList Builtin.unitList (mkInt 0) (mkInt 1))
            (addInt (mkInt 0) (Builtin.dummyInt (mkInt 1)))
            (mkInt 2)
        )
    , equals "different keys; evaluates updateList"
        (updateList
            (updateList twoElementList (mkInt 0) (mkInt 1))
            (addInt (mkInt 0) (Builtin.dummyInt (mkInt 1)))
            (mkInt 2)
        )
        [mkList [mkInt 1, mkInt 2]]
    , equals "different negative keys; evaluates updateList"
        (updateList
            (updateList twoElementList (mkInt (-2)) (mkInt 1))
            (addInt (mkInt 0) (Builtin.dummyInt (mkInt (-1))))
            (mkInt 2)
        )
        [mkList [mkInt 1, mkInt 2]]
    , equals "negative index outside rage"
        (updateList singletonList (mkInt (-2)) (mkInt 1))
        [mkBottom_]
    , equals "positive index outside rage"
        (updateList singletonList (mkInt 1) (mkInt 1))
        [mkBottom_]
    , applies "same abstract key"
        [updateListSimplifier]
        (updateList
            (updateList singletonList (mkElemVar xInt) (mkInt 1))
            (mkElemVar xInt)
            (mkInt 2)
        )
    ]

singletonList :: TermLike Variable
singletonList = Builtin.elementList (mkInt 0)

twoElementList :: TermLike Variable
twoElementList = Builtin.concatList singletonList singletonList

updateList
    :: TermLike Variable -- ^ List
    -> TermLike Variable -- ^ Index
    -> TermLike Variable -- ^ Value
    -> TermLike Variable
updateList = Builtin.updateList

updateListSimplifier :: Equation Variable
updateListSimplifier =
    axiom
        (updateList (updateList varL u v) x y)
        (updateList varL u y)
        (makeEqualsPredicate_ (Builtin.keqBool (injK u) (injK x)) (mkBool True))
  where
    [u, v, x, y] = mkElemVar <$> [uInt, vInt, xInt, yInt]
    injK = Builtin.inj Builtin.kSort

test_lookupMap :: [TestTree]
test_lookupMap =
    [ equals "lookupMap(.Map, 1) = \\bottom"
        (lookupMap (mkMap [] []) (mkInt 1))
        []
    , equals "lookupMap(1 |-> 2, 1) = 2"
        (lookupMap (mkMap [(mkInt 1, mkInt 2)] []) (mkInt 1))
        [mkInt 2]
    , equals "lookupMap(0 |-> 1  1 |-> 2, 1) = 2"
        (lookupMap (mkMap [(mkInt 0, mkInt 1), (mkInt 1, mkInt 2)] []) (mkInt 1))
        [mkInt 2]
    ]

lookupMapSymbol :: Symbol
lookupMapSymbol = Builtin.lookupMapSymbol

lookupMap :: TermLike Variable -> TermLike Variable -> TermLike Variable
lookupMap = Builtin.lookupMap

lookupMapRule :: Equation Variable
lookupMapRule = axiom_ (lookupMap (mkMap [(varX, varY)] [mMapTerm]) varX) varY

lookupMapRules :: [Equation Variable]
lookupMapRules = [lookupMapRule]

lookupMapEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
lookupMapEvaluator = functionEvaluator lookupMapSymbol lookupMapRules

test_updateMap :: [TestTree]
test_updateMap =
    [ notApplies "different concrete keys"
        [updateMapSimplifier]
        (updateMap
            (updateMap mMapTerm (mkInt 0) (mkInt 1))
            (mkInt 1)
            (mkInt 2)
        )
    , applies "same concrete key"
        [updateMapSimplifier]
        (updateMap
            (updateMap mMapTerm (mkInt 0) (mkInt 1))
            (mkInt 0)
            (mkInt 2)
        )
    , notApplies "different abstract keys; evaluates requires with SMT"
        [updateMapSimplifier]
        (updateMap
            (updateMap mMapTerm (mkElemVar xInt) (mkInt 1))
            (addInt (mkElemVar xInt) (mkInt 1))
            (mkInt 2)
        )
    , notApplies "different keys; evaluates requires with function rule"
        [updateMapSimplifier]
        (updateMap
            (updateMap Builtin.unitMap (mkInt 0) (mkInt 1))
            (addInt (mkInt 0) (Builtin.dummyInt (mkInt 1)))
            (mkInt 2)
        )
    , equals "different keys; evaluates updateMap"
        (updateMap
            (updateMap Builtin.unitMap (mkInt 0) (mkInt 1))
            (addInt (mkInt 0) (Builtin.dummyInt (mkInt 1)))
            (mkInt 2)
        )
        [mkMap [(mkInt 0, mkInt 1), (mkInt 1, mkInt 2)] []]
    , applies "same abstract key"
        [updateMapSimplifier]
        (updateMap
            (updateMap mMapTerm (mkElemVar xInt) (mkInt 1))
            (mkElemVar xInt)
            (mkInt 2)
        )
    ]

updateMap
    :: TermLike Variable  -- ^ Map
    -> TermLike Variable  -- ^ Key
    -> TermLike Variable  -- ^ Value
    -> TermLike Variable
updateMap = Builtin.updateMap

updateMapSimplifier :: Equation Variable
updateMapSimplifier =
    axiom
        (updateMap (updateMap mMapTerm u v) x y)
        (updateMap mMapTerm u y)
        (makeEqualsPredicate_ (Builtin.keqBool (injK u) (injK x)) (mkBool True))
  where
    [u, v, x, y] = mkElemVar <$> [uInt, vInt, xInt, yInt]
    injK = Builtin.inj Builtin.kSort

dummyIntSimplifier :: Equation Variable
dummyIntSimplifier =
    axiom_ (Builtin.dummyInt (mkElemVar xInt)) (mkElemVar xInt)

mkBool :: Bool -> TermLike Variable
mkBool = Bool.asInternal

mapSimplifiers :: BuiltinAndAxiomSimplifierMap
mapSimplifiers =
    Map.fromList
        [ lookupMapEvaluator
        , functionSimplifier Builtin.updateMapSymbol [updateMapSimplifier]
        , functionEvaluator Builtin.dummyIntSymbol [dummyIntSimplifier]
        ]

uInt, vInt, xInt, yInt :: ElementVariable Variable
uInt = elemVarS (testId "uInt") intSort
vInt = elemVarS (testId "vInt") intSort
xInt = elemVarS (testId "xInt") intSort
yInt = elemVarS (testId "yInt") intSort

xsInt :: SetVariable Variable
xsInt = setVarS (testId "xsInt") intSort

test_Ceil :: [TestTree]
test_Ceil =
    [ simplifies "\\ceil(dummy(X)) => ... ~ \\ceil(dummy(Y))"
        ceilDummyRule
        (mkCeil_ $ Builtin.dummyInt $ mkElemVar yInt)
    , notSimplifies "\\ceil(dummy(X)) => \\not(\\equals(X, 0)) !~ dummy(Y)"
        ceilDummyRule
        (Builtin.dummyInt $ mkElemVar yInt)
    , simplifies "\\ceil(dummy(@X)) => ... ~ \\ceil(dummy(Y))"
        ceilDummySetRule
        (mkCeil_ $ Builtin.dummyInt $ mkElemVar yInt)
    ]

ceilDummyRule :: Equation Variable
ceilDummyRule =
    axiom_
        (mkCeil_ $ Builtin.dummyInt $ mkElemVar xInt)
        (mkEquals_ (Builtin.eqInt (mkElemVar xInt) (mkInt 0)) (mkBool False))

ceilDummySetRule :: Equation Variable
ceilDummySetRule =
    axiom_
        (mkCeil_ $ Builtin.dummyInt $ mkSetVar xsInt)
        (mkEquals_ (Builtin.eqInt (mkSetVar xsInt) (mkInt 0)) (mkBool False))

-- Simplification tests: check that one or more rules applies or not
withSimplified
    :: (CommonAttemptedAxiom -> Assertion)
    -> TestName
    -> Equation Variable
    -> TermLike Variable
    -> TestTree
withSimplified check comment rule term =
    testCase comment $ do
        actual <- evaluateWith (simplificationEvaluation rule) term
        check actual

simplifies, notSimplifies
    :: TestName
    -> Equation Variable
    -> TermLike Variable
    -> TestTree
simplifies =
    withSimplified $ \attempted -> do
        results <- expectApplied attempted
        expectNoRemainders results
  where
    expectApplied NotApplicable = assertFailure "Expected Applied"
    expectApplied (NotApplicableUntilConditionChanges _) =
        assertFailure "Expected Applied"
    expectApplied (Applied results) = return results
    expectNoRemainders =
        assertBool "Expected no remainders"
        . isBottom
        . Lens.view (field @"remainders")
notSimplifies =
    withSimplified (assertBool "Expected NotApplicable" . isNotApplicable)

axiomEvaluator
    :: TermLike Variable
    -> TermLike Variable
    -> BuiltinAndAxiomSimplifier
axiomEvaluator left right =
    simplificationEvaluation (axiom left right makeTruePredicate_)

appliedMockEvaluator
    :: Pattern Variable -> BuiltinAndAxiomSimplifier
appliedMockEvaluator result =
    BuiltinAndAxiomSimplifier
    $ mockEvaluator
    $ AttemptedAxiom.Applied AttemptedAxiomResults
        { results = OrPattern.fromPatterns
            [Test.Kore.Step.Function.Integration.mapVariables result]
        , remainders = OrPattern.fromPatterns []
        }

mapVariables
    :: forall variable
    .  InternalVariable variable
    => Pattern Variable
    -> Pattern variable
mapVariables =
    Pattern.mapVariables worker worker
  where
    worker :: Functor f => f Variable -> f variable
    worker = fmap $ \v ->
        fromVariable v { variableCounter = Just (Element 1) }

mockEvaluator
    :: Monad simplifier
    => AttemptedAxiom variable
    -> TermLike variable
    -> SideCondition variable
    -> simplifier (AttemptedAxiom variable)
mockEvaluator evaluation _ _ = return evaluation

-- ---------------------------------------------------------------------
-- * Definition

natModuleName :: ModuleName
natModuleName = ModuleName "NAT"

natSortDecl :: Sentence pattern'
natSortDecl =
    asSentence SentenceSort
        { sentenceSortName =
            let SortActualSort SortActual { sortActualName } = natSort
            in sortActualName
        , sentenceSortParameters = []
        , sentenceSortAttributes = Attributes []
        }

-- | Declare the @BOOL@ builtins.
natModule :: ParsedModule
natModule =
    Module
        { moduleName = natModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ natSortDecl
            , Builtin.symbolDecl zeroSymbol
            , Builtin.symbolDecl succSymbol
            , Builtin.symbolDecl plusSymbol
            , Builtin.symbolDecl timesSymbol
            , Builtin.symbolDecl fibonacciSymbol
            , Builtin.symbolDecl factorialSymbol
            ]
        }

testModuleName :: ModuleName
testModuleName = ModuleName "INTEGRATION-TEST"

testModule :: ParsedModule
testModule =
    Module
        { moduleName = testModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ Builtin.importParsedModule Builtin.testModuleName
            , Builtin.importParsedModule natModuleName
            , Builtin.subsortDecl Builtin.intSort Builtin.kSort
            ]
        }

testDefinition :: ParsedDefinition
testDefinition =
    Builtin.testDefinition
    & field @"definitionModules" Lens.<>~ [natModule, testModule]

verify
    :: ParsedDefinition
    -> Either
        (Kore.Error.Error VerifyError)
        (Map ModuleName (VerifiedModule Attribute.Symbol)
        )
verify = verifyAndIndexDefinition Builtin.koreVerifiers

verifiedModules
    :: Map ModuleName (VerifiedModule Attribute.Symbol)
verifiedModules = Kore.Error.assertRight (verify testDefinition)

verifiedModule :: VerifiedModule Attribute.Symbol
Just verifiedModule = Map.lookup testModuleName verifiedModules

testMetadataTools :: SmtMetadataTools Attribute.Symbol
testMetadataTools = MetadataTools.build verifiedModule

testConditionSimplifier
    :: MonadSimplify simplifier => ConditionSimplifier simplifier
testConditionSimplifier =
    Simplifier.Condition.create SubstitutionSimplifier.substitutionSimplifier

testEvaluators :: BuiltinAndAxiomSimplifierMap
testEvaluators = Builtin.koreEvaluators verifiedModule

testTermLikeSimplifier :: TermLikeSimplifier
testTermLikeSimplifier = Simplifier.create

testInjSimplifier :: InjSimplifier
testInjSimplifier =
    mkInjSimplifier $ SortGraph.fromIndexedModule verifiedModule

testEnv :: Env Simplifier
testEnv =
    Env
        { metadataTools = testMetadataTools
        , simplifierTermLike = testTermLikeSimplifier
        , simplifierCondition = testConditionSimplifier
        , simplifierAxioms =
            mconcat
                [ testEvaluators
                , natSimplifiers
                , listSimplifiers
                , mapSimplifiers
                , fatalSimplifiers
                ]
        , memo = Memo.forgetful
        , injSimplifier = testInjSimplifier
        , overloadSimplifier = Mock.overloadSimplifier
        }
