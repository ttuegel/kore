module Test.Kore.Unification.UnifierT
    ( test_mergeAndNormalizeSubstitutions
    , test_simplifyCondition
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map

import qualified Branch
import qualified Kore.Equation as Equation
import Kore.Internal.Condition
    ( Condition
    )
import qualified Kore.Internal.Condition as Condition
import Kore.Internal.Conditional
    ( Conditional (..)
    )
import Kore.Internal.MultiOr
    ( MultiOr
    )
import qualified Kore.Internal.MultiOr as MultiOr
import qualified Kore.Internal.OrCondition as OrCondition
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.Substitution
    ( Assignment
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
import qualified Kore.Step.Axiom.EvaluationStrategy as EvaluationStrategy
import qualified Kore.Step.Axiom.Identifier as Axiom.Identifier
import qualified Kore.Step.Simplification.Condition as Condition
import Kore.Step.Simplification.Data
    ( Env (..)
    )
import qualified Kore.Step.Simplification.Simplify as Simplifier
import Kore.Unification.Error
import qualified Kore.Unification.UnifierT as Monad.Unify
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
    )

import qualified Test.Kore.Step.MockSymbols as Mock
import qualified Test.Kore.Step.Simplification as Test
import Test.Tasty.HUnit.Ext

assertNormalized :: Condition Variable -> IO ()
assertNormalized expect = do
    actual <- normalizeExcept expect
    assertEqual
        "Expected original result"
        (Right $ MultiOr.make [expect])
        actual
    Foldable.traverse_ assertNormalizedPredicatesMulti actual

test_simplifyCondition :: [TestTree]
test_simplifyCondition =
    [ testCase "predicate = \\bottom" $ do
        let expect = mempty
        actual <- normalize Condition.bottomCondition
        assertEqual "Expected empty result" expect actual
        assertNormalizedPredicatesMulti actual
    , testCase "∃ y z. x = σ(y, z)" $ do
        let expect = Condition.fromPredicate existsPredicate
        assertNormalized expect
    , testCase "¬∃ y z. x = σ(y, z)" $ do
        let expect =
                Condition.fromPredicate
                $ Predicate.makeNotPredicate existsPredicate
        assertNormalized expect
    , testCase "x = f(x)" $ do
        let x = ElemVar Mock.x
            expect =
                Predicate.makeEqualsPredicate_ (mkVar x) (Mock.f (mkVar x))
                & Right . OrCondition.fromPredicate
            denormalized =
                Substitution.mkUnwrappedSubstitution
                [(x, Mock.f (mkVar x))]
            input =
                (Condition.fromSubstitution . Substitution.wrap) denormalized
        actual <- normalizeExcept input
        assertEqual "Expected SubstitutionError" expect actual
    , testCase "x = id(x)" $ do
        let x = ElemVar Mock.x
            expect = Right (OrCondition.fromCondition Condition.top)
            input =
                ( Condition.fromSubstitution
                . Substitution.wrap
                . Substitution.mkUnwrappedSubstitution
                )
                    [(x, Mock.functional10 (mkVar x))]
        actual <- normalizeExcept input
        assertEqual "Expected \\top" expect actual
    ]
  where
    existsPredicate =
        Predicate.makeMultipleExists [Mock.y, Mock.z]
        $ Predicate.makeEqualsPredicate_
            (mkElemVar Mock.x)
            (Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.z))

test_mergeAndNormalizeSubstitutions :: [TestTree]
test_mergeAndNormalizeSubstitutions =
    [ testCase "Constructor normalization"
        -- [x=constructor(a)] + [x=constructor(a)]  === [x=constructor(a)]
        $ do
            let expect = Right
                    [ Condition.fromSubstitution $ Substitution.unsafeWrap
                        [ ( ElemVar Mock.x , Mock.constr10 Mock.a ) ]
                    ]
            actual <-
                merge
                    [( ElemVar Mock.x
                     , Mock.constr10 Mock.a
                     )
                    ]
                    [( ElemVar Mock.x
                     , Mock.constr10 Mock.a
                     )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Constructor normalization with variables"
        -- [x=constructor(y)] + [x=constructor(y)]  === [x=constructor(y)]
        $ do
            let expect = Right
                    [ Condition.fromSubstitution $ Substitution.unsafeWrap
                        [(ElemVar Mock.x, Mock.constr10 (mkElemVar Mock.y))]
                    ]
            actual <-
                merge
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (mkElemVar Mock.y)
                        )
                    ]
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (mkElemVar Mock.y)
                        )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Double constructor is bottom"
        -- [x=constructor(a)] + [x=constructor(constructor(a))]  === bottom?
        $ do
            let expect = Right []
            actual <-
                merge
                    [   ( ElemVar Mock.x
                        , Mock.constr10 Mock.a
                        )
                    ]
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (Mock.constr10 Mock.a)
                        )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Double constructor is bottom with variables"
        -- [x=constructor(y)] + [x=constructor(constructor(y))]  === bottom?
        $ do
            let expect = Left $ unsupportedPatterns
                    "Unknown unification case."
                    (Mock.constr10 (mkElemVar Mock.y))
                    (mkElemVar Mock.y)
            actual <-
                merge
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (mkElemVar Mock.y)
                        )
                    ]
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (Mock.constr10 (mkElemVar Mock.y))
                        )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Constructor and constructor of function"
        -- [x=constructor(a)] + [x=constructor(f(a))]
        $ do
            let expect =
                    Right
                        [ Conditional
                            { term = ()
                            , predicate =
                                Predicate.makeEqualsPredicate_
                                    Mock.a
                                    (Mock.f Mock.a)
                            , substitution = Substitution.unsafeWrap
                                [   ( ElemVar Mock.x
                                    , Mock.constr10 Mock.a
                                    )
                                ]
                            }
                        ]
            actual <-
                merge
                    [   ( ElemVar Mock.x
                        , Mock.constr10 Mock.a
                        )
                    ]
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (Mock.f Mock.a)
                        )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Constructor and constructor of function with variables" $ do
        let ctor = Mock.constr10
            f = Mock.f
            y = mkElemVar Mock.y
        let denormCondition =
                Predicate.makeEqualsPredicate_ y (f y)
                & Condition.fromPredicate
            substCondition =
                Substitution.assign (ElemVar Mock.x) (ctor (f y))
                & Condition.fromSingleSubstitution
        let
            expect =
                denormCondition <> substCondition
                & Right . pure
        actual <-
            merge
                [(ElemVar Mock.x, ctor    y )]
                [(ElemVar Mock.x, ctor (f y))]
        assertEqual "" expect actual
        assertNormalizedPredicates actual

    , testCase "Constructor circular dependency?"
        -- [x=y] + [y=constructor(x)]  === error
        $ do
            let expect = Left $ unsupportedPatterns
                    "Unknown unification case."
                    (Mock.constr10 (mkElemVar Mock.x))
                    (mkElemVar Mock.y)
            actual <-
                merge
                    [   ( ElemVar Mock.x
                        , mkElemVar Mock.y
                        )
                    ]
                    [   ( ElemVar Mock.x
                        , Mock.constr10 (mkElemVar Mock.x)
                        )
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicates actual

    , testCase "Non-ctor circular dependency" $ do
        let denormCondition =
                Predicate.makeEqualsPredicate_
                    (mkElemVar Mock.y)
                    (Mock.f (mkElemVar Mock.y))
                & Condition.fromPredicate
            substCondition =
                Substitution.assign (ElemVar Mock.x) (mkElemVar Mock.y)
                & Condition.fromSingleSubstitution
        let expect =
                denormCondition <> substCondition
                & Right . pure
        actual <-
            merge
                [   ( ElemVar Mock.x
                    , mkElemVar Mock.y
                    )
                ]
                [   ( ElemVar Mock.y
                    , Mock.f (mkElemVar Mock.x)
                    )
                ]
        assertEqual "" expect actual
        assertNormalizedPredicates actual

    , testCase "Normalizes substitution"
        $ do
            let expect =
                    [ Condition.fromSubstitution $ Substitution.unsafeWrap
                        [ (ElemVar Mock.x, Mock.constr10 Mock.a)
                        , (ElemVar Mock.y, Mock.a)
                        ]
                    ]
            actual <-
                normalize
                $ Condition.fromSubstitution
                $ Substitution.wrap
                $ Substitution.mkUnwrappedSubstitution
                    [ (ElemVar Mock.x, Mock.constr10 Mock.a)
                    , (ElemVar Mock.x, Mock.constr10 (mkElemVar Mock.y))
                    ]
            assertEqual "" expect actual
            assertNormalizedPredicatesMulti actual

    , testCase "Predicate from normalizing substitution"
        $ do
            let expect =
                    [ Conditional
                        { term = ()
                        , predicate =
                            Predicate.makeEqualsPredicate_ Mock.cf Mock.cg
                        , substitution = Substitution.unsafeWrap
                            [ (ElemVar Mock.x, Mock.constr10 Mock.cf) ]
                        }
                    ]
            actual <-
                normalize
                    Conditional
                        { term = ()
                        , predicate = Predicate.makeTruePredicate_
                        , substitution = Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [ (ElemVar Mock.x, Mock.constr10 Mock.cf)
                            , (ElemVar Mock.x, Mock.constr10 Mock.cg)
                            ]
                        }
            assertEqual "" expect actual
            assertNormalizedPredicatesMulti actual

    , testCase "Normalizes substitution and substitutes in predicate"
        $ do
            let expect =
                    [ Conditional
                        { term = ()
                        , predicate =
                            Predicate.makeCeilPredicate_
                            $ Mock.f Mock.a
                        , substitution = Substitution.unsafeWrap
                            [ (ElemVar Mock.x, Mock.constr10 Mock.a)
                            , (ElemVar Mock.y, Mock.a)
                            ]
                        }
                    ]
            actual <-
                normalize
                    Conditional
                        { term = ()
                        , predicate =
                            Predicate.makeCeilPredicate_
                            $ Mock.f (mkElemVar Mock.y)
                        , substitution = Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [ (ElemVar Mock.x, Mock.constr10 Mock.a)
                            , (ElemVar Mock.x, Mock.constr10 (mkElemVar Mock.y))
                            ]
                        }
            assertEqual "" expect actual
            assertNormalizedPredicatesMulti actual
    ]

merge
    :: [(UnifiedVariable Variable, TermLike Variable)]
    -> [(UnifiedVariable Variable, TermLike Variable)]
    -> IO (Either UnificationError [Condition Variable])
merge
    (Substitution.mkUnwrappedSubstitution -> s1)
    (Substitution.mkUnwrappedSubstitution -> s2)
  =
    Test.runSimplifier mockEnv
    $ Monad.Unify.runUnifierT
    $ mergeSubstitutionsExcept
    $ Substitution.wrap
    . fmap simplifiedAssignment
    <$> [s1, s2]
  where
    simplifiedAssignment
        :: Assignment Variable
        -> Assignment Variable
    simplifiedAssignment =
        Substitution.mapAssignedTerm Test.simplifiedTerm

    mergeSubstitutionsExcept =
        Branch.alternate
        . Simplifier.simplifyCondition SideCondition.top
        . Condition.fromSubstitution
        . mconcat
    mockEnv = Mock.env

normalize :: Conditional Variable term -> IO [Conditional Variable term]
normalize =
    Test.runSimplifierBranch mockEnv
    . Condition.simplifyCondition SideCondition.top
  where
    mockEnv = Mock.env

normalizeExcept
    :: Conditional Variable ()
    -> IO
        (Either
            UnificationError
            (MultiOr (Conditional Variable ()))
        )
normalizeExcept predicated =
    (fmap . fmap) MultiOr.make
    $ Test.runSimplifier mockEnv
    $ Monad.Unify.runUnifierT
    $ Branch.alternate
    $ Simplifier.simplifyCondition SideCondition.top predicated
  where
    mockEnv = Mock.env { simplifierAxioms }
    simplifierAxioms =
        -- Use Mock.functional10 as the identity function.
        Map.fromList
            [   ( Axiom.Identifier.Application Mock.functional10Id
                , EvaluationStrategy.definitionEvaluation
                    [ Equation.mkEquation
                        (mkSortVariable "R")
                        (Mock.functional10 (mkElemVar Mock.x))
                        (mkElemVar Mock.x)
                    ]
                )
            ]


-- | Check that 'Condition.substitution' is normalized for all arguments.
assertNormalizedPredicates :: Foldable f => f [Condition Variable] -> Assertion
assertNormalizedPredicates =
    Foldable.traverse_ assertNormalizedPredicatesMulti

-- | Check that 'Condition.substitution' is normalized for all arguments.
assertNormalizedPredicatesMulti
    :: Foldable f => f (Condition Variable) -> Assertion
assertNormalizedPredicatesMulti =
    Foldable.traverse_ assertNormalizedPredicatesSingle

-- | Check that 'Condition.substitution' is normalized for all arguments.
assertNormalizedPredicatesSingle :: Condition Variable -> Assertion
assertNormalizedPredicatesSingle =
    assertBool "Substitution is normalized"
    . Substitution.isNormalized
    . Condition.substitution
