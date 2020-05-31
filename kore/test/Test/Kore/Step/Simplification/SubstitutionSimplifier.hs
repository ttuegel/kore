{-# LANGUAGE OverloadedLists #-}

module Test.Kore.Step.Simplification.SubstitutionSimplifier
    ( test_SubstitutionSimplifier
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.OrCondition as OrCondition
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.Substitution
    ( mkNormalization
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
import qualified Kore.Step.Simplification.Not as Not
import Kore.Step.Simplification.SubstitutionSimplifier
    ( SubstitutionSimplifier (..)
    )
import qualified Kore.Step.Simplification.SubstitutionSimplifier as Simplification
import Kore.Unification.SubstitutionNormalization
import qualified Kore.Unification.SubstitutionSimplifier as Unification
    ( substitutionSimplifier
    )
import Kore.Unification.UnifierT
    ( runUnifierT
    )
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable
    )

import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
    ( runSimplifier
    )
import Test.Tasty.HUnit.Ext

test_SubstitutionSimplifier :: [TestTree]
test_SubstitutionSimplifier =
    [ test "empty substitution"
        []
        [ mempty ]
    , test "normalized substitution"
        [(y, a)]
        [ mkNormalization [(y, a)] [] ]
    , test "unnormalized substitution, variable-only"
        [(y, mkVar x), (x, a)]
        [ mkNormalization [(x, a), (y, a)] [] ]
    , test "unnormalized substitution, variable under symbol"
        [(y, sigma (mkVar x) b), (x, a)]
        [ mkNormalization [(x, a), (y, sigma a b)] [] ]
    , test "simplification generates substitution"
        [(x, sigma (mkAnd a (mkVar y)) b)]
        [ mkNormalization [(x, sigma a b), (y, a)] [] ]
    , testGroup "element-variable-only cycle"
        [ test "length 1, alone"
            [(x, mkVar x)]
            [ mempty ]
        , test "length 1, beside related substitution"
            [(x, mkVar x), (z, f (mkVar x))]
            [ mkNormalization [(z, f (mkVar x))] [] ]
        , test "length 1, beside unrelated substitution"
            [(x, mkVar x), (z, a)]
            [ mkNormalization [(z, a)] [] ]
        ]
    , testGroup "set-variable-only cycle"
        [ test "length 1, alone"
            [(xs, mkVar xs)]
            [ mempty ]
        -- UnificationError: UnsupportedPatterns
        -- , test "length 1, beside related substitution"
        --     [(xs, mkVar xs), (ys, mkVar xs)]
        --     [([(ys, mkVar xs)], [])]
        , test "length 1, beside unrelated substitution"
            [(xs, mkVar xs), (z, a)]
            [ mkNormalization [(z, a)] [] ]
        ]
    , testGroup "element variable simplifiable cycle"
        [ test "length 1, alone"
            [(x, f (mkVar x))]
            [ mkNormalization [] [(x, f (mkVar x))] ]
        , test "length 1, beside related substitution"
            [(x, f (mkVar x)), (y, g (mkVar x))]
            [ mkNormalization [(y, g (mkVar x))] [(x, f (mkVar x))] ]
        , test "length 1, beside unrelated substitution"
            [(x, f (mkVar x)), (y, a)]
            [ mkNormalization [(y, a)] [(x, f (mkVar x))] ]
        , test "length 1, beside unrelated substitutions"
            [(x, f (mkVar x)), (y, g (mkVar z)), (z, b)]
            [ mkNormalization [(z, b), (y, g b)] [(x, f (mkVar x))] ]
        , test "length 1, with constructor"
            [(x, (constr1 . f) (mkVar x))]
            [ mkNormalization [] [(x, (constr1 . f) (mkVar x))] ]
        , test "length 2, alone"
            [(x, f (mkVar y)), (y, g (mkVar x))]
            [ mkNormalization [] [(x, f (mkVar y)), (y, g (mkVar x))] ]
        , test "length 2, beside related substitution"
            [(x, f (mkVar y)), (y, g (mkVar x)), (z, h (mkVar y))]
            [ mkNormalization
                [(z, h (mkVar y))]
                [(x, f (mkVar y)), (y, g (mkVar x))]
            ]
        , test "length 2, beside unrelated substitution"
            [(x, f (mkVar y)), (y, g (mkVar x)), (z, a)]
            [ mkNormalization [(z, a)] [(x, f (mkVar y)), (y, g (mkVar x))] ]
        , test "two cycles"
            [(x, f (mkVar x)), (y, g (mkVar y)), (z, c)]
            [ mkNormalization [(z, c)] [(x, f (mkVar x)), (y, g (mkVar y))] ]
        ]
    , testGroup "set variable simplifiable cycle"
        [ test "length 1, alone"
            [(xs, f (mkVar xs))]
            [ mkNormalization [] [(xs, f (mkVar xs))] ]
        -- UnificationError: UnsupportedPatterns
        -- , test "length 1, beside related substitution"
        --     [(xs, f (mkVar xs)), (ys, mkVar xs)]
        --     [([(ys, mkVar xs)], [(xs, f (mkVar xs))])]
        , test "length 1, beside unrelated substitution"
            [(xs, f (mkVar xs)), (ys, a)]
            [ mkNormalization [(ys, a)] [(xs, f (mkVar xs))] ]
        , test "length 1, beside unrelated substitutions"
            [(xs, f (mkVar xs)), (y, g (mkVar z)), (z, b)]
            [ mkNormalization [(z, b), (y, g b)] [(xs, f (mkVar xs))] ]
        , test "length 2, alone"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs))]
            [ mkNormalization [] [(xs, f (mkVar ys)), (ys, g (mkVar xs))] ]
        , test "length 2, beside related substitution"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs)), (z, h (mkVar ys))]
            [ mkNormalization
                [(z, h (mkVar ys))]
                [(xs, f (mkVar ys)), (ys, g (mkVar xs))]
            ]
        , test "length 2, beside unrelated substitution"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs)), (z, a)]
            [ mkNormalization [(z, a)] [(xs, f (mkVar ys)), (ys, g (mkVar xs))] ]
        , test "length 2, with And"
            [(xs, mkAnd (mkVar ys) a), (ys, mkAnd (mkVar xs) b)]
            [ mkNormalization
                []
                [(xs, mkAnd (mkVar ys) a), (ys, mkAnd (mkVar xs) b)]
            ]
        , test "two cycles"
            [(xs, f (mkVar xs)), (ys, g (mkVar ys)), (z, c)]
            [ mkNormalization [(z, c)] [(xs, f (mkVar xs)), (ys, g (mkVar ys))] ]
        ]
    , test "two simplifiable cycles, set and element variables"
        [(xs, f (mkVar xs)), (y, g (mkVar y)), (z, c)]
        [ mkNormalization [(z, c)] [(y, g (mkVar y)), (xs, f (mkVar xs))] ]
    , testGroup "element variable non-simplifiable cycle"
        [ test "alone"
            [(x, constr1 (mkVar x))]
            []
        , test "beside simplifiable cycle"
            [(x, sigma (f (mkVar x)) (mkVar x))]
            []
        , test "length 2, with And"
            [(x, mkAnd (mkVar y) a), (y, mkAnd (mkVar x) b)]
            []
        ]
    , testGroup "set variable non-simplifiable cycle"
        [ test "alone"
            [(xs, constr1 (mkVar xs))]
            [ mkNormalization [(xs, mkBottom testSort)] [] ]
        , test "beside unrelated substitution"
            [(xs, constr1 (mkVar xs)), (z, a)]
            [ mkNormalization [(xs, mkBottom testSort), (z, a)] [] ]
        , test "beside related substitution"
            [(xs, constr1 (mkVar xs)), (ys, f (mkVar xs))]
            [ mkNormalization
                [(xs, mkBottom testSort), (ys, f (mkBottom testSort))]
                []
            ]
        ]
    ]
  where
    test
        :: HasCallStack
        => TestName
        -> [(UnifiedVariable VariableName, TermLike VariableName)]
        -- ^ Test input
        -> [Normalization VariableName]
        -- ^ Expected normalized, denormalized outputs
        -> TestTree
    test
        testName
        (Substitution.wrap . Substitution.mkUnwrappedSubstitution -> input)
        results
      =
        testGroup testName
            [ testCase "simplification" $ do
                let SubstitutionSimplifier { simplifySubstitution } =
                        Simplification.substitutionSimplifier
                actual <- runSimplifier Mock.env
                    $ simplifySubstitution SideCondition.top input
                let expect = Condition.fromNormalizationSimplified <$> results
                    actualConditions = OrCondition.toConditions actual
                    actualSubstitutions =
                        Condition.substitution <$> actualConditions
                assertEqual ""
                    (fixExpectedSorts expect actualConditions)
                    actualConditions
                assertBool "Expected normalized substitutions"
                    (all Substitution.isNormalized actualSubstitutions)
            , testCase "unification" $ do
                let SubstitutionSimplifier { simplifySubstitution } =
                        Unification.substitutionSimplifier Not.notSimplifier
                actual <-
                    runSimplifier Mock.env . runUnifierT Not.notSimplifier
                    $ simplifySubstitution SideCondition.top input
                let expect1 = Right . Condition.fromNormalizationSimplified
                    expect
                      | null results = Right []
                      | otherwise    = (: []) <$> traverse expect1 results
                    actualConditions = map OrCondition.toConditions <$> actual
                    actualSubstitutions =
                        (map . map) Condition.substitution <$> actualConditions
                    allNormalized = (all . all . all) Substitution.isNormalized
                    expectSorted = case actualConditions of
                        Left _ -> expect
                        Right r ->
                            map (\expct' -> fixExpectedSorts expct' (concat r))
                            <$> expect
                assertEqual ""
                    expectSorted
                    actualConditions
                assertBool "Expected normalized substitutions"
                    (allNormalized actualSubstitutions)
            ]

    fixExpectedSorts expect actual =
        case actual of
            [] -> expect
            first:_ ->
                let sort = Condition.conditionSort first
                in map (Condition.coerceSort sort) expect

x, y, z, xs, ys :: UnifiedVariable VariableName
x = inject Mock.x
y = inject Mock.y
z = inject Mock.z
xs = inject Mock.setX
ys = inject Mock.setY

a, b, c :: TermLike VariableName
a = Mock.a
b = Mock.b
c = Mock.c

f, g, h, constr1 :: TermLike VariableName -> TermLike VariableName
f = Mock.f
g = Mock.g
h = Mock.h
constr1 = Mock.constr10

sigma :: TermLike VariableName -> TermLike VariableName -> TermLike VariableName
sigma = Mock.sigma

testSort :: Sort
testSort = Mock.testSort
