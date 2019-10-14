{-# LANGUAGE OverloadedLists #-}

module Test.Kore.Step.Simplification.Substitution
    ( test_SubstitutionSimplifier
    ) where

import Test.Tasty

import qualified GHC.Stack as GHC

import qualified Kore.Internal.OrPredicate as OrPredicate
import Kore.Internal.TermLike
import Kore.Step.Simplification.Substitution
    ( SubstitutionSimplifier (..)
    )
import qualified Kore.Step.Simplification.Substitution as Substitution
import Kore.Unification.Error
    ( SubstitutionError (..)
    , UnificationOrSubstitutionError (..)
    )
import Kore.Unification.Substitution
    ( UnwrappedSubstitution
    )
import qualified Kore.Unification.Substitution as Substitution
import Kore.Unification.SubstitutionNormalization
import Kore.Unification.Unify
    ( runUnifierT
    )
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
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
        [([], [])]
    , test "normalized substitution"
        [(y, a)]
        [([(y, a)], [])]
    , test "unnormalized substitution, variable-only"
        [(y, mkVar x), (x, a)]
        [([(x, a), (y, a)], [])]
    , test "unnormalized substitution, variable under symbol"
        [(y, sigma (mkVar x) b), (x, a)]
        [([(x, a), (y, sigma a b)], [])]
    , testGroup "element-variable-only cycle"
        [ test "length 1, alone"
            [(x, mkVar x)]
            [([], [])]
        , test "length 1, beside related substitution"
            [(x, mkVar x), (z, f (mkVar x))]
            [([(z, f (mkVar x))], [])]
        , test "length 1, beside unrelated substitution"
            [(x, mkVar x), (z, a)]
            [([(z, a)], [])]
        ]
    , testGroup "set-variable-only cycle"
        [ test "length 1, alone"
            [(xs, mkVar xs)]
            [([], [])]
        -- UnificationError: UnsupportedPatterns
        -- , test "length 1, beside related substitution"
        --     [(xs, mkVar xs), (ys, mkVar xs)]
        --     [([(ys, mkVar xs)], [])]
        , test "length 1, beside unrelated substitution"
            [(xs, mkVar xs), (z, a)]
            [([(z, a)], [])]
        ]
    , testGroup "element variable simplifiable cycle"
        [ test "length 1, alone"
            [(x, f (mkVar x))]
            [([], [(x, f (mkVar x))])]
        , test "length 1, beside related substitution"
            [(x, f (mkVar x)), (y, g (mkVar x))]
            [([(y, g (mkVar x))], [(x, f (mkVar x))])]
        , test "length 1, beside unrelated substitution"
            [(x, f (mkVar x)), (y, a)]
            [([(y, a)], [(x, f (mkVar x))])]
        , test "length 1, beside unrelated substitutions"
            [(x, f (mkVar x)), (y, g (mkVar z)), (z, b)]
            [([(z, b), (y, g b)], [(x, f (mkVar x))])]
        , test "length 1, with constructor"
            [(x, (constr1 . f) (mkVar x))]
            [([], [(x, (constr1 . f) (mkVar x))])]
        , test "length 2, alone"
            [(x, f (mkVar y)), (y, g (mkVar x))]
            [([], [(x, f (mkVar y)), (y, g (mkVar x))])]
        , test "length 2, beside related substitution"
            [(x, f (mkVar y)), (y, g (mkVar x)), (z, h (mkVar y))]
            [([(z, h (mkVar y))], [(x, f (mkVar y)), (y, g (mkVar x))])]
        , test "length 2, beside unrelated substitution"
            [(x, f (mkVar y)), (y, g (mkVar x)), (z, a)]
            [([(z, a)], [(x, f (mkVar y)), (y, g (mkVar x))])]
        , test "length 2, with And"
            [(x, mkAnd (mkVar y) a), (y, mkAnd (mkVar x) b)]
            [([], [(x, mkAnd (mkVar y) a), (y, mkAnd (mkVar x) b)])]
        , test "two cycles"
            [(x, f (mkVar x)), (y, g (mkVar y)), (z, c)]
            [([(z, c)], [(x, f (mkVar x)), (y, g (mkVar y))])]
        ]
    , testGroup "set variable simplifiable cycle"
        [ test "length 1, alone"
            [(xs, f (mkVar xs))]
            [([], [(xs, f (mkVar xs))])]
        -- UnificationError: UnsupportedPatterns
        -- , test "length 1, beside related substitution"
        --     [(xs, f (mkVar xs)), (ys, mkVar xs)]
        --     [([(ys, mkVar xs)], [(xs, f (mkVar xs))])]
        , test "length 1, beside unrelated substitution"
            [(xs, f (mkVar xs)), (ys, a)]
            [([(ys, a)], [(xs, f (mkVar xs))])]
        , test "length 1, beside unrelated substitutions"
            [(xs, f (mkVar xs)), (y, g (mkVar z)), (z, b)]
            [([(z, b), (y, g b)], [(xs, f (mkVar xs))])]
        , test "length 2, alone"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs))]
            [([], [(xs, f (mkVar ys)), (ys, g (mkVar xs))])]
        , test "length 2, beside related substitution"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs)), (z, h (mkVar ys))]
            [([(z, h (mkVar ys))], [(xs, f (mkVar ys)), (ys, g (mkVar xs))])]
        , test "length 2, beside unrelated substitution"
            [(xs, f (mkVar ys)), (ys, g (mkVar xs)), (z, a)]
            [([(z, a)], [(xs, f (mkVar ys)), (ys, g (mkVar xs))])]
        , test "length 2, with And"
            [(xs, mkAnd (mkVar ys) a), (ys, mkAnd (mkVar xs) b)]
            [([], [(xs, mkAnd (mkVar ys) a), (ys, mkAnd (mkVar xs) b)])]
        , test "two cycles"
            [(xs, f (mkVar xs)), (ys, g (mkVar ys)), (z, c)]
            [([(z, c)], [(xs, f (mkVar xs)), (ys, g (mkVar ys))])]
        ]
    , test "two simplifiable cycles, set and element variables"
        [(xs, f (mkVar xs)), (y, g (mkVar y)), (z, c)]
        [([(z, c)], [(y, g (mkVar y)), (xs, f (mkVar xs))])]
    , testGroup "element variable non-simplifiable cycle"
        [ test "alone"
            [(x, constr1 (mkVar x))]
            []
        , test "beside simplifiable cycle"
            [(x, sigma (f (mkVar x)) (mkVar x))]
            []
        ]
    , testGroup "set variable non-simplifiable cycle"
        [ test "alone"
            [(xs, constr1 (mkVar xs))]
            [([(xs, mkBottom testSort)], [])]
        , test "beside unrelated substitution"
            [(xs, constr1 (mkVar xs)), (z, a)]
            [([(xs, mkBottom testSort), (z, a)], [])]
        , test "beside related substitution"
            [(xs, constr1 (mkVar xs)), (ys, f (mkVar xs))]
            [([(xs, mkBottom testSort), (ys, f (mkBottom testSort))], [])]
        ]
    ]
  where
    test
        :: GHC.HasCallStack
        => TestName
        -> [(UnifiedVariable Variable, TermLike Variable)]
        -- ^ Test input
        -> [(UnwrappedSubstitution Variable, UnwrappedSubstitution Variable)]
        -- ^ Expected normalized, denormalized outputs
        -> TestTree
    test testName (Substitution.wrap -> input) results =
        testGroup testName
            [ testCase "simplification" $ do
                let SubstitutionSimplifier { simplifySubstitution } =
                        Substitution.simplification
                actual <- runSimplifier Mock.env $ simplifySubstitution input
                let expect = fromNormalization <$> results
                assertEqual "" expect (OrPredicate.toPredicates actual)
            , testCase "unification" $ do
                let SubstitutionSimplifier { simplifySubstitution } =
                        Substitution.unification
                actual <-
                    runSimplifier Mock.env
                    . runUnifierT
                    $ simplifySubstitution input
                let expect1 normalization@(_, denormalized)
                      | null denormalized =
                        Right $ fromNormalization normalization
                      | otherwise =
                        Left
                        $ SubstitutionError
                        $ SimplifiableCycle (fst <$> denormalized)
                    expect = (: []) <$> traverse expect1 results
                assertEqual "" expect (map OrPredicate.toPredicates <$> actual)
            ]
      where
        fromNormalization (normalized, denormalized) =
            Substitution.fromNormalization
                Normalization { normalized, denormalized }

x, y, z, xs, ys :: UnifiedVariable Variable
x = ElemVar Mock.x
y = ElemVar Mock.y
z = ElemVar Mock.z
xs = SetVar Mock.setX
ys = SetVar Mock.setY

a, b, c :: TermLike Variable
a = Mock.a
b = Mock.b
c = Mock.c

f, g, h, constr1 :: TermLike Variable -> TermLike Variable
f = Mock.f
g = Mock.g
h = Mock.h
constr1 = Mock.constr10

sigma :: TermLike Variable -> TermLike Variable -> TermLike Variable
sigma = Mock.sigma

testSort :: Sort
testSort = Mock.testSort
