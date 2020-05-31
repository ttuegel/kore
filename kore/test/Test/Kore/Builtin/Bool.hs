module Test.Kore.Builtin.Bool
    ( test_or
    , test_orElse
    , test_and
    , test_andThen
    , test_xor
    , test_ne
    , test_eq
    , test_not
    , test_implies
    , hprop_unparse
    , test_termAndEquals
    --
    , asPattern
    , asInternal
    ) where

import Prelude.Kore

import Hedgehog hiding
    ( test
    )
import qualified Hedgehog.Gen as Gen
import Test.Tasty

import Control.Error
    ( runMaybeT
    )
import qualified Data.Text as Text

import qualified Kore.Builtin.Bool as Bool
import qualified Kore.Internal.Condition as Condition
import Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike
import Kore.Step.Simplification.Data
    ( runSimplifier
    )
import qualified Kore.Step.Simplification.Not as Not
import Kore.Unification.UnifierT
    ( runUnifierT
    )

import Test.Kore.Builtin.Builtin
import Test.Kore.Builtin.Definition
import Test.SMT
import Test.Tasty.HUnit.Ext

test_or :: TestTree
test_or = testBinary orBoolSymbol (||)

test_orElse :: TestTree
test_orElse = testBinary orElseBoolSymbol (||)

test_and :: TestTree
test_and = testBinary andBoolSymbol (&&)

test_andThen :: TestTree
test_andThen = testBinary andThenBoolSymbol (&&)

test_xor :: TestTree
test_xor = testBinary xorBoolSymbol xor
  where
    xor u v = (u && not v) || (not u && v)

test_ne :: TestTree
test_ne = testBinary neBoolSymbol (/=)

test_eq :: TestTree
test_eq = testBinary eqBoolSymbol (==)

test_not :: TestTree
test_not = testUnary notBoolSymbol not

test_implies :: TestTree
test_implies = testBinary impliesBoolSymbol implies
  where
    implies u v = not u || v

-- | Specialize 'Bool.asInternal' to the builtin sort 'boolSort'.
asInternal :: Bool -> TermLike VariableName
asInternal = Bool.asInternal boolSort

-- | Specialize 'Bool.asPattern' to the builtin sort 'boolSort'.
asPattern :: Bool -> Pattern VariableName
asPattern = Bool.asPattern boolSort

-- | Test a binary operator hooked to the given symbol.
testBinary
    :: Symbol
    -- ^ hooked symbol
    -> (Bool -> Bool -> Bool)
    -- ^ operator
    -> TestTree
testBinary symb impl =
    testPropertyWithSolver (Text.unpack name) $ do
        a <- forAll Gen.bool
        b <- forAll Gen.bool
        let expect = asPattern $ impl a b
        actual <- evaluateT $ mkApplySymbol symb (asInternal <$> [a, b])
        (===) expect actual
  where
    name = expectHook symb

-- | Test a unary operator hooked to the given symbol
testUnary
    :: Symbol
    -- ^ hooked symbol
    -> (Bool -> Bool)
    -- ^ operator
    -> TestTree
testUnary symb impl =
    testPropertyWithSolver (Text.unpack name) $ do
        a <- forAll Gen.bool
        let expect = asPattern $ impl a
        actual <- evaluateT $ mkApplySymbol symb (asInternal <$> [a])
        (===) expect actual
  where
    name = expectHook symb

hprop_unparse :: Property
hprop_unparse = hpropUnparse (asInternal <$> Gen.bool)

test_termAndEquals :: [TestTree]
test_termAndEquals =
    [ testGroup "literals" $ do
        (term1, value1) <- literals
        (term2, value2) <- literals
        let result
              | value1 == value2 = [Just (Pattern.fromTermLike term1)]
              | otherwise        = []
        [test "" term1 term2 result]
    ,
        let term1 = _True
            term2 = andBool (mkVar x) (mkVar y)
            condition =
                Condition.assign x _True
                <> Condition.assign y _True
            result = [Just (Pattern.withCondition _True condition)]
        in
            test "BOOL.and - true" term1 term2 result
    ,
        let term1 = _False
            term2 = andBool (mkVar x) (mkVar y)
            result = [Nothing]
        in
            test "BOOL.and - false" term1 term2 result
    ]
  where
    _True  = asInternal True
    _False = asInternal False
    literals = [(_True, True), (_False, False)]
    x = inject (elemVarS "x" boolSort)
    y = inject (elemVarS "y" boolSort)

    test
        :: HasCallStack
        => TestName
        -> TermLike VariableName
        -> TermLike VariableName
        -> [Maybe (Pattern VariableName)]
        -> TestTree
    test testName term1 term2 expected =
        testCase testName $ do
            actual <- unify term1 term2
            assertEqual "" expected actual

    unify term1 term2 =
        run (Bool.termAndEquals termSimplifier term1 term2)
        >>= expectRight

    run =
        runNoSMT
        . runSimplifier testEnv
        . runUnifierT Not.notSimplifier
        . runMaybeT

    expectRight (Right r) = return r
    expectRight (Left  _) = assertFailure "Expected Right"

    termSimplifier = \term1 term2 ->
        runMaybeT (worker term1 term2 <|> worker term2 term1)
        >>= maybe (fallback term1 term2) return
      where
        worker term1 term2
          | ElemVar_ var <- term1 =
            Pattern.assign (inject var) term2
            & return
          | otherwise = empty
        fallback term1 term2 =
            mkAnd term1 term2
            & Pattern.fromTermLike
            & return
