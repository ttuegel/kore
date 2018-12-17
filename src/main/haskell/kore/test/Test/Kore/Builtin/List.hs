module Test.Kore.Builtin.List where

import           Hedgehog hiding
                 ( property )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty

import           Data.Reflection
                 ( give )
import           Data.Sequence
                 ( Seq )
import qualified Data.Sequence as Seq

import           Kore.AST.Pure
import           Kore.AST.Valid
import qualified Kore.Builtin.List as List
import qualified Kore.Domain.Builtin as Domain
import           Kore.Step.ExpandedPattern
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.Pattern

import           Test.Kore
                 ( testId )
import           Test.Kore.Builtin.Builtin
import           Test.Kore.Builtin.Definition
import qualified Test.Kore.Builtin.Int as Test.Int
import           Test.SMT

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-1024) 1024)

genSeqInteger :: Gen (Seq Integer)
genSeqInteger = Gen.seq (Range.linear 0 16) genInteger

test_getUnit :: TestTree
test_getUnit =
    testPropertyWithSolver
        "get{}(unit{}(), _) === \\bottom{}()"
        property
  where
    property = do
        k <- forAll genInteger
        let patGet =
                mkApp intSort getListSymbol
                    [ mkApp listSort unitListSymbol []
                    , Test.Int.asPattern k
                    ]
            predicate = mkEquals (mkBottomOf patGet) patGet
        (===) (ExpandedPattern.bottom intSort) =<< evaluate patGet
        (===) (ExpandedPattern.top intSort) =<< evaluate predicate

test_getFirstElement :: TestTree
test_getFirstElement =
    testPropertyWithSolver
        "get{}(concat{}(element{}(e), _), 0) === e"
        property
  where
    property = do
        values <- forAll genSeqInteger
        let patGet =
                mkApp intSort getListSymbol [ patList , Test.Int.asPattern 0 ]
            patList = asPattern (Test.Int.asPattern <$> values)
            value =
                case values of
                    Seq.Empty -> Nothing
                    v Seq.:<| _ -> Just v
            patFirst = maybe (mkBottom intSort) Test.Int.asPattern value
            predicate = mkEquals patGet patFirst
        let expectGet = Test.Int.asPartialExpandedPattern value
        (===) expectGet =<< evaluate patGet
        (===) (ExpandedPattern.top intSort) =<< evaluate predicate

test_getLastElement :: TestTree
test_getLastElement =
    testPropertyWithSolver
        "get{}(concat{}(_, element{}(e)), -1) === e"
        property
  where
    property = do
        values <- forAll genSeqInteger
        let patGet = mkApp intSort getListSymbol [ patList , Test.Int.asPattern (-1) ]
            patList = asPattern (Test.Int.asPattern <$> values)
            value =
                case values of
                    Seq.Empty -> Nothing
                    _ Seq.:|> v -> Just v
            patFirst = maybe (mkBottom intSort) Test.Int.asPattern value
            predicate = give testSymbolOrAliasSorts $ mkEquals patGet patFirst
        let expectGet = Test.Int.asPartialExpandedPattern value
        (===) expectGet =<< evaluate patGet
        (===) (ExpandedPattern.top intSort) =<< evaluate predicate

test_concatUnit :: TestTree
test_concatUnit =
    testPropertyWithSolver
        "concat{}(unit{}(), xs) === concat{}(xs, unit{}()) === xs"
        property
  where
    property = give testSymbolOrAliasSorts $ do
        values <- forAll genSeqInteger
        let patUnit = mkApp listSort unitListSymbol []
            patValues = asPattern (Test.Int.asPattern <$> values)
            patConcat1 = mkApp listSort concatListSymbol [ patUnit, patValues ]
            patConcat2 = mkApp listSort concatListSymbol [ patValues, patUnit ]
            predicate1 = mkEquals patValues patConcat1
            predicate2 = mkEquals patValues patConcat2
        expectValues <- evaluate patValues
        (===) expectValues =<< evaluate patConcat1
        (===) expectValues =<< evaluate patConcat2
        (===) (ExpandedPattern.top listSort) =<< evaluate predicate1
        (===) (ExpandedPattern.top listSort) =<< evaluate predicate2

test_concatAssociates :: TestTree
test_concatAssociates =
    testPropertyWithSolver
        "concat{}(concat{}(as, bs), cs) === concat{}(as, concat{}(bs, cs))"
        property
  where
    property = give testSymbolOrAliasSorts $ do
        values1 <- forAll genSeqInteger
        values2 <- forAll genSeqInteger
        values3 <- forAll genSeqInteger
        let patList1 = asPattern $ Test.Int.asPattern <$> values1
            patList2 = asPattern $ Test.Int.asPattern <$> values2
            patList3 = asPattern $ Test.Int.asPattern <$> values3
            patConcat12 = mkApp listSort concatListSymbol [ patList1, patList2 ]
            patConcat23 = mkApp listSort concatListSymbol [ patList2, patList3 ]
            patConcat12_3 =
                mkApp listSort concatListSymbol [ patConcat12, patList3 ]
            patConcat1_23 =
                mkApp listSort concatListSymbol [ patList1, patConcat23 ]
            predicate = mkEquals patConcat12_3 patConcat1_23
        evalConcat12_3 <- evaluate patConcat12_3
        evalConcat1_23 <- evaluate patConcat1_23
        (===) evalConcat12_3 evalConcat1_23
        (===) (ExpandedPattern.top listSort) =<< evaluate predicate

-- | Check that simplification is carried out on list elements.
test_simplify :: TestTree
test_simplify =
    testPropertyWithSolver
        "simplify elements"
        property
  where
    property = do
        let
            x =
                mkVar Variable
                    { variableName = testId "x"
                    , variableSort = intSort
                    }
            original =
                mkDomainValue listSort
                $ Domain.BuiltinList (Seq.fromList [mkAnd x (mkTopOf x)])
            expected =
                ExpandedPattern.fromPurePattern
                $ mkDomainValue listSort
                $ Domain.BuiltinList (Seq.fromList [x])
        (===) expected =<< evaluate original

-- | Specialize 'List.asPattern' to the builtin sort 'listSort'.
asPattern :: List.Builtin Variable -> CommonStepPattern Object
Right asPattern = List.asPattern verifiedModule listSort

-- | Specialize 'List.asPattern' to the builtin sort 'listSort'.
asExpandedPattern :: List.Builtin Variable -> CommonExpandedPattern Object
Right asExpandedPattern = List.asExpandedPattern verifiedModule listSort
