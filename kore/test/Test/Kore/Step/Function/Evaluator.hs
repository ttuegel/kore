module Test.Kore.Step.Function.Evaluator
    ( test_evaluateApplication ) where

import Prelude.Kore

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Kore.Attribute.Synthetic
    ( synthesize
    )
import qualified Kore.Equation as Equation
import Kore.Internal.Condition
    ( Condition
    )
import qualified Kore.Internal.Condition as Condition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.TermLike
    ( Application
    , Symbol
    , TermLike
    , VariableName
    )
import qualified Kore.Internal.TermLike as TermLike
import qualified Kore.Step.Axiom.EvaluationStrategy as Kore
import qualified Kore.Step.Axiom.Identifier as Axiom.Identifier
import qualified Kore.Step.Function.Evaluator as Kore
import qualified Kore.Step.Simplification.Simplify as Kore
import Kore.Syntax.Application
    ( Application (..)
    )

import Test.Kore
    ( testId
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import qualified Test.Kore.Step.Simplification as Test

test_evaluateApplication :: [TestTree]
test_evaluateApplication =
    [ evaluates "f(a) -- f(x) => x" (f a) a
    , notEvaluates "g(a) -- no axioms" (g a) id
    ]
  where
    a = Mock.a
    evaluates
        :: HasCallStack
        => TestName
        -> Application Symbol (TermLike VariableName)
        -> TermLike VariableName
        -> TestTree
    evaluates name origin expect =
        makeTest
            name
            origin
            (OrPattern.fromTermLike expect)
    notEvaluates
        :: HasCallStack
        => TestName
        -> Application Symbol (TermLike VariableName)
        -> (TermLike VariableName -> TermLike VariableName)
        -> TestTree
    notEvaluates name origin mkExpect =
        makeTest
            name
            origin
            (OrPattern.fromTermLike $ mkExpect $ mkApplySymbol origin)

    makeTest
        :: HasCallStack
        => TestName
        -> Application Symbol (TermLike VariableName)
        -> OrPattern VariableName
        -> TestTree
    makeTest name origin expect =
        testCase name $ do
            actual <- evaluateApplication Condition.top origin
            assertEqual "" expect actual

fSymbol, gSymbol :: Symbol
fSymbol = Mock.fSymbol
gSymbol = Mock.gSymbol

f, g :: child -> Application Symbol child
f x = Application fSymbol [x]
g x = Application gSymbol [x]

mkApplySymbol :: Application Symbol (TermLike VariableName) -> TermLike VariableName
mkApplySymbol = synthesize . TermLike.ApplySymbolF

fEvaluator :: Kore.BuiltinAndAxiomSimplifier
fEvaluator =
    Kore.simplificationEvaluation
    $ Equation.mkEquation sortR left right
  where
    sortR = TermLike.mkSortVariable (testId "R")
    left = mkApplySymbol (f x)
    right = x
    x = TermLike.mkElemVar Mock.x

evaluateApplication
    :: Condition VariableName
    -> Application Symbol (TermLike VariableName)
    -> IO (OrPattern VariableName)
evaluateApplication predicate =
    Test.runSimplifier env
    . Kore.evaluateApplication SideCondition.top predicate

simplifierAxioms :: Kore.BuiltinAndAxiomSimplifierMap
simplifierAxioms = Map.fromList [ (fId, fEvaluator) ]
  where
    fId = Axiom.Identifier.Application (TermLike.symbolConstructor fSymbol)

env :: Test.Env Test.Simplifier
env = Mock.env { Test.simplifierAxioms = simplifierAxioms }
