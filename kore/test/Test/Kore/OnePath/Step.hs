module Test.Kore.OnePath.Step
    ( test_onePathStrategy
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import           Data.Default
                 ( def )
import           Data.List
                 ( nub, sort )
import qualified Data.Map as Map
import           Data.Maybe
                 ( fromMaybe )
import           Numeric.Natural
                 ( Natural )

import           Data.Limit
                 ( Limit (..) )
import qualified Data.Limit as Limit
import           Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike
                 ( TermLike )
import qualified Kore.Internal.TermLike as TermLike
import           Kore.OnePath.Step
import           Kore.OnePath.StrategyPattern
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeEqualsPredicate, makeNotPredicate,
                 makeTruePredicate )
import qualified Kore.Predicate.Predicate as Syntax
                 ( Predicate )
import           Kore.Step.Rule
                 ( RewriteRule (RewriteRule), RulePattern (RulePattern) )
import           Kore.Step.Rule as RulePattern
                 ( RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.Simplifier as Simplifier
import           Kore.Step.Strategy
                 ( Strategy, pickFinal, runStrategy )
import qualified Kore.Step.Strategy as Strategy
import           Kore.Syntax.Variable
                 ( Variable (..) )
import qualified Kore.Unification.Substitution as Substitution
import qualified SMT

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

type ExecutionGraph a = Strategy.ExecutionGraph a (RewriteRule Variable)

test_onePathStrategy :: [TestTree]
test_onePathStrategy =
    [ testCase "Runs zero steps" $ do
        -- Target: a
        -- Coinductive axiom: a => b
        -- Normal axiom: a => c
        -- Start pattern: a
        -- Expected: a
        [ actual ] <- runOnePathSteps
            (Limit 0)
            (Pattern.fromTermLike Mock.a)
            Mock.a
            [simpleRewrite Mock.a Mock.b]
            [simpleRewrite Mock.a Mock.c]
        assertEqualWithExplanation ""
            (RewritePattern $ Pattern.fromTermLike Mock.a)
            actual
    , testCase "Axiom priority, first step" $ do
        -- Target: a
        -- Coinductive axiom: a => b
        -- Normal axiom: a => c
        -- Start pattern: a
        -- Expected: bottom, since a->bottom
        [ _actual ] <- runOnePathSteps
            (Limit 1)
            (Pattern.fromTermLike Mock.a)
            Mock.a
            [simpleRewrite Mock.a Mock.b]
            [simpleRewrite Mock.a Mock.c]
        assertEqualWithExplanation "" Bottom _actual

        -- Target: d
        -- Coinductive axiom: a => b
        -- Normal axiom: a => c
        -- Start pattern: a
        -- Expected: c, since coinductive axioms are applied only at the second
        -- step
        [ _actual ] <- runOnePathSteps
            (Limit 1)
            (Pattern.fromTermLike Mock.a)
            Mock.d
            [simpleRewrite Mock.a Mock.b]
            [simpleRewrite Mock.a Mock.c]
        assertEqualWithExplanation ""
            (RewritePattern $ Pattern.fromTermLike Mock.c)
            _actual
    , testCase "Axiom priority, second step" $ do
        -- Target: b
        -- Coinductive axiom: b => c
        -- Normal axiom: b => d
        -- Normal axiom: a => b
        -- Start pattern: a
        -- Expected: bottom, since a->b = target
        [ _actual ] <- runOnePathSteps
            (Limit 2)
            (Pattern.fromTermLike Mock.a)
            Mock.b
            [simpleRewrite Mock.b Mock.c]
            [ simpleRewrite Mock.b Mock.d
            , simpleRewrite Mock.a Mock.b
            ]
        assertEqualWithExplanation ""
            Bottom
            _actual

        -- Target: e
        -- Coinductive axiom: b => c
        -- Normal axiom: b => d
        -- Normal axiom: a => b
        -- Start pattern: a
        -- Expected: c, since a->b->c and b->d is ignored
        [ _actual1 ] <- runOnePathSteps
            (Limit 2)
            (Pattern.fromTermLike Mock.a)
            Mock.e
            [simpleRewrite Mock.b Mock.c]
            [ simpleRewrite Mock.b Mock.d
            , simpleRewrite Mock.a Mock.b
            ]
        assertEqualWithExplanation ""
            (sort
                [ RewritePattern $ Pattern.fromTermLike Mock.c
                ]
            )
            (sort
                [ _actual1
                ]
            )

        -- Target: e
        -- Coinductive axiom: e => c
        -- Normal axiom: b => d
        -- Normal axiom: a => b
        -- Start pattern: a
        -- Expected: d, since a->b->d
        [ _actual ] <- runOnePathSteps
            (Limit 2)
            (Pattern.fromTermLike Mock.a)
            Mock.e
            [simpleRewrite Mock.e Mock.c]
            [ simpleRewrite Mock.b Mock.d
            , simpleRewrite Mock.a Mock.b
            ]
        assertEqualWithExplanation ""
            (sort
                [ RewritePattern $ Pattern.fromTermLike Mock.d
                ]
            )
            (sort
                [ _actual
                ]
            )
    , testCase "Differentiated axioms" $ do
        -- Target: constr11(a)
        -- Coinductive axiom: constr11(a) => g(a)
        -- Coinductive axiom: constr11(b) => f(b)
        -- Normal axiom: constr11(a) => g(a)
        -- Normal axiom: constr11(b) => g(b)
        -- Normal axiom: constr11(c) => f(c)
        -- Normal axiom: constr11(x) => h(x)
        -- Normal axiom: constr10(x) => constr11(x)
        -- Start pattern: constr10(x)
        -- Expected:
        --   Bottom
        --   or (f(b) and x=b)
        --   or (f(c) and x=c)
        --   or (h(x) and x!=a and x!=b and x!=c )
        [ _actual1, _actual2, _actual3, _actual4 ] <-
            runOnePathSteps
                (Limit 2)
                (Pattern.fromTermLike
                    (Mock.functionalConstr10 (TermLike.mkVar Mock.x))
                )
                (Mock.functionalConstr11 Mock.a)
                [ simpleRewrite (Mock.functionalConstr11 Mock.a) (Mock.g Mock.a)
                , simpleRewrite (Mock.functionalConstr11 Mock.b) (Mock.f Mock.b)
                ]
                [ simpleRewrite (Mock.functionalConstr11 Mock.a) (Mock.g Mock.a)
                , simpleRewrite (Mock.functionalConstr11 Mock.b) (Mock.g Mock.b)
                , simpleRewrite (Mock.functionalConstr11 Mock.c) (Mock.f Mock.c)
                , simpleRewrite
                    (Mock.functionalConstr11 (TermLike.mkVar Mock.y))
                    (Mock.h (TermLike.mkVar Mock.y))
                , simpleRewrite
                    (Mock.functionalConstr10 (TermLike.mkVar Mock.y))
                    (Mock.functionalConstr11 (TermLike.mkVar Mock.y))
                ]
        assertEqualWithExplanation ""
            [ RewritePattern Conditional
                { term = Mock.f Mock.b
                , predicate = makeTruePredicate
                , substitution = Substitution.unsafeWrap [(Mock.x, Mock.b)]
                }
            , RewritePattern Conditional
                { term = Mock.f Mock.c
                , predicate = makeTruePredicate
                , substitution = Substitution.unsafeWrap [(Mock.x, Mock.c)]
                }
            , RewritePattern Conditional
                { term = Mock.h (TermLike.mkVar Mock.x)
                , predicate =  -- TODO(virgil): Better and simplification.
                    makeAndPredicate
                        (makeAndPredicate
                            (makeNotPredicate
                                (makeEqualsPredicate
                                    (TermLike.mkVar Mock.x) Mock.a
                                )
                            )
                            (makeNotPredicate
                                (makeEqualsPredicate
                                    (TermLike.mkVar Mock.x) Mock.b
                                )
                            )
                        )
                        (makeNotPredicate
                            (makeEqualsPredicate (TermLike.mkVar Mock.x) Mock.c)
                        )
                , substitution = mempty
                }
            , Bottom
            ]
            [ _actual1
            , _actual2
            , _actual3
            , _actual4
            ]
    , testCase "Stuck pattern" $ do
        -- Target: constr11(a)
        -- Coinductive axiom: constr11(b) => f(b)
        -- Normal axiom: constr11(c) => f(c)
        -- Normal axiom: constr10(x) => constr11(x)
        -- Start pattern: constr10(x)
        -- Expected:
        --   Bottom
        --   or (f(b) and x=b)
        --   or (f(c) and x=c)
        --   Stuck (functionalConstr11(x) and x!=a and x!=b and x!=c )
        [ _actual1, _actual2, _actual3 ] <-
            runOnePathSteps
                (Limit 2)
                (Pattern.fromTermLike
                    (Mock.functionalConstr10 (TermLike.mkVar Mock.x))
                )
                (Mock.functionalConstr11 Mock.a)
                [ simpleRewrite (Mock.functionalConstr11 Mock.b) (Mock.f Mock.b)
                ]
                [ simpleRewrite (Mock.functionalConstr11 Mock.c) (Mock.f Mock.c)
                , simpleRewrite
                    (Mock.functionalConstr10 (TermLike.mkVar Mock.y))
                    (Mock.functionalConstr11 (TermLike.mkVar Mock.y))
                ]
        assertEqualWithExplanation ""
            [ RewritePattern Conditional
                { term = Mock.f Mock.b
                , predicate = makeTruePredicate
                , substitution = Substitution.unsafeWrap [(Mock.x, Mock.b)]
                }
            , RewritePattern Conditional
                { term = Mock.f Mock.c
                , predicate = makeTruePredicate
                , substitution = Substitution.unsafeWrap [(Mock.x, Mock.c)]
                }
            , Stuck Conditional
                { term = Mock.functionalConstr11 (TermLike.mkVar Mock.x)
                , predicate =
                    makeAndPredicate
                        (makeAndPredicate
                            (makeNotPredicate
                                (makeEqualsPredicate
                                    (TermLike.mkVar Mock.x) Mock.a
                                )
                            )
                            (makeNotPredicate
                                (makeEqualsPredicate
                                    (TermLike.mkVar Mock.x) Mock.b
                                )
                            )
                        )
                        (makeNotPredicate
                            (makeEqualsPredicate (TermLike.mkVar Mock.x) Mock.c)
                        )
                , substitution = mempty
                }
            ]
            [ _actual1
            , _actual2
            , _actual3
            ]
    , testCase "Axiom with requires" $ do
        -- Target: a
        -- Coinductive axiom: n/a
        -- Normal axiom: constr10(b) => a | f(b) == c
        -- Start pattern: constr10(b)
        -- Expected: a | f(b) == c
        [ _actual1, _actual2 ] <- runOnePathSteps
            (Limit 2)
            (Pattern.fromTermLike
                (Mock.functionalConstr10 Mock.b)
            )
            Mock.a
            []
            [ rewriteWithPredicate
                (Mock.functionalConstr10 Mock.b)
                Mock.a
                $ makeEqualsPredicate
                    Mock.c
                    $ Mock.f Mock.b
            ]
        assertEqualWithExplanation ""
            [ Stuck Conditional
                { term = Mock.functionalConstr10 Mock.b
                , predicate =
                    makeNotPredicate
                        $ makeEqualsPredicate
                            Mock.c
                            $ Mock.f Mock.b
                , substitution = mempty
                }
            , Bottom
            ]
            [ _actual1
            , _actual2
            ]
    , testCase "Stuck pattern simplification" $ do
        -- Target: 1
        -- Coinductive axioms: none
        -- Normal axiom: x => 1 if x<2
        -- Start pattern: 0
        [ _actual ] <-
            runOnePathSteps
                (Limit 2)
                (Pattern.fromTermLike (Mock.builtinInt 0))
                (Mock.builtinInt 1)
                []
                [ rewriteWithPredicate
                    (TermLike.mkVar Mock.xInt)
                    (Mock.builtinInt 1)
                    (makeEqualsPredicate
                        (Mock.lessInt
                            (TermLike.mkVar Mock.xInt) (Mock.builtinInt 2)
                        )
                        (Mock.builtinBool True)
                    )
                ]
        assertEqualWithExplanation ""
            Bottom
            _actual
    ]

simpleRewrite
    :: TermLike Variable
    -> TermLike Variable
    -> RewriteRule Variable
simpleRewrite left right =
    RewriteRule RulePattern
        { left = left
        , right = right
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }

rewriteWithPredicate
    :: TermLike Variable
    -> TermLike Variable
    -> Syntax.Predicate Variable
    -> RewriteRule Variable
rewriteWithPredicate left right predicate =
    RewriteRule RulePattern
        { left = left
        , right = right
        , requires = predicate
        , ensures = makeTruePredicate
        , attributes = def
        }

runSteps
    :: (ExecutionGraph (CommonStrategyPattern) -> Maybe (ExecutionGraph b))
    -> (ExecutionGraph b -> a)
    -> Pattern Variable
    -- ^left-hand-side of unification
    -> [Strategy (Prim (Pattern Variable) (RewriteRule Variable))]
    -> IO a
runSteps graphFilter picker configuration strategy =
    (<$>) picker
    $ SMT.runSMT SMT.defaultConfig emptyLogger
    $ evalSimplifier Mock.env
    $ (fromMaybe (error "Unexpected missing tree") . graphFilter)
    <$> runStrategy
        (transitionRule
            metadataTools
            (Mock.substitutionSimplifier metadataTools)
            simplifier
            Map.empty
        )
        strategy
        (RewritePattern configuration)
  where
    metadataTools = Mock.metadataTools
    simplifier = Simplifier.create Map.empty

runOnePathSteps
    :: Limit Natural
    -> Pattern Variable
    -- ^left-hand-side of unification
    -> TermLike Variable
    -> [RewriteRule Variable]
    -> [RewriteRule Variable]
    -> IO [CommonStrategyPattern]
runOnePathSteps
    stepLimit
    configuration
    target
    coinductiveRewrites
    rewrites
  = do
    result <- runSteps
        Just
        pickFinal
        configuration
        (Limit.takeWithin
            stepLimit
            ( onePathFirstStep expandedTarget rewrites
            : repeat
                (onePathFollowupStep
                    expandedTarget
                    coinductiveRewrites
                    rewrites
                )
            )
        )
    return (sort $ nub result)
  where
    expandedTarget = Pattern.fromTermLike target
