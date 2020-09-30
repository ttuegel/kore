module Test.Kore.Step.Rule.Simplify
    ( test_simplifyRule_RewriteRule
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Control.Lens as Lens
import Control.Monad.Morph
    ( MFunctor (..)
    )
import Control.Monad.Reader
    ( MonadReader
    , ReaderT
    )
import qualified Control.Monad.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Generics.Product
    ( field
    )

import Kore.Internal.Condition
    ( Condition
    )
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.MultiAnd as MultiAnd
import qualified Kore.Internal.OrPattern as OrPattern
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , makeAndPredicate
    , makeCeilPredicate
    , makeEqualsPredicate
    , makeTruePredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Internal.SideCondition as SideCondition
import Kore.Internal.TermLike
    ( AdjSomeVariableName
    , InternalVariable
    , TermLike
    , mkAnd
    , mkElemVar
    , mkEquals
    , mkOr
    , termLikeSort
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Reachability
    ( OnePathClaim (..)
    )
import Kore.Rewriting.RewritingVariable
    ( RewritingVariableName
    , getRewritingVariable
    )
import Kore.Sort
    ( predicateSort
    )
import Kore.Step.Rule.Simplify
import Kore.Step.RulePattern
    ( RewriteRule
    )
import Kore.Step.Simplification.Data
    ( runSimplifier
    )
import Kore.Step.Simplification.Simplify
    ( MonadSMT
    , MonadSimplify (..)
    )
import qualified Kore.Step.SMT.Declaration.All as SMT.All
import Kore.Syntax.Variable
    ( VariableName
    , fromVariableName
    )
import Log

import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Rule.Common
    ( Pair (..)
    , RuleBase
    )
import qualified Test.Kore.Step.Rule.Common as Common
import Test.SMT
    ( runNoSMT
    )
import Test.Tasty.HUnit.Ext

test_simplifyRule_RewriteRule :: [TestTree]
test_simplifyRule_RewriteRule =
    [ testCase "No simplification needed" $ do
        let rule = Mock.a `rewritesToWithSortRewriteRule` Mock.cf
            expected = [rule]

        actual <- runSimplifyRuleNoSMT rule

        assertEqual "" expected actual

    , testCase "Simplify lhs term" $ do
        let expected = [Mock.a `rewritesToWithSortRewriteRule` Mock.cf]

        actual <- runSimplifyRuleNoSMT
            (   mkAnd Mock.a (mkEquals Mock.testSort Mock.a Mock.a)
                `rewritesToWithSortRewriteRule`
                Mock.cf
            )

        assertEqual "" expected actual

    , testCase "Does not simplify rhs term" $ do
        let rule =
                Mock.a
                `rewritesToWithSortRewriteRule`
                mkAnd Mock.cf (mkEquals Mock.testSort Mock.a Mock.a)
            expected = [rule]

        actual <- runSimplifyRuleNoSMT rule

        assertEqual "" expected actual

    , testCase "Substitution in lhs term" $ do
        let expected = [Mock.a `rewritesToWithSortRewriteRule` Mock.f Mock.b]

        actual <- runSimplifyRuleNoSMT
            (   mkAnd Mock.a (mkEquals Mock.testSort Mock.b x)
                `rewritesToWithSortRewriteRule` Mock.f x
            )

        assertEqual "" expected actual

    , testCase "Does not simplify ensures predicate" $ do
        let rule =
                Pair (Mock.a,  makeTruePredicate Mock.testSort)
                `rewritesToWithSortRewriteRule`
                Pair (Mock.cf, makeEqualsPredicate Mock.testSort Mock.b Mock.b)
            expected = [rule]

        actual <- runSimplifyRuleNoSMT rule

        assertEqual "" expected actual

    , testCase "Splits rule" $ do
        let expected =
                [ Mock.a `rewritesToWithSortRewriteRule` Mock.cf
                , Mock.b `rewritesToWithSortRewriteRule` Mock.cf
                ]

        actual <- runSimplifyRuleNoSMT
            (   mkOr Mock.a Mock.b
                `rewritesToWithSortRewriteRule`
                Mock.cf
            )

        assertEqual "" expected actual
    , testCase "f(x) is always defined" $ do
        let expected =
                [ Mock.functional10 x `rewritesToWithSortRewriteRule` Mock.a
                ]

        actual <- runSimplifyRuleNoSMT
            (   Pair (Mock.functional10 x, makeTruePredicate Mock.testSort)
                `rewritesToWithSortRewriteRule`
                Pair (Mock.a, makeTruePredicate Mock.testSort)
            )

        assertEqual "" expected actual
    ]
  where
    rewritesToWithSortRewriteRule
        :: RuleBase base (RewriteRule VariableName)
        => base VariableName
        -> base VariableName
        -> RewriteRule VariableName
    rewritesToWithSortRewriteRule = Common.rewritesToWithSort

    x = mkElemVar Mock.x

runSimplifyRuleNoSMT
    :: SimplifyRuleLHS rule
    => rule
    -> IO [rule]
runSimplifyRuleNoSMT rule =
    fmap Foldable.toList
    $ runNoSMT
    $ runSimplifier Mock.env $ do
        SMT.All.declare Mock.smtDeclarations
        simplifyRuleLhs rule

data TestEnv =
    TestEnv
    { replacements
        :: ![(TermLike RewritingVariableName, TermLike RewritingVariableName)]
    , input :: !OnePathClaim
    , requires :: !(Condition RewritingVariableName)
    }

newtype TestSimplifierT m a =
    TestSimplifierT { runTestSimplifierT :: ReaderT TestEnv m a }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader TestEnv)
    deriving newtype (MonadLog, MonadSMT)

instance MonadTrans TestSimplifierT where
    lift = TestSimplifierT . lift

instance MFunctor TestSimplifierT where
    hoist f = TestSimplifierT . hoist f . runTestSimplifierT

instance MonadSimplify m => MonadSimplify (TestSimplifierT m) where
    simplifyTermLike sideCondition termLike = do
        TestEnv { replacements, input, requires } <- Reader.ask
        let rule = getOnePathClaim input
            leftTerm =
                Lens.view (field @"left") rule
                & Pattern.term
            sort = termLikeSort leftTerm
            expectSideCondition =
                makeAndPredicate
                    (Condition.toPredicate requires)
                    (makeCeilPredicate sort leftTerm)
                & liftPredicate
                & Predicate.coerceSort predicateSort
                & Condition.fromPredicate
                & SideCondition.fromCondition
            -- Equivalent under associativity of \\and
            checkEquivalence cond1 cond2 =
                (==)
                    (cond1 & SideCondition.toPredicate & MultiAnd.fromPredicate)
                    (cond2 & SideCondition.toPredicate & MultiAnd.fromPredicate)
            satisfied = checkEquivalence sideCondition expectSideCondition
        return
            . OrPattern.fromTermLike
            . (if satisfied then applyReplacements replacements else id)
            $ termLike
      where
        applyReplacements
            :: InternalVariable variable
            => [(TermLike RewritingVariableName, TermLike RewritingVariableName)]
            -> TermLike variable
            -> TermLike variable
        applyReplacements replacements zero =
            Foldable.foldl' applyReplacement zero
            $ fmap liftReplacement replacements

        applyReplacement orig (ini, fin)
          | orig == ini = fin
          | otherwise   = orig

        liftPredicate
            :: InternalVariable variable
            => Predicate RewritingVariableName
            -> Predicate variable
        liftPredicate =
            Predicate.mapVariables liftRewritingVariable

        liftTermLike
            :: InternalVariable variable
            => TermLike RewritingVariableName
            -> TermLike variable
        liftTermLike =
            TermLike.mapVariables liftRewritingVariable

        liftReplacement
            :: InternalVariable variable
            => (TermLike RewritingVariableName, TermLike RewritingVariableName)
            -> (TermLike variable, TermLike variable)
        liftReplacement = Bifunctor.bimap liftTermLike liftTermLike

        liftRewritingVariable
            :: InternalVariable variable
            => AdjSomeVariableName (RewritingVariableName -> variable)
        liftRewritingVariable =
            pure (.) <*> pure fromVariableName <*> getRewritingVariable
