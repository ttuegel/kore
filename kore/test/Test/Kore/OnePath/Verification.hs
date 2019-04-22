module Test.Kore.OnePath.Verification
    ( test_onePathVerification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import           Control.Monad.Trans.Except
                 ( runExceptT )
import           Data.Coerce
                 ( coerce )
import           Data.Default
                 ( def )
import           Data.Limit
                 ( Limit (..) )
import qualified Data.Map as Map
import           Numeric.Natural
                 ( Natural )

import           Kore.AST.Pure
import           Kore.AST.Valid
import qualified Kore.Attribute.Axiom as Attribute
import           Kore.Attribute.Symbol
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import qualified Kore.OnePath.Verification as OnePath
import qualified Kore.OnePath.Verification as Claim
import           Kore.Predicate.Predicate
                 ( makeEqualsPredicate, makeNotPredicate, makeTruePredicate )
import           Kore.Step.Pattern
                 ( CommonStepPattern )
import           Kore.Step.Representation.ExpandedPattern
                 ( Predicated (Predicated) )
import           Kore.Step.Representation.ExpandedPattern as Predicated
                 ( Predicated (..) )
import           Kore.Step.Representation.ExpandedPattern as ExpandedPattern
                 ( fromPurePattern )
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Representation.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern )
import           Kore.Step.Rule
                 ( OnePathRule (..), RewriteRule (..),
                 RulePattern (RulePattern) )
import           Kore.Step.Rule as RulePattern
                 ( RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.Simplifier as Simplifier
import qualified SMT

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

test_onePathVerification :: [TestTree]
test_onePathVerification =
    [ testCase "Runs zero steps" $ do
        -- Axiom: a => b
        -- Claim: a => b
        -- Expected: error a
        actual <- runVerification
            metadataTools
            (Limit 0)
            [simpleAxiom Mock.a Mock.b]
            [simpleClaim Mock.a Mock.b]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.a])
            actual
    , testCase "Runs one step" $ do
        -- Axiom: a => b
        -- Claim: a => b
        -- Expected: error b
        -- Note that the check that we have reached the destination happens
        -- at the beginning of each step. At the beginning of the first step
        -- the pattern is 'a', so we didn't reach our destination yet, even if
        -- the rewrite transforms 'a' into 'b'. We detect the success at the
        -- beginning of the second step, which does not run here.
        actual <- runVerification
            metadataTools
            (Limit 1)
            [simpleAxiom Mock.a Mock.b]
            [simpleClaim Mock.a Mock.b]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.b])
            actual
    , testCase "Returns multiple results" $ do
        -- Axiom: a => b or c
        -- Claim: a => d
        -- Expected: error [b, c]
        actual <- runVerification
            metadataTools
            (Limit 1)
            [simpleAxiom Mock.a (mkOr Mock.b Mock.c)]
            [simpleClaim Mock.a Mock.d]
        assertEqualWithExplanation ""
            (Left . MultiOr.make $
                ExpandedPattern.fromPurePattern <$> [Mock.b, Mock.c]
            )
            actual
    , testCase "Verifies one claim" $ do
        -- Axiom: a => b
        -- Claim: a => b
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 2)
            [simpleAxiom Mock.a Mock.b]
            [simpleClaim Mock.a Mock.b]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "Trusted claim cannot prove itself" $ do
        -- Trusted Claim: a => b
        -- Expected: error a
        actual <- runVerification
            metadataTools
            (Limit 4)
            []
            [ simpleTrustedClaim Mock.a Mock.b
            , simpleClaim Mock.a Mock.b
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.a])
            actual
    , testCase "Verifies one claim multiple steps" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Claim: a => c
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            ]
            [simpleClaim Mock.a Mock.c]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "Verifies one claim stops early" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Claim: a => b
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            ]
            [simpleClaim Mock.a Mock.c]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "Verifies one claim with branching" $ do
        -- Axiom: constr11(a) => b
        -- Axiom: constr11(x) => b
        -- Axiom: constr10(x) => constr11(x)
        -- Claim: constr10(x) => b
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom (Mock.functionalConstr11 Mock.a) Mock.b
            , simpleAxiom (Mock.functionalConstr11 (mkVar Mock.x)) Mock.b
            , simpleAxiom
                (Mock.functionalConstr10 (mkVar Mock.x))
                (Mock.functionalConstr11 (mkVar Mock.x))
            ]
            [simpleClaim (Mock.functionalConstr10 (mkVar Mock.x)) Mock.b]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "Partial verification failure" $ do
        -- Axiom: constr11(a) => b
        -- Axiom: constr10(x) => constr11(x)
        -- Claim: constr10(x) => b
        -- Expected: error constr11(x) and x != a
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom (Mock.functionalConstr11 Mock.a) Mock.b
            , simpleAxiom
                (Mock.functionalConstr10 (mkVar Mock.x))
                (Mock.functionalConstr11 (mkVar Mock.x))
            ]
            [simpleClaim (Mock.functionalConstr10 (mkVar Mock.x)) Mock.b]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make
                [ Predicated
                    { term = Mock.functionalConstr11 (mkVar Mock.x)
                    , predicate =
                        makeNotPredicate
                            (makeEqualsPredicate (mkVar Mock.x) Mock.a)
                    , substitution = mempty
                    }
                ]
            )
            actual
    , testCase "Verifies two claims" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Axiom: d => e
        -- Claim: a => c
        -- Claim: d => e
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            , simpleAxiom Mock.d Mock.e
            ]
            [ simpleClaim Mock.a Mock.c
            , simpleClaim Mock.d Mock.e
            ]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "fails first of two claims" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Axiom: d => e
        -- Claim: a => e
        -- Claim: d => e
        -- Expected: error c
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            , simpleAxiom Mock.d Mock.e
            ]
            [ simpleClaim Mock.a Mock.e
            , simpleClaim Mock.d Mock.e
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.c])
            actual
    , testCase "fails second of two claims" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Axiom: d => e
        -- Claim: a => c
        -- Claim: d => c
        -- Expected: error e
        actual <- runVerification
            metadataTools
            (Limit 3)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            , simpleAxiom Mock.d Mock.e
            ]
            [ simpleClaim Mock.a Mock.c
            , simpleClaim Mock.d Mock.c
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.e])
            actual
    , testCase "second proves first but fails" $ do
        -- Axiom: a => b
        -- Axiom: c => d
        -- Claim: a => d
        -- Claim: b => c
        -- Expected: error b
        actual <- runVerification
            metadataTools
            (Limit 4)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.c Mock.d
            ]
            [ simpleClaim Mock.a Mock.d
            , simpleClaim Mock.b Mock.c
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.b])
            actual
    , testCase "first proves second but fails" $ do
        -- Axiom: a => b
        -- Axiom: c => d
        -- Claim: b => c
        -- Claim: a => d
        -- Expected: error b
        actual <- runVerification
            metadataTools
            (Limit 4)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.c Mock.d
            ]
            [ simpleClaim Mock.b Mock.c
            , simpleClaim Mock.a Mock.d
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.b])
            actual
    , testCase "trusted second proves first" $ do
        -- Axiom: a => b
        -- Axiom: c => d
        -- Claim: a => d
        -- Trusted Claim: b => c
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 4)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.c Mock.d
            ]
            [ simpleClaim Mock.a Mock.d
            , simpleTrustedClaim Mock.b Mock.c
            ]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "trusted first proves second" $ do
        -- Axiom: a => b
        -- Axiom: c => d
        -- Claim: b => c
        -- Claim: a => d
        -- Expected: success
        actual <- runVerification
            metadataTools
            (Limit 4)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.c Mock.d
            ]
            [ simpleTrustedClaim Mock.b Mock.c
            , simpleClaim Mock.a Mock.d
            ]
        assertEqualWithExplanation ""
            (Right ())
            actual
    , testCase "Prefers using claims for rewriting" $ do
        -- Axiom: a => b
        -- Axiom: b => c
        -- Axiom: c => d
        -- Claim: a => d
        -- Claim: b => e
        -- Expected: error e
        --    first verification: a=>b=>e,
        --        without second claim would be: a=>b=>c=>d
        --    second verification: b=>c=>d, not visible here
        actual <- runVerification
            metadataTools
            (Limit 4)
            [ simpleAxiom Mock.a Mock.b
            , simpleAxiom Mock.b Mock.c
            , simpleAxiom Mock.c Mock.d
            ]
            [ simpleClaim Mock.a Mock.d
            , simpleClaim Mock.b Mock.e
            ]
        assertEqualWithExplanation ""
            (Left $ MultiOr.make [ExpandedPattern.fromPurePattern Mock.e])
            actual
    ]
  where
    metadataTools :: MetadataTools Object StepperAttributes
    metadataTools =
        Mock.makeMetadataTools
            Mock.attributesMapping
            Mock.headTypeMapping
            Mock.sortAttributesMapping
            Mock.subsorts
            Mock.headSortsMapping

simpleAxiom
    :: MetaOrObject level
    => CommonStepPattern level
    -> CommonStepPattern level
    -> OnePath.Axiom level
simpleAxiom left right =
    OnePath.Axiom $ simpleRewrite left right

simpleClaim
    :: MetaOrObject level
    => CommonStepPattern level
    -> CommonStepPattern level
    -> OnePathRule level Variable
simpleClaim left right =
    OnePathRule . getRewriteRule $ simpleRewrite left right

simpleTrustedClaim
    :: MetaOrObject level
    => CommonStepPattern level
    -> CommonStepPattern level
    -> OnePathRule level Variable
simpleTrustedClaim left right =
    OnePathRule
    $ RulePattern
            { left = left
            , right = right
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
                { Attribute.trusted = Attribute.Trusted True }
            }

simpleRewrite
    :: MetaOrObject level
    => CommonStepPattern level
    -> CommonStepPattern level
    -> RewriteRule level Variable
simpleRewrite left right =
    RewriteRule RulePattern
        { left = left
        , right = right
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }

runVerification
    ::  ( MetaOrObject level
        , OnePath.Claim claim
        )
    => MetadataTools level StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> Limit Natural
    -> [OnePath.Axiom level]
    -> [claim]
    -> IO (Either (CommonOrOfExpandedPattern level) ())
runVerification
    metadataTools
    stepLimit
    axioms
    claims
  =
    SMT.runSMT SMT.defaultConfig
    $ evalSimplifier emptyLogger
    $ runExceptT
    $ OnePath.verify
        metadataTools
        simplifier
        (Mock.substitutionSimplifier metadataTools)
        Map.empty
        (OnePath.defaultStrategy claims axioms)
        ( map (\c -> (RewriteRule . coerce $ c, stepLimit))
        . filter (not . Claim.isTrusted)
        $ claims)
  where
    simplifier = Simplifier.create metadataTools Map.empty
