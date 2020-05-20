module Test.Kore.Step.RulePattern
    ( test_freeVariables
    , test_refreshRulePattern
    ) where

import Prelude.Kore

import Test.Tasty
import Test.Tasty.HUnit.Ext

import Data.Default
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
import Kore.Step.RulePattern
import Kore.Step.Step
    ( refreshRule
    )
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
    )

import qualified Test.Kore.Step.MockSymbols as Mock

test_freeVariables :: TestTree
test_freeVariables =
    testCase "Extract free variables" $ do
        let expect =
                foldMap freeVariable
                $ ElemVar <$> [Mock.x, Mock.z, Mock.t, Mock.u]
            actual = freeVariables testRulePattern
        assertEqual "Expected free variables" expect actual

test_refreshRulePattern :: TestTree
test_refreshRulePattern =
    testCase "Rename target variables" $ do
        let avoiding :: FreeVariables Variable
            avoiding = freeVariables testRulePattern
            (renaming, rulePattern') =
                refreshRule avoiding testRulePattern
            renamed = Set.fromList (Foldable.toList renaming)
            free' :: FreeVariables Variable
            free' = freeVariables rulePattern'
            notAvoided x = not (FreeVariables.isFreeVariable x avoiding)
        assertEqual
            "Expected to rename all free variables of original RulePattern"
            (FreeVariables.toList avoiding)
            (Map.keys renaming)
        assertBool
            "Expected to renamed variables distinct from original variables"
            (all notAvoided renamed)
        assertBool
            "Expected no free variables in common with original RulePattern"
            (all notAvoided (FreeVariables.toList free'))

testRulePattern :: RulePattern Variable
testRulePattern =
    RulePattern
        { left =
            -- Include an implicitly-quantified variable.
            mkElemVar Mock.x
        , antiLeft = Just $ mkElemVar Mock.u
        , requires = Predicate.makeCeilPredicate_ (mkElemVar Mock.z)
        , rhs = RHS
            { existentials = [Mock.y]
            , right = mkElemVar Mock.y
            , ensures = Predicate.makeCeilPredicate_ (mkElemVar Mock.t)
            }
        , attributes = def
        }
