module Test.Kore.Step.Remainder
    ( test_existentiallyQuantifyTarget
    ) where

import Prelude.Kore

import Test.Tasty

import Kore.Internal.Predicate
    ( Predicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
import Kore.Rewriting.RewritingVariable
import qualified Kore.Step.Remainder as Remainder

import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Terse

test_existentiallyQuantifyTarget :: [TestTree]
test_existentiallyQuantifyTarget =
    [ target `becomes` quantified $  "quantifies target variables"
    ]
  where
    becomes original expect =
        equals (Remainder.existentiallyQuantifyRuleVariables original) expect

target :: Predicate RewritingVariableName
target =
    Predicate.makeEqualsPredicate_
        (mkElemVar $ mkElementConfigVariable Mock.x)
        (Mock.sigma
            (mkElemVar $ mkElementRuleVariable Mock.y)
            (mkElemVar $ mkElementRuleVariable Mock.z)
        )

quantified :: Predicate RewritingVariableName
quantified =
    Predicate.makeMultipleExists
        [ mkElementRuleVariable Mock.y
        , mkElementRuleVariable Mock.z
        ]
        target
