module Test.Data.Kore.Variables.Int (test_int) where

import           Test.Tasty                 (TestTree)
import           Test.Tasty.HUnit           (assertEqual, testCase)

import           Test.Data.Kore

import           Data.Kore.AST.Common
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.Variables.Int

test_int :: [TestTree]
test_int =
    [ testCase "Testing intVariable Object 1"
        (assertEqual ""
            (Variable
                { variableName = testId "var_1"
                , variableSort =
                    SortVariableSort (SortVariable (testId "s"))
                }::Variable Object)
            (intVariable Variable
                { variableName = testId "v"
                , variableSort =
                    SortVariableSort (SortVariable (testId "s"))
                }
                1)
        )
    , testCase "Testing intVariable Meta 1"
        (assertEqual ""
            (Variable
                { variableName = testId "#var_1"
                , variableSort =
                    SortVariableSort (SortVariable (testId "#s"))
                }:: Variable Meta)
            (intVariable Variable
                  { variableName = testId "#v"
                  , variableSort =
                    SortVariableSort (SortVariable (testId "#s"))
                  }
            1)
        )
    ]
