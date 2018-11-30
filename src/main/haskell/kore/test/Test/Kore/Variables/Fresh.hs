module Test.Kore.Variables.Fresh (test_freshVariable) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( assertEqual, assertFailure, testCase )

import Control.DeepSeq
       ( rnf )
import Control.Exception
       ( ErrorCall (ErrorCall), catch, evaluate )

import Control.Monad.Counter
import Kore.AST.Common
import Kore.AST.MetaOrObject
import Kore.Sort
import Kore.Variables.Fresh

import Test.Kore

objectVariable :: Variable Object
objectVariable = Variable
    { variableName = testId "v"
    , variableSort = SortVariableSort (SortVariable (testId "s"))
    }

metaVariable :: Variable Meta
metaVariable = Variable
    { variableName = testId "#v"
    , variableSort = SortVariableSort (SortVariable (testId "#s"))
    }

metaFreshVariable :: Variable Meta
metaFreshVariable = Variable
    { variableName = testId "#var_v_1"
    , variableSort = SortVariableSort (SortVariable (testId "#s"))
    }

test_freshVariable :: [TestTree]
test_freshVariable =
    [ testCase "Testing freshVariable Object 2"
        (assertEqual ""
            (objectVariable { variableName = testId "var_v_2" }, 3)
            (runCounter
                (freshVariable objectVariable)
                2
            )
        )
    , testCase "Testing freshVariable Meta 2"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_v_2" }, 3)
            (runCounter
                (freshVariable metaVariable)
                2
            )
        )
    , testCase "Testing freshVariable over fresh variable"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_v_2" }, 3)
            (runCounter
                (freshVariable metaFreshVariable)
                2
            )
        )
    , testCase "Testing freshVariable Functor Meta 1"
        (assertEqual ""
            ( ( metaVariable { variableName = testId "#var_v_1" }
              , metaVariable { variableName = testId "#var_v_2" }
              )
            , 3
            )
            (runCounter
                ((,)
                    <$> freshVariable metaVariable
                    <*> freshVariable metaVariable
                )
                1
            )
          )
    , testCase "Testing freshUnifiedVariable Meta 2"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_v_2" }, 3)
            (runCounter
                (freshVariable metaVariable)
                2
            )
        )
    , testCase "Testing failing freshVariableSuchThat Meta 1"
        (let
            freshen = freshVariableSuchThat metaVariable (== metaVariable)
            test = do
                let var = runCounter freshen 2
                evaluate (rnf var)
                assertFailure "This evaluation should fail"
            handler (ErrorCall s) =
                assertEqual ""
                    "Cannot generate variable satisfying predicate"
                    s
         in
           catch test handler
        )
    , testCase "Testing freshVariableSuchThat Meta 1"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_v_2" }, 3)
            (runCounter
                (freshVariableSuchThat
                    metaVariable
                    (const True)
                )
                2
            )
        )
    , testCase "Testing successful findState"
        (assertEqual ""
            (Just 1, 7)
            (runCounter
                (findState (>0)
                    [action (-1), action 0, action 1, action (-2), action 1]
                )
                6
            )
        )
    , testCase "Testing unsuccessful findState"
        (assertEqual ""
            (Nothing, 6)
            (runCounter
                (findState (>1)
                    [action (-1), action 0, action 1, action (-2), action 1]
                )
                6
            )
        )
    ]
  where
    action :: Int -> Counter Int
    action n = do
        _ <- increment
        return n
