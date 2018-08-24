module Test.Kore.Variables.Fresh (test_freshVariable) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( assertEqual, assertFailure, testCase )

import Control.Exception
       ( ErrorCall (ErrorCall), catch, evaluate )
import Control.Monad.State.Strict
       ( State, runState )

import           Control.Monad.Counter
                 ( Counter (..) )
import qualified Control.Monad.Counter as Monad.Counter
import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.Variables.Fresh

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

test_freshVariable :: [TestTree]
test_freshVariable =
    [ testCase "Testing freshVariable Object 2"
        (assertEqual ""
            (objectVariable { variableName = testId "var_2" }, Counter 3)
            (runState (freshVariable objectVariable) (Counter 2))
        )
    , testCase "Testing freshVariable Meta 2"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_2" }, Counter 3)
            (runState (freshVariable metaVariable) (Counter 2))
        )
    , testCase "Testing freshVariable Functor Meta 1"
        (assertEqual ""
            (( metaVariable { variableName = testId "#var_1" }
              , metaVariable { variableName = testId "#var_2" }), Counter 3)
            (runState
                ((,)
                    <$> freshVariable metaVariable
                    <*> freshVariable metaVariable
                )
                (Counter 1)
            )
          )
    , testCase "Testing freshUnifiedVariable Meta 2"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_2" }, Counter 3)
            (runState
                (freshVariable metaVariable)
                (Counter 2)
            )
        )
    , testCase "Testing failing freshVariableSuchThat Meta 1"
        ((evaluate
            (runState
                (freshVariableSuchThat metaVariable (== metaVariable))
                (Counter 2)
            )
         >> assertFailure "This evaluation should fail")
            `catch` \ (ErrorCall s) ->
                    assertEqual ""
                        "Cannot generate variable satisfying predicate"
                        s
        )
    , testCase "Testing freshVariableSuchThat Meta 1"
        (assertEqual ""
            (metaVariable { variableName = testId "#var_2" }, Counter 3)
            (runState
                (freshVariableSuchThat
                    metaVariable
                    (const True)
                )
                (Counter 2)
            )
        )
    , testCase "Testing succesful findState"
        (assertEqual ""
            (Just 1, Counter 7)
            (runState
                (Monad.Counter.findState (>0)
                    [action (-1), action 0, action 1, action (-2), action 1]
                )
                (Counter 6)
            )
        )
    , testCase "Testing unsuccesful findState"
        (assertEqual ""
            (Nothing, Counter 6)
            (runState
                (Monad.Counter.findState (>1)
                    [action (-1), action 0, action 1, action (-2), action 1]
                )
                (Counter 6)
            )
        )
    ]
  where
    action :: Int -> State Counter Int
    action n = do
        _ <- Monad.Counter.increment
        return n
