module Test.Kore.Syntax.Variable
    ( test_Debug
    ) where

import Prelude.Kore

import Test.Tasty

import Debug
import Kore.Sort
import Kore.Syntax.Variable

import Test.Kore
import qualified Test.Terse as Terse

test_Debug :: [TestTree]
test_Debug =
    [ Variable
        { variableName = testId "v"
        , variableCounter = mempty
        , variableSort = SortVariableSort (SortVariable (testId "sv"))
        }
        `yields`
        "Variable\n\
        \{ variableName = Id { getId = \"v\", idLocation = AstLocationTest }\n\
        \, variableCounter = Nothing\n\
        \, variableSort =\n\
        \    SortVariableSort\n\
        \        SortVariable\n\
        \        { getSortVariable = Id { getId = \"sv\", idLocation = AstLocationTest }\n\
        \        }\n\
        \}"
        $  "Variable"
    , Just (testId "v")
        `yields`
        "Just Id { getId = \"v\", idLocation = AstLocationTest }"
        $ "Maybe - Just"
    , (Nothing :: Maybe Id)
        `yields`
        "Nothing"
        $ "Maybe - Nothing"
    ]
  where
    yields input = Terse.equals (show $ debug input)
