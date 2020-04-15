module Test.Kore.Syntax.Id
    ( test_Id
    , test_Debug
    ) where

import Prelude.Kore

import Test.Tasty

import Debug
import Kore.Syntax.Id

import Test.Kore
    ( testId
    )
import Test.Terse as Terse

test_Id :: [TestTree]
test_Id =
    [ equals (testId "x") (noLocationId "x") "Eq"
    , on equals hash (testId "x") (noLocationId "x") "Hashable"
    ]

test_Debug :: [TestTree]
test_Debug =
    [ Just (testId "v")
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
