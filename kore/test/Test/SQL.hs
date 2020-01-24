module Test.SQL
    ( testTable
    , test_Unit
    , test_Either
    , test_Maybe
    ) where

import Test.Tasty

import Data.Int
    ( Int64
    )

import SQL

import Test.Tasty.HUnit.Ext

testTable :: forall table. Table table => [table] -> TestTree
testTable rows =
    testCase "" . runTestSQL $ do
        -- create the table
        createTable (Proxy @table)
        -- insert (unique) rows
        keys1 <- traverse insertUniqueRow rows
        -- select the rows which were just inserted
        keys2 <- traverse selectRow rows
        -- assert that the inserted and selected keys are the same
        assertEqual "expected to select inserted keys" (Just <$> keys1) keys2

runTestSQL :: SQL a -> IO a
runTestSQL = runSQL ":memory:"

test_Either :: TestTree
test_Either =
    testTable @(Either Int64 Int64)
        [ Left 0
        , Right 1
        , Right 2
        ]

test_Unit :: TestTree
test_Unit = testTable [ () ]

test_Maybe :: TestTree
test_Maybe =
    testTable @(Maybe Int64)
        [ Just 0
        , Just 1
        , Just 2
        , Nothing
        ]
