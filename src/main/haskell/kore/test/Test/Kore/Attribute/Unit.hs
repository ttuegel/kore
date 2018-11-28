module Test.Kore.Attribute.Unit where

import Test.Tasty
import Test.Tasty.HUnit

import Kore.AST.Kore
import Kore.Attribute.Unit

import Test.Kore.Attribute.Parser

parseUnit :: Attributes -> Parser Unit
parseUnit = parseAttributes

test_unit :: TestTree
test_unit =
    testCase "[unit{}()] :: Unit"
        $ expectSuccess Unit { isUnit = True }
        $ parseUnit $ Attributes [ unitAttribute ]

test_Attributes :: TestTree
test_Attributes =
    testCase "[unit{}()] :: Attributes"
        $ expectSuccess attrs $ parseAttributes attrs
  where
    attrs = Attributes [ unitAttribute ]

test_duplicate :: TestTree
test_duplicate =
    testCase "[unit{}(), unit{}()]"
        $ expectFailure
        $ parseUnit $ Attributes [ unitAttribute, unitAttribute ]

test_arguments :: TestTree
test_arguments =
    testCase "[unit{}(\"illegal\")]"
        $ expectFailure
        $ parseUnit $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (asCommonKorePattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias = unitSymbol
                , applicationChildren =
                    [ (asCommonKorePattern . StringLiteralPattern)
                        (StringLiteral "illegal")
                    ]
                }

test_parameters :: TestTree
test_parameters =
    testCase "[unit{illegal}()]"
        $ expectFailure
        $ parseUnit $ Attributes [ illegalAttribute ]
  where
    illegalAttribute =
        (asCommonKorePattern . ApplicationPattern)
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = unitId
                        , symbolOrAliasParams =
                            [ SortVariableSort (SortVariable "illegal") ]
                        }
                , applicationChildren = []
                }
