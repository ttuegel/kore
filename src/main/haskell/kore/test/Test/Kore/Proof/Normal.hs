module Test.Kore.Proof.Normal where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.Reflection
                 ( give )

import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.AST.MLPatterns
import           Kore.AST.PureML
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import qualified Kore.ASTUtils.SmartConstructors as Kore
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Proof.Normal
import           Kore.Step.StepperAttributes

import           Test.Kore
import           Test.Kore.Builtin.Int
                 ( intSort )
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock

unit_constructorUnit :: Assertion
unit_constructorUnit = assertNormal unitPattern

unit_domainValue :: Assertion
unit_domainValue = assertNormal onePattern

unit_injConstructor :: Assertion
unit_injConstructor = assertNormal (mkInj unitPattern)

unit_injInj :: Assertion
unit_injInj = assertNotNormal (mkInj (mkInj unitPattern))

unit_pairConstructor :: Assertion
unit_pairConstructor = assertNormal (mkPair unitPattern unitPattern)

test_pairDomainValue :: [TestTree]
test_pairDomainValue =
    [ testNormal "(0, 0)" (mkPair zeroPattern zeroPattern)
    , testNormal "(0, 1)" (mkPair zeroPattern onePattern)
    , testNormal "(1, 1)" (mkPair onePattern onePattern)
    , testNormal "(1, 0)" (mkPair onePattern zeroPattern)
    ]

unit_fun :: Assertion
unit_fun = assertNotNormal (mkApp funSymbol [onePattern])

mkApp
    :: SymbolOrAlias Object
    -> [PureMLPattern Object var]
    -> PureMLPattern Object var
mkApp = give symbolOrAliasSorts Kore.mkApp

mkInj :: CommonPurePattern Object -> CommonPurePattern Object
mkInj input = mkApp (injSymbol inputSort supSort) [input]
  where
    inputSort =
        getPatternResultSort
            symbolOrAliasSorts
            (Functor.Foldable.project input)

mkPair
    :: CommonPurePattern Object
    -> CommonPurePattern Object
    -> CommonPurePattern Object
mkPair a b = mkApp (pairSymbol inputSort) [a, b]
  where
    inputSort =
        getPatternResultSort
            symbolOrAliasSorts
            (Functor.Foldable.project a)

mkDomainValue
    :: Sort Object
    -> BuiltinDomain (PureMLPattern Object var)
    -> PureMLPattern Object var
mkDomainValue = give symbolOrAliasSorts Kore.mkDomainValue

unitPattern :: CommonPurePattern Object
unitPattern = mkApp unitSymbol []

onePattern :: CommonPurePattern Object
onePattern =
    (mkDomainValue intSort . BuiltinDomainPattern)
        (Kore.mkStringLiteral "1")

zeroPattern :: CommonPurePattern Object
zeroPattern =
    (mkDomainValue intSort . BuiltinDomainPattern)
        (Kore.mkStringLiteral "1")

unitSort :: Sort Object
unitSort =
    SortActualSort SortActual
        { sortActualName = testId "Unit"
        , sortActualSorts = []
        }

unitSymbol :: SymbolOrAlias Object
unitSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = testId "unit"
        , symbolOrAliasParams = []
        }

pairSort :: Sort Object -> Sort Object
pairSort sort =
    SortActualSort SortActual
        { sortActualName = testId "Pair"
        , sortActualSorts = [sort]
        }

pairSymbol :: Sort Object -> SymbolOrAlias Object
pairSymbol sort =
    SymbolOrAlias
        { symbolOrAliasConstructor = testId "pair"
        , symbolOrAliasParams = [sort]
        }

injSymbol :: Sort Object -> Sort Object -> SymbolOrAlias Object
injSymbol sub sup =
    SymbolOrAlias
        { symbolOrAliasConstructor = testId "inj"
        , symbolOrAliasParams = [sub, sup]
        }

funSymbol :: SymbolOrAlias Object
funSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = testId "fun"
        , symbolOrAliasParams = []
        }

symbolOrAliasSorts :: SymbolOrAlias Object -> ApplicationSorts Object
symbolOrAliasSorts =
    Mock.makeSymbolOrAliasSorts
        [ (unitSymbol, ApplicationSorts [] unitSort)
        , (injSymbol subSort supSort, ApplicationSorts [subSort] supSort)
        , (injSymbol unitSort supSort, ApplicationSorts [unitSort] supSort)
        , (injSymbol supSort supSort, ApplicationSorts [supSort] supSort)
        , ( pairSymbol unitSort
          , ApplicationSorts [unitSort, unitSort] (pairSort unitSort)
          )
        , ( pairSymbol intSort
          , ApplicationSorts [intSort, intSort] (pairSort intSort)
          )
        , (funSymbol, ApplicationSorts [intSort] intSort)
        ]

subSort :: Sort level
subSort = (SortVariableSort . SortVariable) (testId "sub")

supSort :: Sort level
supSort = (SortVariableSort . SortVariable) (testId "sup")

symbolOrAliasAttrs :: [(SymbolOrAlias Object, StepperAttributes)]
symbolOrAliasAttrs =
    [ (unitSymbol, Mock.constructorAttributes)
    , (injSymbol subSort supSort, Mock.sortInjectionAttributes)
    , (injSymbol unitSort supSort, Mock.sortInjectionAttributes)
    , (injSymbol supSort supSort, Mock.sortInjectionAttributes)
    , (pairSymbol unitSort, Mock.constructorAttributes)
    , (pairSymbol intSort, Mock.constructorAttributes)
    , (funSymbol, Mock.functionAttributes)
    ]

tools :: MetadataTools Object StepperAttributes
tools =
    Mock.makeMetadataTools
        symbolOrAliasSorts
        symbolOrAliasAttrs
        []

assertNormal :: CommonPurePattern Object -> Assertion
assertNormal purePattern =
    assertEqual "Expected normalized pattern"
        concretePattern
        (concretePattern >>= asNormalConcreteTerm tools)
  where
    concretePattern = asConcretePurePattern purePattern

testNormal :: TestName -> CommonPurePattern Object -> TestTree
testNormal name = testCase name . assertNormal

assertNotNormal :: CommonPurePattern Object -> Assertion
assertNotNormal purePattern =
    assertEqual "Unexpected normalized pattern"
        Nothing
        (concretePattern >>= asNormalConcreteTerm tools)
  where
    concretePattern = asConcretePurePattern purePattern
