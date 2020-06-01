module Test.Kore.Step.Rule.Expand
    ( test_expandRule
    ) where

import Prelude.Kore

import Test.Tasty

import Data.Default
    ( def
    )
import Data.Generics.Product
    ( field
    )
import qualified Data.Map.Strict as Map

import qualified Control.Lens as Lens
import Data.Sup
    ( Sup (Element)
    )
import qualified Kore.Attribute.Sort.Constructors as Attribute
    ( Constructor (Constructor)
    , ConstructorLike (..)
    , Constructors (Constructors)
    )
import qualified Kore.Attribute.Sort.Constructors as Constructors.DoNotUse
import qualified Kore.Attribute.Symbol as Attribute
    ( Symbol
    )
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    )
import qualified Kore.IndexedModule.MetadataTools as MetadataTools
    ( MetadataTools (..)
    )
import Kore.Internal.Predicate
    ( Predicate
    , makeEqualsPredicate_
    , makeTruePredicate_
    )
import Kore.Internal.Symbol
    ( Symbol
    )
import qualified Kore.Internal.Symbol as Symbol
    ( constructor
    , functional
    )
import Kore.Internal.TermLike
    ( TermLike
    , mkApplySymbol
    , mkElemVar
    )
import Kore.Step.Rule.Expand
import Kore.Step.RulePattern
    ( OnePathRule (OnePathRule)
    , RHS (..)
    , RulePattern (RulePattern)
    )
import qualified Kore.Step.RulePattern as Rule.DoNotUse
import Kore.Syntax.Id
    ( Id
    )
import Kore.Syntax.Variable

import Test.Kore
    ( testId
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.With
    ( with
    )
import Test.Tasty.HUnit.Ext

class OnePathRuleBase base where
    rewritesTo :: base VariableName -> base VariableName -> OnePathRule

newtype Pair variable = Pair (TermLike variable, Predicate variable)

instance OnePathRuleBase Pair where
    Pair (t1, p1) `rewritesTo` Pair (t2, p2) =
        OnePathRule RulePattern
            { left = t1
            , requires = p1
            , rhs = RHS
                { existentials = []
                , right = t2
                , ensures = p2
                }
            , antiLeft = Nothing
            , attributes = def
            }

instance OnePathRuleBase TermLike where
    t1 `rewritesTo` t2 =
        Pair (t1, makeTruePredicate_) `rewritesTo` Pair (t2, makeTruePredicate_)


test_expandRule :: [TestTree]
test_expandRule =
    [ testCase "Nothing to expand" $
        let expected = Mock.f x `rewritesTo` Mock.g x
            actual =
                expandSingleConstructors
                    (metadataTools [])
                    (Mock.f x `rewritesTo` Mock.g x)
        in assertEqual "" expected actual
    , testCase "Nothing to expand without constructors" $
        let expected = Mock.f x `rewritesTo` Mock.g x
            actual =
                expandSingleConstructors
                    (metadataTools
                        [ (Mock.testSortId, noConstructor) ]
                    )
                    (Mock.f x `rewritesTo` Mock.g x)
        in assertEqual "" expected actual
    , testCase "Nothing to expand with multiple constructors" $
        let expected = Mock.f x `rewritesTo` Mock.g x
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSortId
                            , noConstructor
                                `with` constructor Mock.aSymbol
                                `with` constructor Mock.bSymbol
                            )
                        ]
                    )
                    (Mock.f x `rewritesTo` Mock.g x)
        in assertEqual "" expected actual
    , testCase "Expands variable once to constant" $
        let expected =
                Pair (Mock.f Mock.a, makeEqualsPredicate_ x Mock.a)
                `rewritesTo`
                Pair (Mock.g Mock.a, makeTruePredicate_)
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSortId
                            , noConstructor `with` constructor Mock.aSymbol
                            )
                        ]
                    )
                    (Mock.f x `rewritesTo` Mock.g x)
        in assertEqual "" expected actual
    , testCase "Expands variable once to argument constructor" $
        let expected =
                Pair
                    ( Mock.fSort0 (expandableConstructor1 x00TestSort)
                    , makeEqualsPredicate_
                        x0
                        (expandableConstructor1 x00TestSort)
                    )
                `rewritesTo`
                Pair
                    ( Mock.gSort0 (expandableConstructor1 x00TestSort)
                    , makeTruePredicate_
                    )
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSort0Id
                            , noConstructor
                                `with`
                                    ( constructor expandableConstructor1Symbol
                                    `with` Mock.testSort
                                    )
                            )
                        ]
                    )
                    (Mock.fSort0 x0 `rewritesTo` Mock.gSort0 x0)
        in assertEqual "" expected actual
    , testCase "Expands variable twice." $
        let expected =
                Pair
                    ( Mock.fSort0 (expandableConstructor1 Mock.a)
                    , makeEqualsPredicate_
                        x0
                        (expandableConstructor1 Mock.a)
                    )
                `rewritesTo`
                Pair
                    ( Mock.gSort0 (expandableConstructor1 Mock.a)
                    , makeTruePredicate_
                    )
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSort0Id
                            , noConstructor
                                `with`
                                    ( constructor expandableConstructor1Symbol
                                    `with` Mock.testSort
                                    )
                            )
                        ,   ( Mock.testSortId
                            , noConstructor `with` constructor Mock.aSymbol
                            )
                        ]
                    )
                    (Mock.fSort0 x0 `rewritesTo` Mock.gSort0 x0)
        in assertEqual "" expected actual
    , testCase "Expands multiple arguments." $
        let expected =
                Pair
                    ( Mock.fSort0 (expandableConstructor2 Mock.a Mock.a)
                    , makeEqualsPredicate_
                        x0
                        (expandableConstructor2 Mock.a Mock.a)
                    )
                `rewritesTo`
                Pair
                    ( Mock.gSort0 (expandableConstructor2 Mock.a Mock.a)
                    , makeTruePredicate_
                    )
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSort0Id
                            , noConstructor
                                `with`
                                    ( constructor expandableConstructor2Symbol
                                    `with` Mock.testSort
                                    `with` Mock.testSort
                                    )
                            )
                        ,   ( Mock.testSortId
                            , noConstructor `with` constructor Mock.aSymbol
                            )
                        ]
                    )
                    (Mock.fSort0 x0 `rewritesTo` Mock.gSort0 x0)
        in assertEqual "" expected actual
    , testCase "Expands one of multiple arguments" $
        let expected =
                Pair
                    ( Mock.fSort0 (expandableConstructor2a x00TestSort1 Mock.a)
                    , makeEqualsPredicate_
                        x0
                        (expandableConstructor2a x00TestSort1 Mock.a)
                    )
                `rewritesTo`
                Pair
                    ( Mock.gSort0 (expandableConstructor2a x00TestSort1 Mock.a)
                    , makeTruePredicate_
                    )
            actual =
                expandSingleConstructors
                    (metadataTools
                        [   ( Mock.testSort0Id
                            , noConstructor
                                `with`
                                    ( constructor expandableConstructor2aSymbol
                                    `with` Mock.testSort1
                                    `with` Mock.testSort
                                    )
                            )
                        ,   ( Mock.testSortId
                            , noConstructor `with` constructor Mock.aSymbol
                            )
                        ]
                    )
                    (Mock.fSort0 x0 `rewritesTo` Mock.gSort0 x0)
        in assertEqual "" expected actual
    ]
  where
    x = mkElemVar Mock.x
    x0 = mkElemVar Mock.x0

    x00TestSortVar =
        mkElementVariable (testId "x0") Mock.testSort
        & Lens.set
            (field @"variableName" . Lens.mapped . field @"counter")
            (Just (Element 0))
    x00TestSort = mkElemVar x00TestSortVar

    x00TestSort1Var =
        mkElementVariable (testId "x0") Mock.testSort1
        & Lens.set
            (field @"variableName" . Lens.mapped . field @"counter")
            (Just (Element 0))
    x00TestSort1 = mkElemVar x00TestSort1Var

    metadataTools
        :: [(Id, Attribute.Constructors)]
        -> SmtMetadataTools Attribute.Symbol
    metadataTools sortAndConstructors =
        Mock.metadataTools
            { MetadataTools.sortConstructors = Map.fromList sortAndConstructors
            }

    expandableConstructor1Id :: Id
    expandableConstructor1Id = testId "expandableConstructor1"
    expandableConstructor1Symbol :: Symbol
    expandableConstructor1Symbol =
        Mock.symbol expandableConstructor1Id [Mock.testSort] Mock.testSort0
        & Symbol.functional
        & Symbol.constructor
    expandableConstructor1
        :: HasCallStack
        => TermLike VariableName -> TermLike VariableName
    expandableConstructor1 arg =
        mkApplySymbol expandableConstructor1Symbol [arg]

    expandableConstructor2Id :: Id
    expandableConstructor2Id = testId "expandableConstructor2"
    expandableConstructor2Symbol :: Symbol
    expandableConstructor2Symbol =
        Mock.symbol
            expandableConstructor2Id
            [Mock.testSort, Mock.testSort]
            Mock.testSort0
        & Symbol.functional
        & Symbol.constructor
    expandableConstructor2
        :: HasCallStack
        => TermLike VariableName
        -> TermLike VariableName
        -> TermLike VariableName
    expandableConstructor2 arg1 arg2 =
        mkApplySymbol expandableConstructor2Symbol [arg1, arg2]

    expandableConstructor2aId :: Id
    expandableConstructor2aId = testId "expandableConstructor2a"
    expandableConstructor2aSymbol :: Symbol
    expandableConstructor2aSymbol =
        Mock.symbol
            expandableConstructor2aId
            [Mock.testSort1, Mock.testSort]
            Mock.testSort0
        & Symbol.functional
        & Symbol.constructor
    expandableConstructor2a
        :: HasCallStack
        => TermLike VariableName
        -> TermLike VariableName
        -> TermLike VariableName
    expandableConstructor2a arg1 arg2 =
        mkApplySymbol expandableConstructor2aSymbol [arg1, arg2]

noConstructor :: Attribute.Constructors
noConstructor = Attribute.Constructors Nothing

constructor :: Symbol -> Attribute.ConstructorLike
constructor constructorSymbol =
    Attribute.ConstructorLikeConstructor Attribute.Constructor
        { name = constructorSymbol
        , sorts = []
        }
