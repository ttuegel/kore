{-|
Module      : Test.Kore.ASTUtils.AlphaCompare
Description : Compare
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}

module Test.Kore.ASTUtils.AlphaCompare
    ( test_alphaEq
    ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Reflection
       ( give )

import           Kore.AST.Pure
import           Kore.ASTUtils.AlphaCompare
import           Kore.ASTUtils.SmartConstructors
import qualified Kore.Domain.Builtin as Domain
import           Kore.IndexedModule.MetadataTools
                 ( SymbolOrAliasSorts )

import           Test.Kore
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
import qualified Test.Kore.Step.MockSymbols as Mock

test_alphaEq :: TestTree
test_alphaEq = testGroup ""
    [ alphaComparePositives
    , alphaCompareNegatives
    , alphaEq1
    , alphaEqMap
    , alphaEqList
    ]

alphaComparePositives :: TestTree
alphaComparePositives =
    QC.testProperty
    "alphaCompare x x == True" $
    forAll (purePatternGen Object) $
    (\(x :: CommonPurePattern Object Domain.Builtin ()) -> alphaEq x x)

alphaCompareNegatives :: TestTree
alphaCompareNegatives =
    QC.testProperty
    "x /y ==> alphaCompare x y == False" $
    forAll pairs $
    (\(x, y) -> (x /= y) ==> not (alphaEq x y))
      where
       pairs = (,) <$> purePatternGen Object <*> purePatternGen Object

alphaEq1 :: TestTree
alphaEq1 =
    give symbolOrAliasSorts $
    QC.testProperty
    "(forall a. a) = (forall b. b)" $
    alphaEq (mkForall v1 (mkVar s v1)) (mkForall v2 (mkVar s v2))

alphaEqList :: TestTree
alphaEqList =
    give symbolOrAliasSorts $
    QC.testProperty
    "forall a. [a, x] = forall b. [b, x]" $
    alphaEq
        (mkForall v1 $ Mock.builtinList [mkVar s v1, mkVar s v3])
        (mkForall v2 $ Mock.builtinList [mkVar s v2, mkVar s v3])


alphaEqMap :: TestTree
alphaEqMap =
    give symbolOrAliasSorts $
    QC.testProperty
    "(forall a. x |-> a) = (forall b. x |-> b)" $
    alphaEq
        (mkForall v1 $ Mock.builtinMap [(mkTop s, mkVar s v1)])
        (mkForall v2 $ Mock.builtinMap [(mkTop s, mkVar s v2)])

s :: Sort Object
s = mkSort "S"
v1, v2, v3 :: Variable Object
v1 = varS "a" s
v2 = varS "b" s
v3 = varS "c" s

symbolOrAliasSorts :: SymbolOrAliasSorts Object
symbolOrAliasSorts = Mock.makeSymbolOrAliasSorts Mock.symbolOrAliasSortsMapping
