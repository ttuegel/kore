{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Test.Kore.ASTUtils
    ( test_substitutions
    , test_sortAgreement
    , var
    , var_
    , mkSort
    , dummyEnvironment
    ) where

import Test.Tasty
       ( TestTree, testGroup )
import Test.Tasty.HUnit
       ( assertEqual, testCase )

import           Control.Lens
import           Data.Reflection
import           Data.Text
                 ( Text )
import qualified Data.Text as Text

import Kore.AST.Pure
import Kore.ASTHelpers
       ( ApplicationSorts (..) )
import Kore.ASTUtils.SmartConstructors
import Kore.ASTUtils.SmartPatterns
import Kore.ASTUtils.Substitution
import Kore.IndexedModule.MetadataTools
import Kore.Step.Pattern

test_substitutions :: TestTree
test_substitutions = testGroup "Substitutions"
    [ testCase "subTrivial" $ assertEqual "" subTrivial subTrivialSolution
    , testCase "subShadow"  $ assertEqual "" subShadow subShadowSolution
    , testCase "subAlphaRename1" $
          assertEqual ""
              subAlphaRename1
              subAlphaRename1Solution
    , testCase "subAlphaRename2" $
          assertEqual ""
              (subAlphaRename2 ^? inPath [0])
              (Just $ mkVar sortS $ var "b")
    , testCase "subTermForTerm" $
          assertEqual ""
              subTermForTerm
              subTermForTermSolution
    ]

test_sortAgreement :: TestTree
test_sortAgreement = testGroup "Sort agreement"
    [ testCase "sortAgreement1" $
        assertEqual ""
            (sortAgreement1 ^? inPath [1])
            (Just $ mkBottom (mkSort "X"))
    , testCase "sortAgreement2.1" $
        assertEqual ""
            (sortAgreement2 ^? inPath [0])
            (Just $ mkBottom (mkSort "Y"))
    , testCase "sortAgreement2.2" $
        assertEqual ""
            (sortAgreement2 ^? (inPath [1] . resultSort ))
            (Just $ mkSort "Y")
    , testCase "predicateSort.1" $
        assertEqual ""
            (dummyEnvironment
                (mkBottom predicateSort :: CommonStepPattern Object)
                ^? resultSort
            )
            (Just (predicateSort :: Sort Object))
    , testCase "predicateSort.2" $
        assertEqual ""
            (dummyEnvironment
                (mkTop predicateSort :: CommonStepPattern Object) ^? resultSort)
            (Just (predicateSort :: Sort Object))
    , testCase "predicateSort.3" $
        assertEqual ""
            (dummyEnvironment
                (mkExists (var_ "a" sortA) (mkBottom predicateSort)
                    :: CommonStepPattern Object) ^? resultSort
            )
            (Just (predicateSort :: Sort Object))
    , testGetSetIdentity 5
    ]

sortA :: MetaOrObject level => Sort level
sortA = mkSort "A"

subTrivial :: CommonStepPattern Object
subTrivial = dummyEnvironment $
    subst (mkCommonVar $ var "a") (mkCommonVar $ var "b") $
    mkExists (var "p") (mkVar sortS $ var "a")

subTrivialSolution :: CommonStepPattern Object
subTrivialSolution = dummyEnvironment $
    mkExists (var "p") (mkCommonVar $ var "b")

subShadow :: CommonStepPattern Object
subShadow = dummyEnvironment $
    subst (mkCommonVar $ var "a") (mkCommonVar $ var "b") $
    mkExists (var "a") (mkCommonVar $ var "q")

subShadowSolution :: CommonStepPattern Object
subShadowSolution = dummyEnvironment $
    mkExists (var "a") (mkCommonVar $ var "q")

subAlphaRename1 :: CommonStepPattern Object
subAlphaRename1 = dummyEnvironment $
    subst (mkCommonVar $ var "a") (mkCommonVar $ var "b") $
    mkExists (var "b") (mkCommonVar $ var "q")

subAlphaRename1Solution :: CommonStepPattern Object
subAlphaRename1Solution = dummyEnvironment $
    mkExists (var "b0") (mkCommonVar $ var "q")

subAlphaRename2 :: CommonStepPattern Object
subAlphaRename2 = dummyEnvironment $
    subst (mkCommonVar $ var "a") (mkCommonVar $ var "b") $
    mkExists (var "b") (mkCommonVar $ var "a")

subTermForTerm :: CommonStepPattern Object
subTermForTerm = dummyEnvironment $
    subst
        (mkOr (mkTop sortS) (mkBottom sortS))
        (mkAnd (mkTop sortS) (mkBottom sortS))
        (mkImplies (mkOr (mkTop sortS) (mkBottom sortS)) (mkTop sortS))

subTermForTermSolution :: CommonStepPattern Object
subTermForTermSolution = dummyEnvironment $
    mkImplies (mkAnd (mkTop sortS) (mkBottom sortS)) (mkTop sortS)

-- subAlphaRename2Solution :: CommonStepPattern Object
-- subAlphaRename2Solution = dummyEnvironment @Object $
--   subst (Var_ $ var "a") (Var_ $ var "b") $
--   mkExists (var "b0") (Var_ $ var "b")

-- the a : X forces bottom : X
sortAgreement1 :: CommonStepPattern Object
sortAgreement1 = dummyEnvironment $
    mkOr (mkCommonVar $ var_ "a" sortX) (mkBottom sortX)

-- the y : Y should force everything else to be Y
sortAgreement2 :: CommonStepPattern Object
sortAgreement2 = dummyEnvironment $
    mkImplies (mkBottom sortY) $
    mkIff
        (mkEquals
            sortY
            (mkCommonVar $ var_ "foo" sortX)
            (mkCommonVar $ var_ "bar" sortX)
        )
        (mkCommonVar $ var_ "y" sortY)

varX :: CommonStepPattern Object
varX = mkCommonVar $ var_ "x" sortX

sortY :: MetaOrObject level => Sort level
sortY = mkSort "Y"

substitutionGetSetIdentity a b pat =
  assertEqual ""
  (subst b a pat)
  (subst b a $ subst a b pat)

generatePatterns
  :: Given (SymbolOrAliasSorts Object)
  => Int
  -> [CommonStepPattern Object]
generatePatterns size = genBinaryPatterns size ++ genUnaryPatterns size
genBinaryPatterns
  :: Given (SymbolOrAliasSorts Object)
  => Int
  -> [CommonStepPattern Object]
genBinaryPatterns 0 = []
genBinaryPatterns size = do
  sa <- [1..size-1]
  let sb = size - sa
  a <- generatePatterns sa
  b <- generatePatterns sb
  [mkAnd a b, mkOr a b, mkImplies a b, mkIff a b, mkRewrites a b]
genUnaryPatterns
  :: Given (SymbolOrAliasSorts Object)
  => Int
  -> [CommonStepPattern Object]
genUnaryPatterns 0 = []
genUnaryPatterns 1 = [mkCommonVar $ var_ "x" sortX]
genUnaryPatterns size = do
  a <- generatePatterns (size - 1)
  [mkNot a, mkNext a, mkForall (var $ Text.pack $ show size) a]

--FIXME: Make a proper Tasty generator instead
testGetSetIdentity
  :: Int
  -> TestTree
testGetSetIdentity size = dummyEnvironment $ testGroup "getSetIdent" $ do
  a <- generatePatterns (size `div` 3)
  b <- generatePatterns (size `div` 3)
  pat <- generatePatterns size
  return $ testCase "" $ substitutionGetSetIdentity a b pat

sortS :: MetaOrObject level => Sort level
sortS = mkSort "S"

sortX :: MetaOrObject level => Sort level
sortX = mkSort "X"

var :: MetaOrObject level => Text -> Variable level
var x = Variable (noLocationId x) sortS

var_ :: MetaOrObject level => Text -> Sort level -> Variable level
var_ x = Variable (noLocationId x)

dummyEnvironment
  :: forall r . MetaOrObject Object
  => (Given (SymbolOrAliasSorts Object) => r)
  -> r
dummyEnvironment = give (dummySymbolOrAliasSorts @Object)

dummySymbolOrAliasSorts :: MetaOrObject level => SymbolOrAliasSorts level
dummySymbolOrAliasSorts = const ApplicationSorts
    { applicationSortsOperands = []
    , applicationSortsResult   = mkSort "S"
    }
