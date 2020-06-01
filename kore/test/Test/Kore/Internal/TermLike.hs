{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Kore.Internal.TermLike
    ( test_substitute
    , test_externalizeFreshVariables
    , test_refreshVariables
    , test_hasConstructorLikeTop
    , test_renaming
    --
    , termLikeGen
    , termLikeChildGen
    ) where

import Prelude.Kore

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty

import qualified Control.Lens as Lens
import Control.Monad.Reader as Reader
import Data.Functor.Identity
    ( runIdentity
    )
import Data.Generics.Product
    ( field
    )
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Sup
import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    , freeVariable
    )
import Kore.Attribute.Synthetic
    ( resynthesize
    )
import Kore.Domain.Builtin
    ( Builtin (..)
    , InternalInt (..)
    )
import Kore.Internal.ApplicationSorts
import Kore.Internal.TermLike
import Kore.Variables.Fresh
    ( refreshElementVariable
    )

import Test.Kore hiding
    ( symbolGen
    )
import Test.Kore.Internal.Symbol
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Tasty.HUnit.Ext
import Test.Terse

type TermLike' = TermLike VariableName
type ElementVariable' = ElementVariable VariableName

termLikeGen :: Hedgehog.Gen (TermLike')
termLikeGen = standaloneGen (termLikeChildGen =<< sortGen)

termLikeChildGen :: Sort -> Gen (TermLike')
termLikeChildGen patternSort =
    Gen.sized termLikeChildGenWorker
  where
    termLikeChildGenWorker n
      | n <= 1 =
        case () of
            ()
              | patternSort == stringMetaSort ->
                mkStringLiteral . getStringLiteral <$> stringLiteralGen
              | otherwise ->
                Gen.choice
                    [ mkElemVar <$> elementVariableGen patternSort
                    , mkBuiltin <$> genBuiltin patternSort
                    ]
      | otherwise =
        (Gen.small . Gen.frequency)
            [ (1, termLikeAndGen)
            , (1, termLikeAppGen)
            , (1, termLikeBottomGen)
            , (1, termLikeCeilGen)
            , (1, termLikeEqualsGen)
            , (1, termLikeExistsGen)
            , (1, termLikeFloorGen)
            , (1, termLikeForallGen)
            , (1, termLikeIffGen)
            , (1, termLikeImpliesGen)
            , (1, termLikeInGen)
            , (1, termLikeNotGen)
            , (1, termLikeOrGen)
            , (1, termLikeTopGen)
            , (5, termLikeVariableGen)
            ]
    termLikeAndGen =
        mkAnd
            <$> termLikeChildGen patternSort
            <*> termLikeChildGen patternSort
    termLikeAppGen = do
        symbol <- symbolGen patternSort
        let childSorts = applicationSortsOperands . symbolSorts $ symbol
        children <- traverse termLikeChildGen childSorts
        pure $ mkApplySymbol symbol children
    termLikeBottomGen = pure (mkBottom patternSort)
    termLikeCeilGen = do
        child <- termLikeChildGen =<< sortGen
        pure (mkCeil patternSort child)
    termLikeEqualsGen = do
        operandSort <- sortGen
        mkEquals patternSort
            <$> termLikeChildGen operandSort
            <*> termLikeChildGen operandSort
    termLikeExistsGen = do
        varSort <- sortGen
        var <- elementVariableGen varSort
        child <-
            Reader.local
                (addVariable (inject var))
                (termLikeChildGen patternSort)
        pure (mkExists var child)
    termLikeForallGen = do
        varSort <- sortGen
        var <- elementVariableGen varSort
        child <-
            Reader.local
                (addVariable (inject var))
                (termLikeChildGen patternSort)
        pure (mkForall var child)
    termLikeFloorGen = do
        child <- termLikeChildGen =<< sortGen
        pure (mkFloor patternSort child)
    termLikeIffGen =
        mkIff
            <$> termLikeChildGen patternSort
            <*> termLikeChildGen patternSort
    termLikeImpliesGen =
        mkImplies
            <$> termLikeChildGen patternSort
            <*> termLikeChildGen patternSort
    termLikeInGen =
        mkIn patternSort
            <$> termLikeChildGen patternSort
            <*> termLikeChildGen patternSort
    termLikeNotGen =
        mkNot <$> termLikeChildGen patternSort
    termLikeOrGen =
        mkOr
            <$> termLikeChildGen patternSort
            <*> termLikeChildGen patternSort
    termLikeTopGen = pure (mkTop patternSort)
    termLikeVariableGen = mkElemVar <$> elementVariableGen patternSort

mkSubst
    :: Injection (SomeVariableName variable) variable'
    => InternalVariable variable
    => Variable variable'
    -> ElementVariable variable
    -> Map (SomeVariableName variable) (TermLike variable)
mkSubst x' y' = Map.singleton (inject $ variableName x') (mkElemVar y')

test_substitute :: [TestTree]
test_substitute =
    [ testCase "Replaces target variable" $ do
        let subst =
                Map.singleton
                    (inject $ variableName Mock.x)
                    (mkElemVar Mock.z)
        assertEqual
            "Expected substituted variable"
            (mkElemVar Mock.z)
            (substitute subst (mkElemVar Mock.x))

    , testCase "Replaces target variable (SetVariable)"
        (assertEqual
            "Expected substituted variable"
            (mkElemVar Mock.z)
            (substitute
                (Map.singleton
                    (variableName $ Mock.makeTestSomeVariable "@x")
                    (mkElemVar Mock.z)
                )
                (Mock.mkTestSomeVariable "@x")
            )
        )

    , testCase "Replaces target variable in subterm (SetVariable)"
        (assertEqual
            "Expected substituted variable"
            (Mock.functionalConstr10 (mkElemVar Mock.z))
            (substitute
                (Map.singleton
                    (variableName $ Mock.makeTestSomeVariable "@x")
                    (mkElemVar Mock.z)
                )
                (Mock.functionalConstr10 (Mock.mkTestSomeVariable "@x"))
            )
        )

    , testCase "Ignores non-target variable" $ do
        let subst =
                Map.singleton
                    (inject $ variableName Mock.x)
                    (mkElemVar Mock.z)
        assertEqual
            "Expected original non-target variable"
            (mkElemVar Mock.y)
            (substitute subst (mkElemVar Mock.y))

    , testGroup "Ignores patterns without children" $
        let ignoring mkPredicate =
                assertEqual
                    "Expected no substitution"
                    expect actual
              where
                expect = mkPredicate Mock.testSort
                actual =
                    substitute
                        (mkSubst Mock.x Mock.z)
                        (mkPredicate Mock.testSort)
        in
            [ testCase "Bottom" (ignoring mkBottom)
            , testCase "Top" (ignoring mkTop)
            ]

    , testGroup "Ignores shadowed variables" $
        let ignoring mkQuantifier =
                assertEqual
                    "Expected shadowed variable to be ignored"
                    expect actual
              where
                expect = mkQuantifier Mock.x (mkElemVar Mock.x)
                actual =
                    substitute
                        (mkSubst Mock.x Mock.z)
                        (mkQuantifier Mock.x (mkElemVar Mock.x))
        in
            [ testCase "Exists" (ignoring mkExists)
            , testCase "Forall" (ignoring mkForall)
            ]

    , testGroup "Renames quantified variables to avoid capture" $
        let renaming mkQuantifier =
                assertEqual
                    "Expected quantified variable to be renamed"
                    expect actual
              where
                expect =
                    mkQuantifier z'
                        $ mkAnd (mkElemVar z') (mkElemVar Mock.z)
                  where
                    Just z' =
                        refreshElementVariable
                            (Set.singleton (inject $ variableName Mock.z))
                            Mock.z
                actual =
                    substitute (mkSubst Mock.x Mock.z)
                    $ mkQuantifier Mock.z
                    $ mkAnd (mkElemVar Mock.z) (mkElemVar Mock.x)
        in
            [ testCase "Exists" (renaming mkExists)
            , testCase "Forall" (renaming mkForall)
            ]
    ]

test_externalizeFreshVariables :: [TestTree]
test_externalizeFreshVariables =
    [ becomes (mkElemVar x_0) (mkElemVar x0) "Append counter"
    , testGroup "No aliasing"
        [ becomes (mk (mkElemVar x0) (mkElemVar x_0)) (mk (mkElemVar x0) (mkElemVar x1)) comment
        | (mk, comment) <- binaryPatterns
        ]
    , testGroup "No capturing - Original free"
        [ becomes (mk x_0 $ mkElemVar x0) (mk x1 $ mkElemVar x0) comment
        | (mk, comment) <- quantifiers
        ]
    , testGroup "No capturing - Generated free"
        [ becomes (mk x0 $ mkElemVar x_0) (mk x00 $ mkElemVar x0) comment
        | (mk, comment) <- quantifiers
        ]
    ]
  where
    binaryPatterns =
        [ (mkAnd, "And")
        , (mkEquals_, "Equals")
        , (mkIff, "Iff")
        , (mkImplies, "Implies")
        , (mkIn_, "In")
        , (mkOr, "Or")
        , (mkRewrites, "Rewrites")
        ]
    quantifiers =
        [ (mkExists, "Exists")
        , (mkForall, "Forall")
        ]
    becomes original expected =
        equals (externalizeFreshVariables original) expected

test_refreshVariables :: [TestTree]
test_refreshVariables =
    [ (Mock.a, [Mock.x]) `becomes` Mock.a
        $ "Does not rename symbols"
    , (xTerm, []) `becomes` xTerm
        $ "No used variable"
    , (xTerm, [Mock.y]) `becomes` xTerm
        $ "No renaming if variable not used"
    , (xTerm, [Mock.x]) `becomes` mkElemVar x_0
        $ "Renames used variable"
    , (Mock.f xTerm, [Mock.x]) `becomes` Mock.f (mkElemVar x_0)
        $ "Renames under symbol"
    ]
  where
    xTerm = mkElemVar Mock.x
    becomes
        :: (TermLike', [ElementVariable'])
        -> TermLike'
        -> TestName
        -> TestTree
    becomes (term, vars) expected =
        equals
            (refreshVariables (foldMap (freeVariable . inject) vars) term)
            expected

test_hasConstructorLikeTop :: [TestTree]
test_hasConstructorLikeTop =
    [ testCase "hasConstructorLikeTop"
        (do
            assertEqual "ApplySymbolF is constructor-like-top"
                True
                $ isConstructorLikeTop (mkApplySymbol Mock.aSymbol [])
            let
                dv :: DomainValue Sort (TermLike')
                dv = DomainValue
                        { domainValueSort = Mock.testSort
                        , domainValueChild = mkStringLiteral "a"
                        }

            assertEqual "DomainValueF is constructor-like-top"
                True
                $ isConstructorLikeTop (mkDomainValue dv)
            let
                b :: Kore.Domain.Builtin.Builtin key child
                b = BuiltinInt
                        (InternalInt
                            { builtinIntSort = Mock.intSort
                            , builtinIntValue = 1
                            }
                        )
            assertEqual "BuiltinF is constructor-like-top"
                True
                (isConstructorLikeTop $ mkBuiltin b)
            assertEqual "StringLiteralF is constructor-like-top"
                True
                (isConstructorLikeTop $ mkStringLiteral "")
            assertEqual "AndF is not is constructor-like-top"
                False
                (isConstructorLikeTop $ mkAnd Mock.a Mock.b)
        )
    ]
  where
    isConstructorLikeTop :: TermLike' -> Bool
    isConstructorLikeTop = hasConstructorLikeTop

x :: ElementVariable'
x = Mock.x

setVariableName :: Lens.Setter' ElementVariable' VariableName
setVariableName = Lens.mapped . Lens.mapped

x_0 :: ElementVariable'
x_0 = Lens.set (setVariableName . field @"counter") (Just (Element 0)) x

x0 :: ElementVariable'
x0 = Lens.set (setVariableName . field @"base") (testId "x0") x

x00 :: ElementVariable'
x00 = Lens.set (setVariableName . field @"base") (testId "x00") x

x1 :: ElementVariable'
x1 = Lens.set (setVariableName . field @"base") (testId "x1") x

test_renaming :: [TestTree]
test_renaming =
    [ testElement "\\exists" mkExists
    , testElement "\\forall" mkForall
    , testSet "\\mu" mkMu
    , testSet "\\nu" mkNu
    ]
  where
    mapElementVariables' Variable { variableName } =
        mapVariables (pure id)
            { adjSomeVariableNameElement = const <$> variableName }
    mapSetVariables' Variable { variableName } =
        mapVariables (pure id)
            { adjSomeVariableNameSet = const <$> variableName }

    traverseElementVariables' Variable { variableName } =
        runIdentity . traverseVariables (pure return)
            { adjSomeVariableNameElement = const . return <$> variableName }
    traverseSetVariables' Variable { variableName } =
        runIdentity . traverseVariables (pure return)
            { adjSomeVariableNameSet = const . return <$> variableName }

    doesNotCapture
        :: HasCallStack
        => SomeVariable VariableName
        -> TermLike'
        -> Assertion
    doesNotCapture Variable { variableName } renamed =
        assertBool
            "does not capture free variables"
            (hasFreeVariable variableName renamed)

    updatesFreeVariables
        :: HasCallStack
        => TermLike'
        -> Assertion
    updatesFreeVariables renamed =
        assertEqual
            "updates the FreeVariables attribute"
            (freeVariables resynthesized :: FreeVariables VariableName)
            (freeVariables       renamed)
      where
        resynthesized :: TermLike'
        resynthesized = resynthesize renamed

    testElement
        :: TestName
        -> (ElementVariable' -> TermLike' -> TermLike')
        -> TestTree
    testElement testName mkBinder =
        testGroup testName
            [ testCase "mapVariables" $ do
                let original = mkBinder Mock.y (mkElemVar Mock.x)
                    renamed = mapElementVariables' Mock.y original
                updatesFreeVariables renamed
                doesNotCapture (inject Mock.y) renamed
            , testCase "traverseVariables" $ do
                let original = mkBinder Mock.y (mkElemVar Mock.x)
                    renamed = traverseElementVariables' Mock.y original
                updatesFreeVariables renamed
                doesNotCapture (inject Mock.y) renamed
            ]

    testSet
        :: TestName
        -> (SetVariable VariableName -> TermLike' -> TermLike')
        -> TestTree
    testSet testName mkBinder =
        testGroup testName
            [ testCase "mapVariables" $ do
                let original = mkBinder Mock.setY (mkSetVar Mock.setX)
                    renamed = mapSetVariables' Mock.setY original
                updatesFreeVariables renamed
                doesNotCapture (inject Mock.setY) renamed
            , testCase "traverseVariables" $ do
                let original = mkBinder Mock.setY (mkSetVar Mock.setX)
                    renamed = traverseSetVariables' Mock.setY original
                updatesFreeVariables renamed
                doesNotCapture (inject Mock.setY) renamed
            ]
