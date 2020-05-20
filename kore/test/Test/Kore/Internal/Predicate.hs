module Test.Kore.Internal.Predicate
    ( test_predicate
    ) where

import Prelude.Kore

import Test.Tasty

import Data.Foldable
    ( traverse_
    )
import qualified Data.Set as Set

import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( toRepresentation
    , top
    )
import qualified Kore.Internal.SideCondition.SideCondition as SideCondition
    ( Representation
    )
import Kore.Internal.TermLike
import qualified Kore.Internal.TermLike as TermLike
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
    )

import Test.Expect
import Test.Kore
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

test_predicate :: [TestTree]
test_predicate =
    [ testCase "And truth table"
        (do
            assertEqual "false and false = false"
                makeFalsePredicate_
                (makeAnd makeFalsePredicate_ makeFalsePredicate_)
            assertEqual "false and true = false"
                makeFalsePredicate_
                (makeAnd makeFalsePredicate_ makeTruePredicate_)
            assertEqual "true and false = false"
                makeFalsePredicate_
                (makeAnd makeTruePredicate_ makeFalsePredicate_)
            assertEqual "true and true = true"
                makeTruePredicate_
                (makeAnd makeTruePredicate_ makeTruePredicate_)
        )
    , let
        makeOr
            :: Predicate Variable
            -> Predicate Variable
            -> Predicate Variable
        makeOr c1 c2 = makeOrPredicate c1 c2
      in
        testCase "Or truth table"
            (do
                assertEqual "false or false = false"
                    makeFalsePredicate_
                    (makeOr makeFalsePredicate_ makeFalsePredicate_)
                assertEqual "false or true = true"
                    makeTruePredicate_
                    (makeOr makeFalsePredicate_ makeTruePredicate_)
                assertEqual "true or false = true"
                    makeTruePredicate_
                    (makeOr makeTruePredicate_ makeFalsePredicate_)
                assertEqual "true or true = true"
                    makeTruePredicate_
                    (makeOr makeTruePredicate_ makeTruePredicate_)
            )
    , let
        makeImplies
            :: Predicate Variable
            -> Predicate Variable
            -> Predicate Variable
        makeImplies c1 c2 = makeImpliesPredicate c1 c2
      in
        testCase "Implies truth table"
            (do
                assertEqual "false implies false = true"
                    makeTruePredicate_
                    (makeImplies makeFalsePredicate_ makeFalsePredicate_)
                assertEqual "false implies true = true"
                    makeTruePredicate_
                    (makeImplies makeFalsePredicate_ makeTruePredicate_)
                assertEqual "true implies false = false"
                    makeFalsePredicate_
                    (makeImplies makeTruePredicate_ makeFalsePredicate_)
                assertEqual "true implies true = true"
                    makeTruePredicate_
                    (makeImplies makeTruePredicate_ makeTruePredicate_)
            )
    , let
        makeIff
            :: Predicate Variable
            -> Predicate Variable
            -> Predicate Variable
        makeIff c1 c2 = makeIffPredicate c1 c2
      in
        testCase "Iff truth table"
            (do
                assertEqual "false iff false = true"
                    makeTruePredicate_
                    (makeIff makeFalsePredicate_ makeFalsePredicate_)
                assertEqual "false iff true = false"
                    makeFalsePredicate_
                    (makeIff makeFalsePredicate_ makeTruePredicate_)
                assertEqual "true iff false = false"
                    makeFalsePredicate_
                    (makeIff makeTruePredicate_ makeFalsePredicate_)
                assertEqual "true iff true = true"
                    makeTruePredicate_
                    (makeIff makeTruePredicate_ makeTruePredicate_)
            )
    , let
        makeNot :: Predicate Variable -> Predicate Variable
        makeNot p = makeNotPredicate p
      in
        testCase "Not truth table"
            (do
                assertEqual "not false = true"
                    makeTruePredicate_
                    (makeNot makeFalsePredicate_)
                assertEqual "not true = false"
                    makeFalsePredicate_
                    (makeNot makeTruePredicate_)
            )
    , testCase "String unwrapping which occurs in test comparisons"
        (assertEqual ""
            "a"
            (stringFromPredicate $ compactPredicatePredicate $
                fmap
                    (\_ ->
                        fmap
                            (const "a")
                            (makeTruePredicate_ :: Predicate Variable)
                    )
                    (makeFalsePredicate_ :: Predicate Variable)
            )
        )
    ,  testCase "Wrapping and predicates without full simplification"
        (do
            assertEqual ""
                (wrapPredicate $
                    mkAnd pa1 pa2
                )
                (makeAndPredicate pr1 pr2)
            assertEqual ""
                (wrapPredicate pa1)
                (makeAndPredicate pr1 makeTruePredicate_)
            assertEqual ""
                (wrapPredicate pa2)
                (makeAndPredicate makeTruePredicate_ pr2)
            assertEqual ""
                makeFalsePredicate_
                (makeAndPredicate pr1 makeFalsePredicate_)
            assertEqual ""
                makeFalsePredicate_
                (makeAndPredicate makeFalsePredicate_ pr2)
            assertEqual ""
                pr1
                (makeAndPredicate pr1 pr1)
        )
    ,  testCase "Wrapping or predicates without full simplification"
        (do
            assertEqual ""
                (wrapPredicate $
                    mkOr pa1 pa2
                )
                (makeOrPredicate pr1 pr2)
            assertEqual ""
                makeTruePredicate_
                (makeOrPredicate pr1 makeTruePredicate_)
            assertEqual ""
                makeTruePredicate_
                (makeOrPredicate makeTruePredicate_ pr2)
            assertEqual ""
                (wrapPredicate pa1)
                (makeOrPredicate pr1 makeFalsePredicate_)
            assertEqual ""
                (wrapPredicate pa2)
                (makeOrPredicate makeFalsePredicate_ pr2)
            assertEqual ""
                pr1
                (makeOrPredicate pr1 pr1)
 )
    ,  testCase "Wrapping and predicates without full simplification"
        (do
            assertEqual ""
                (wrapPredicate $
                    mkImplies pa1 pa2
                )
                (makeImpliesPredicate pr1 pr2)
            assertEqual ""
                makeTruePredicate_
                (makeImpliesPredicate pr1 makeTruePredicate_)
            assertEqual ""
                (wrapPredicate pa2)
                (makeImpliesPredicate makeTruePredicate_ pr2)
            assertEqual ""
                (wrapPredicate $
                    mkNot pa1
                )
                (makeImpliesPredicate pr1 makeFalsePredicate_)
            assertEqual ""
                makeTruePredicate_
                (makeImpliesPredicate makeFalsePredicate_ pr2)
        )
    , testCase "Wrapping iff predicates without full simplification"
        (do
            assertEqual ""
                (wrapPredicate $
                    mkIff pa1 pa2
                )
                (makeIffPredicate pr1 pr2)
            assertEqual ""
                (wrapPredicate pa1)
                (makeIffPredicate pr1 makeTruePredicate_)
            assertEqual ""
                (wrapPredicate pa2)
                (makeIffPredicate makeTruePredicate_ pr2)
            assertEqual ""
                (wrapPredicate $
                    mkNot pa1
                )
                (makeIffPredicate pr1 makeFalsePredicate_)
            assertEqual ""
                (wrapPredicate $
                    mkNot pa2
                )
                (makeIffPredicate makeFalsePredicate_ pr2)
        )
    , testCase "Wrapping not predicates without full simplification"
        (assertEqual ""
            (wrapPredicate $
                mkNot pa1
            )
            (makeNotPredicate pr1)
        )
    , testCase "isFalsePredicate True"
        (assertEqual ""
            True
            (Predicate.isFalse (makeFalsePredicate_::Predicate Variable))
        )
    , testCase "isFalsePredicate False"
        (assertEqual ""
            False
            (Predicate.isFalse (makeTruePredicate_::Predicate Variable))
        )
    , testCase "isFalsePredicate False for generic predicate"
        (assertEqual ""
            False
            (Predicate.isFalse pr1)
        )
    , testCase "Multiple and"
        ( do
            assertEqual "Top is ignored"
                (wrapPredicate $
                    mkAnd pa1 pa2
                )
                (makeMultipleAndPredicate [pr1, makeTruePredicate_, pr2])
            assertEqual "Removes duplicates"
                (wrapPredicate $
                    mkAnd pa1 pa2
                )
                (makeMultipleAndPredicate [pr1, makeTruePredicate_, pr2, pr1])
        )
    , testCase "Multiple Or"
        ( do
            assertEqual "Bottom is ignored"
                (wrapPredicate $
                    mkOr pa1 pa2
                )
                (makeMultipleOrPredicate [pr1, makeFalsePredicate_, pr2])
            assertEqual "Removes duplicates"
                (wrapPredicate $
                    mkOr pa1 pa2
                )
                (makeMultipleOrPredicate [pr1, makeFalsePredicate_, pr2, pr1])
        )
    , testCase "freeVariables"
        ( do
            assertBool "top has no free variables"
                $ FreeVariables.nullFreeVariables @Variable
                $ freeVariables (makeTruePredicate_ :: Predicate Variable)
            assertEqual "equals predicate has two variables"
                (Set.fromList
                    [ ElemVar $ a Mock.testSort
                    , ElemVar $ b Mock.testSort
                    ]
                )
                (freeVariables pr1 & FreeVariables.toSet)
            assertBool "quantified variables are not included"
                $ not . FreeVariables.isFreeVariable (ElemVar $ a Mock.testSort)
                $ freeVariables
                $ makeExistsPredicate (a Mock.testSort)
                $ makeEqualsPredicate_
                    (mkElemVar $ a Mock.testSort)
                    (mkElemVar $ b Mock.testSort)
        )
    , let
        makeExists :: Predicate Variable -> Predicate Variable
        makeExists p = makeExistsPredicate (a Mock.testSort) p
      in
        testCase "Exists truth table"
            (do
                assertEqual "(exists a . true) = true"
                    makeTruePredicate_
                    (makeExists makeTruePredicate_)
                assertEqual "(exists a . false) = false"
                    makeFalsePredicate_
                    (makeExists makeFalsePredicate_)
            )
    , let
        makeForall :: Predicate Variable -> Predicate Variable
        makeForall p = makeForallPredicate (a Mock.testSort) p
      in
        testCase "Forall truth table"
            (do
                assertEqual "(forall a . true) = true"
                    makeTruePredicate_
                    (makeForall makeTruePredicate_)
                assertEqual "(forall a . false) = false"
                    makeFalsePredicate_
                    (makeForall makeFalsePredicate_)
            )
    , testGroup "makePredicate"
        [ testCase "makePredicate yields wrapPredicate"
            (traverse_ (uncurry makePredicateYieldsWrapPredicate)
                [ ("Top", mkTop_)
                , ("Bottom", mkBottom_)
                , ("And", mkAnd pa1 pa2)
                , ("Or", mkOr pa1 pa2)
                , ("Iff", mkIff pa1 pa2)
                , ("Implies", mkImplies pa1 pa2)
                , ("Not", mkNot pa1)
                , ("Exists", mkExists (a Mock.testSort) pa1)
                , ("Forall", mkForall (a Mock.testSort) pa1)
                , ("Equals", pa1)
                , ("Ceil", ceilA)
                , ("Floor", floorA)
                , ("In", inA)
                ]
            )
        , testGroup "keeps simplified bit"
            [ testCase "unsimplified stays unsimplified" $
                (mkEquals_ Mock.cf Mock.cg, NotSimplified)
                `makesPredicate`
                (makeEqualsPredicate_ Mock.cf Mock.cg, NotSimplified)
            , testCase "simplified stays simplified" $
                ( simplifiedTerm $ mkEquals_ Mock.cf Mock.cg
                , IsSimplified
                )
                `makesPredicate`
                (makeEqualsPredicate_ Mock.cf Mock.cg, IsSimplified)
            , testCase "Partial predicate stays simplified" $
                ( simplifiedTerm
                    $ mkAnd mkTop_ (mkEquals_ Mock.cf Mock.cg)
                , IsSimplified
                )
                `makesPredicate`
                (makeEqualsPredicate_ Mock.cf Mock.cg, IsSimplified)
            , testCase "changed simplified becomes unsimplified" $
                ( simplifiedTerm
                    $ mkAnd
                        (mkAnd mkTop_ (mkEquals_ Mock.cf Mock.cg))
                        (mkEquals_ Mock.cg Mock.ch)
                , IsSimplified
                )
                `makesPredicate`
                ( makeAndPredicate
                    (makeEqualsPredicate_ Mock.cf Mock.cg)
                    (makeEqualsPredicate_ Mock.cg Mock.ch)
                , NotSimplified)
            ]
        ]
    ]

data Simplified = IsSimplified | NotSimplified

makesPredicate
    :: HasCallStack
    => (TermLike Variable, Simplified)
    -> (Predicate Variable, Simplified)
    -> IO ()
makesPredicate
    (term, termSimplification)
    (predicate, predicateSimplification)
  = do
    actual <- expectRight (makePredicate term)
    assertEqual "Predicate equality" predicate actual
    assertEqual "Term simplification"
        (toBool termSimplification)
        (TermLike.isSimplified sideRepresentation term)
    assertEqual "Predicate simplification"
        (toBool predicateSimplification)
        (Predicate.isSimplified sideRepresentation actual)
  where
    toBool IsSimplified = True
    toBool NotSimplified = False

makePredicateYieldsWrapPredicate :: String -> TermLike Variable -> IO ()
makePredicateYieldsWrapPredicate msg p = do
    p' <- expectRight (makePredicate p)
    assertEqual msg (wrapPredicate p) p'

pr1 :: Predicate Variable
pr1 =
    makeEqualsPredicate_
        (mkElemVar $ a Mock.testSort)
        (mkElemVar $ b Mock.testSort)

pr2 :: Predicate Variable
pr2 =
    makeEqualsPredicate_
        (mkElemVar $ c Mock.testSort)
        (mkElemVar $ d Mock.testSort)

pa1 :: TermLike Variable
pa1 =
    mkEquals_
        (mkElemVar $ a Mock.testSort)
        (mkElemVar $ b Mock.testSort)

pa2 :: TermLike Variable
pa2 =
    mkEquals_
        (mkElemVar $ c Mock.testSort)
        (mkElemVar $ d Mock.testSort)

ceilA :: TermLike Variable
ceilA =
    mkCeil_
        (mkElemVar $ a Mock.testSort)

inA :: TermLike Variable
inA =
    mkIn_
        (mkElemVar $ a Mock.testSort)
        (mkElemVar $ b Mock.testSort)

floorA :: TermLike Variable
floorA = mkFloor_ (mkElemVar $ a Mock.testSort)

makeAnd
    :: Predicate Variable
    -> Predicate Variable
    -> Predicate Variable
makeAnd p1 p2 = makeAndPredicate p1 p2

a, b, c, d :: Sort -> ElementVariable Variable
a = ElementVariable . Variable (testId "a") mempty
b = ElementVariable . Variable (testId "b") mempty
c = ElementVariable . Variable (testId "c") mempty
d = ElementVariable . Variable (testId "d") mempty

sideRepresentation :: SideCondition.Representation
sideRepresentation =
    SideCondition.toRepresentation (SideCondition.top :: SideCondition Variable)
