module Test.Kore.Step.Simplification.Floor
    ( test_floorSimplification
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Data.Default as Default

import qualified Kore.Internal.Condition as Condition
    ( top
    )
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Conditional (..)
    , Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
    ( bottom
    , fromConditionSorted
    , top
    )
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeEqualsPredicate_
    , makeFloorPredicate_
    , makeTruePredicate_
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.Symbol
import Kore.Internal.TermLike
import Kore.Step.Simplification.Floor
    ( makeEvaluateFloor
    , simplify
    )

import Test.Kore
    ( testId
    )
import Test.Kore.Step.MockSymbols
    ( testSort
    )
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

test_floorSimplification :: [TestTree]
test_floorSimplification =
    [ testCase "Floor - or distribution"
        -- floor(a or b) = (top and floor(a)) or (top and floor(b))
        (assertEqual ""
            (OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate = makeFloorPredicate_ (mkOr a b)
                    , substitution = mempty
                    }
                ]
            )
            (evaluate
                (makeFloor
                    [aExpanded, bExpanded]
                )
            )
        )
    , testCase "Floor - bool operations"
        (do
            -- floor(top) = top
            assertEqual "floor(top)"
                (OrPattern.fromPatterns
                    [ Pattern.top ]
                )
                (evaluate
                    (makeFloor
                        [Pattern.top]
                    )
                )
            -- floor(bottom) = bottom
            assertEqual "floor(bottom)"
                (OrPattern.fromPatterns
                    []
                )
                (evaluate
                    (makeFloor
                        []
                    )
                )
        )
    , testCase "Floor - bool operations" $
        -- floor(top{testSort}) = top
        assertEqual "floor(top)"
            (OrPattern.fromPatterns
                [ Pattern.top ]
            )
            (evaluate
                (makeFloor
                    [ Pattern.fromConditionSorted testSort Condition.top ]
                )
            )
    , testCase "expanded Floor - bool operations"
        (do
            -- floor(top) = top
            assertEqual "floor(top)"
                (OrPattern.fromPatterns
                    [ Pattern.top ]
                )
                (makeEvaluate
                    (Pattern.top :: Pattern VariableName)
                )
            -- floor(bottom) = bottom
            assertEqual "floor(bottom)"
                (OrPattern.fromPatterns
                    []
                )
                (makeEvaluate
                    (Pattern.bottom :: Pattern VariableName)
                )
        )
    , testCase "floor with predicates and substitutions"
        -- floor(term and predicate and subst)
        --     = top and (floor(term) and predicate) and subst
        (assertEqual "floor(top)"
            (OrPattern.fromPatterns
                [ Conditional
                    { term = mkTop_
                    , predicate =
                        makeAndPredicate
                            (makeFloorPredicate_ a)
                            (makeEqualsPredicate_ fOfA gOfA)
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject x, fOfB)]
                    }
                ]
            )
            (makeEvaluate
                Conditional
                    { term = a
                    , predicate = makeEqualsPredicate_ fOfA gOfA
                    , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject x, fOfB)]
                    }
            )
        )
    -- floor moves predicates and substitutions up
    ]
  where
    symbol name operands result =
        Symbol
            { symbolConstructor = testId name
            , symbolParams = []
            , symbolAttributes = Default.def
            , symbolSorts = applicationSorts operands result
            }
    aSymbol = symbol "a" [] testSort
    bSymbol = symbol "b" [] testSort
    fSymbol = symbol "f" [testSort] testSort
    gSymbol = symbol "g" [testSort] testSort
    x = mkElementVariable (testId "x") testSort
    a :: TermLike VariableName
    a = mkApplySymbol aSymbol []
    b :: TermLike VariableName
    b = mkApplySymbol bSymbol []
    fOfA = mkApplySymbol fSymbol [a]
    fOfB = mkApplySymbol fSymbol [b]
    gOfA = mkApplySymbol gSymbol [a]
    aExpanded = Conditional
        { term = a
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    bExpanded = Conditional
        { term = b
        , predicate = makeTruePredicate_
        , substitution = mempty
        }

makeFloor
    :: InternalVariable variable
    => [Pattern variable]
    -> Floor Sort (OrPattern variable)
makeFloor patterns =
    Floor
        { floorOperandSort = testSort
        , floorResultSort  = testSort
        , floorChild       = OrPattern.fromPatterns patterns
        }

evaluate :: Floor Sort (OrPattern VariableName) -> OrPattern VariableName
evaluate = simplify . fmap simplifiedOrPattern

makeEvaluate :: Pattern VariableName -> OrPattern VariableName
makeEvaluate = makeEvaluateFloor . simplifiedPattern
