module Test.Kore.Step.Simplification.Floor
    ( test_floorSimplification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import Data.Reflection
       ( Given, give )

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.IndexedModule.MetadataTools
                 ( SymbolOrAliasSorts )
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeEqualsPredicate, makeFloorPredicate )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, ExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( bottom, top )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern, OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Floor
                 ( makeEvaluateFloor, simplify )
import qualified Kore.Unification.Substitution as Substitution

import           Test.Kore
                 ( testId )
import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( makeSymbolOrAliasSorts )
import           Test.Tasty.HUnit.Extensions

test_floorSimplification :: [TestTree]
test_floorSimplification =
    [ testCase "Floor - or distribution"
        -- floor(a or b) = (top and floor(a)) or (top and floor(b))
        (assertEqualWithExplanation ""
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkTop testSort
                    , predicate = give mockSymbolOrAliasSorts $
                        makeFloorPredicate (mkOr a b)
                    , substitution = mempty
                    }
                ]
            )
            (give mockSymbolOrAliasSorts $ evaluate
                (makeFloor
                    [aExpanded, bExpanded]
                )
            )
        )
    , testCase "Floor - bool operations"
        (do
            -- floor(top) = top
            assertEqualWithExplanation "floor(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern.top testSort ]
                )
                (give mockSymbolOrAliasSorts $ evaluate
                    (makeFloor
                        [ExpandedPattern.top testSort]
                    )
                )
            -- floor(bottom) = bottom
            assertEqualWithExplanation "floor(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (give mockSymbolOrAliasSorts $ evaluate
                    (makeFloor
                        []
                    )
                )
        )
    , testCase "expanded Floor - bool operations"
        (do
            -- floor(top) = top
            assertEqualWithExplanation "floor(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern.top testSort ]
                )
                (give mockSymbolOrAliasSorts $ makeEvaluate
                    (ExpandedPattern.top testSort)
                )
            -- floor(bottom) = bottom
            assertEqualWithExplanation "floor(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (give mockSymbolOrAliasSorts $ makeEvaluate
                    (ExpandedPattern.bottom testSort)
                )
        )
    , testCase "floor with predicates and substitutions"
        -- floor(term and predicate and subst)
        --     = top and (floor(term) and predicate) and subst
        (assertEqualWithExplanation "floor(top)"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkTop testSort
                    , predicate =
                        give mockSymbolOrAliasSorts $ makeAndPredicate
                            (makeFloorPredicate a)
                            (makeEqualsPredicate fOfA gOfA)
                    , substitution = Substitution.wrap [(x, fOfB)]
                    }
                ]
            )
            (give mockSymbolOrAliasSorts $ makeEvaluate
                Predicated
                    { term = a
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = Substitution.wrap [(x, fOfB)]
                    }
            )
        )
    -- floor moves predicates and substitutions up
    ]
  where
    fId = Id "f" AstLocationTest
    gId = Id "g" AstLocationTest
    aSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "a" AstLocationTest
        , symbolOrAliasParams      = []
        }
    bSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = Id "b" AstLocationTest
        , symbolOrAliasParams      = []
        }
    fSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = fId
        , symbolOrAliasParams      = []
        }
    gSymbol = SymbolOrAlias
        { symbolOrAliasConstructor = gId
        , symbolOrAliasParams      = []
        }
    x = Variable (testId "x") testSort
    a :: CommonStepPattern Object
    a = mkApp testSort aSymbol []
    b :: CommonStepPattern Object
    b = mkApp testSort bSymbol []
    fOfA = mkApp testSort fSymbol [a]
    fOfB = mkApp testSort fSymbol [b]
    gOfA = mkApp testSort gSymbol [a]
    aExpanded = pure a
    bExpanded = pure b
    symbolOrAliasSortsMapping =
        [   ( aSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( bSymbol
            , ApplicationSorts
                { applicationSortsOperands = []
                , applicationSortsResult = testSort
                }
            )
        ,   ( fSymbol
            , ApplicationSorts
                { applicationSortsOperands = [testSort]
                , applicationSortsResult = testSort
                }
            )
        ,   ( gSymbol
            , ApplicationSorts
                { applicationSortsOperands = [testSort]
                , applicationSortsResult = testSort
                }
            )
        ]
    mockSymbolOrAliasSorts = Mock.makeSymbolOrAliasSorts symbolOrAliasSortsMapping

makeFloor
    :: Ord (variable Object)
    => [ExpandedPattern Object variable]
    -> Floor Object (OrOfExpandedPattern Object variable)
makeFloor patterns =
    Floor
        { floorOperandSort = testSort
        , floorResultSort  = testSort
        , floorChild       = OrOfExpandedPattern.make patterns
        }

testSort :: Sort Object
testSort = predicateSort

evaluate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        )
    => Floor level (CommonOrOfExpandedPattern level)
    -> CommonOrOfExpandedPattern level
evaluate floor' =
    case simplify floor' of
        (result, _proof) -> result


makeEvaluate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        )
    => CommonExpandedPattern level
    -> CommonOrOfExpandedPattern level
makeEvaluate child =
    case makeEvaluateFloor child of
        (result, _proof) -> result
