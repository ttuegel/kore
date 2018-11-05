module Test.Kore.Step.Simplification.Exists
    ( test_existsSimplification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import qualified Data.Map as Map
import           Data.Reflection
                 ( Given, give )

import           Kore.AST.Common
                 ( AstLocation (..), Exists (..), Id (..), Sort (..),
                 SortActual (..), Variable )
import           Kore.AST.MetaOrObject
import           Kore.ASTUtils.SmartConstructors
                 ( mkAnd, mkApp, mkEquals, mkExists, mkTop, mkVar )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools, SymbolOrAliasSorts )
import           Kore.Predicate.Predicate
                 ( makeCeilPredicate, makeEqualsPredicate, makeExistsPredicate,
                 makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, ExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( bottom, top )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern, OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.Exists as Exists
                 ( makeEvaluate, simplify )
import qualified Kore.Step.Simplification.Simplifier as Simplifier
                 ( create )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( makeMetadataTools, makeSymbolOrAliasSorts )
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

test_existsSimplification :: [TestTree]
test_existsSimplification = give mockSymbolOrAliasSorts
    [ testCase "Exists - or distribution"
        -- exists(a or b) = exists(a) or exists(b)
        (assertEqualWithExplanation ""
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkExists Mock.x something1OfX
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                , Predicated
                    { term = mkExists Mock.x something2OfX
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                ]
            )
            (evaluate mockMetadataTools
                (makeExists
                    Mock.x
                    [something1OfXExpanded, something2OfXExpanded]
                )
            )
        )
    , testCase "Exists - bool operations"
        (do
            -- exists(top) = top
            assertEqualWithExplanation "exists(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern.top ]
                )
                (evaluate mockMetadataTools
                    (makeExists
                        Mock.x
                        [ExpandedPattern.top]
                    )
                )
            -- exists(bottom) = bottom
            assertEqualWithExplanation "exists(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (evaluate mockMetadataTools
                    (makeExists
                        Mock.x
                        []
                    )
                )
        )
    , testCase "expanded Exists - bool operations"
        (do
            -- exists(top) = top
            assertEqualWithExplanation "exists(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern.top ]
                )
                (makeEvaluate mockMetadataTools
                    Mock.x
                    (ExpandedPattern.top :: CommonExpandedPattern Object)
                )
            -- exists(bottom) = bottom
            assertEqualWithExplanation "exists(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (makeEvaluate mockMetadataTools
                    Mock.x
                    (ExpandedPattern.bottom :: CommonExpandedPattern Object)
                )
        )
    , testCase "exists applies substitution if possible"
        -- exists x . (t(x) and p(x) and [x = alpha, others])
        --    = t(alpha) and p(alpha) and [others]
        (assertEqualWithExplanation "exists with substitution"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkApp Mock.fSymbol [gOfA]
                    , predicate = makeCeilPredicate (mkApp Mock.hSymbol [gOfA])
                    , substitution = [(Mock.y, fOfA)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = mkApp Mock.fSymbol [mkVar Mock.x]
                    , predicate = makeCeilPredicate (Mock.h (mkVar Mock.x))
                    , substitution = [(Mock.x, gOfA), (Mock.y, fOfA)]
                    }
            )
        )
    , testCase "exists disappears if variable not used"
        -- exists x . (t and p and s)
        --    = t and p and s
        --    if t, p, s do not depend on x.
        (assertEqualWithExplanation "exists with substitution"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = fOfA
                    , predicate = makeCeilPredicate gOfA
                    , substitution = []
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = fOfA
                    , predicate = makeCeilPredicate gOfA
                    , substitution = []
                    }
            )
        )
    , testCase "exists applied on term if not used elsewhere"
        -- exists x . (t(x) and p and s)
        --    = (exists x . t(x)) and p and s
        --    if p, s do not depend on x.
        (assertEqualWithExplanation "exists on term"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkExists Mock.x fOfX
                    , predicate = makeCeilPredicate gOfA
                    , substitution = []
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = fOfX
                    , predicate = makeCeilPredicate gOfA
                    , substitution = []
                    }
            )
        )
    , testCase "exists applied on predicate if not used elsewhere"
        -- exists x . (t and p(x) and s)
        --    = t and (exists x . p(x)) and s
        --    if t, s do not depend on x.
        (assertEqualWithExplanation "exists on predicate"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = fOfA
                    , predicate =
                        makeExistsPredicate Mock.x (makeCeilPredicate fOfX)
                    , substitution = []
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = fOfA
                    , predicate = makeCeilPredicate fOfX
                    , substitution = []
                    }
            )
        )
    , testCase "exists moves substitution above"
        -- exists x . (t(x) and p(x) and s)
        --    = exists x . (t(x) and p(x)) and Top and s
        --    if s do not depend on x.
        (assertEqualWithExplanation "exists moves substitution"
            (OrOfExpandedPattern.make
                [ Predicated
                    { term = mkExists Mock.x (mkAnd fOfX (mkEquals fOfX gOfA))
                    , predicate = makeTruePredicate
                    , substitution = [(Mock.y, hOfA)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = fOfX
                    , predicate = makeEqualsPredicate fOfX gOfA
                    , substitution = [(Mock.y, hOfA)]
                    }
            )
        )
    , testCase "exists reevaluates"
        -- exists x . (top and (f(x) = f(g(a)) and [x=g(a)])
        --    = top.s
        (assertEqualWithExplanation "exists reevaluates"
            (OrOfExpandedPattern.make
                [ ExpandedPattern.top ]
            )
            (makeEvaluate mockMetadataTools
                Mock.x
                Predicated
                    { term = mkTop
                    , predicate = makeEqualsPredicate fOfX (Mock.f gOfA)
                    , substitution = [(Mock.x, gOfA)]
                    }
            )
        )
    ]
  where
    fOfA = give mockSymbolOrAliasSorts $ Mock.f Mock.a
    fOfX = give mockSymbolOrAliasSorts $ Mock.f (mkVar Mock.x)
    gOfA = give mockSymbolOrAliasSorts $ Mock.g Mock.a
    hOfA = give mockSymbolOrAliasSorts $ Mock.h Mock.a
    something1OfX = give mockSymbolOrAliasSorts $ Mock.plain10 (mkVar Mock.x)
    something2OfX = give mockSymbolOrAliasSorts $ Mock.plain11 (mkVar Mock.x)
    something1OfXExpanded = Predicated
        { term = something1OfX
        , predicate = makeTruePredicate
        , substitution = []
        }
    something2OfXExpanded = Predicated
        { term = something2OfX
        , predicate = makeTruePredicate
        , substitution = []
        }
    mockSymbolOrAliasSorts =
        Mock.makeSymbolOrAliasSorts Mock.symbolOrAliasSortsMapping
    mockMetadataTools =
        Mock.makeMetadataTools
            mockSymbolOrAliasSorts
            Mock.attributesMapping
            Mock.headTypeMapping
            Mock.subsorts

makeExists
    :: Ord (variable Object)
    => variable Object
    -> [ExpandedPattern Object variable]
    -> Exists Object variable (OrOfExpandedPattern Object variable)
makeExists variable patterns =
    Exists
        { existsSort = testSort
        , existsVariable = variable
        , existsChild = OrOfExpandedPattern.make patterns
        }

testSort :: Sort Object
testSort =
    SortActualSort SortActual
        { sortActualName  = Id "testSort" AstLocationTest
        , sortActualSorts = []
        }

evaluate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        )
    => MetadataTools level StepperAttributes
    -> Exists level Variable (CommonOrOfExpandedPattern level)
    -> CommonOrOfExpandedPattern level
evaluate tools exists =
    fst $ evalSimplifier
        $ Exists.simplify
            tools
            (Mock.substitutionSimplifier tools)
            (Simplifier.create tools Map.empty)
            exists

makeEvaluate
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        )
    => MetadataTools level StepperAttributes
    -> Variable level
    -> CommonExpandedPattern level
    -> CommonOrOfExpandedPattern level
makeEvaluate tools variable child =
    fst $ evalSimplifier
        $ Exists.makeEvaluate
            tools
            (Mock.substitutionSimplifier tools)
            (Simplifier.create tools Map.empty)
            variable
            child
