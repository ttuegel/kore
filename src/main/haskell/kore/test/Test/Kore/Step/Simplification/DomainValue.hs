module Test.Kore.Step.Simplification.DomainValue
    ( test_domainValueSimplification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import Data.Reflection
       ( give )

import           Kore.AST.Common
                 ( BuiltinDomain (..), DomainValue (..), Sort (..) )
import           Kore.AST.MetaOrObject
import           Kore.ASTUtils.SmartConstructors
                 ( mkBottom, mkDomainValue, mkStringLiteral )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern Bottom_ )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools, SortTools )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern (ExpandedPattern) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.DomainValue
                 ( simplify )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
import           Test.Tasty.HUnit.Extensions

test_domainValueSimplification :: [TestTree]
test_domainValueSimplification =
    [ testCase "DomainValue evaluates to DomainValue"
        (assertEqualWithExplanation ""
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term =
                        give mockSortTools $
                            mkDomainValue
                                testSort
                                (BuiltinDomainPattern (mkStringLiteral "a"))
                    , predicate = makeTruePredicate
                    , substitution = []
                    }
                ]
            )
            (evaluate
                mockMetadataTools
                (DomainValue
                    testSort
                    (BuiltinDomainPattern (mkStringLiteral "a"))
                )
            )
        )
    ]
  where

testSort :: Sort Object
testSort =
    case mkBottom of
        Bottom_ sort -> sort
        _ -> error "unexpected"

mockSortTools :: SortTools Object
mockSortTools = Mock.makeSortTools []

mockMetadataTools :: MetadataTools Object StepperAttributes
mockMetadataTools = Mock.makeMetadataTools mockSortTools [] []

evaluate
    :: (MetaOrObject Object)
    => MetadataTools Object attrs
    -> DomainValue Object (BuiltinDomain (CommonOrOfExpandedPattern Object))
    -> CommonOrOfExpandedPattern Object
evaluate tools domainValue =
    case simplify tools domainValue of
        (result, _proof) -> result
