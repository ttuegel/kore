module Test.Kore.Builtin
    ( test_internalize ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map
import qualified Data.Set
import qualified GHC.Stack as GHC
import           Prelude hiding
                 ( concatMap )

import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin as Kore
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Internal.TermLike

import qualified Test.Kore.Builtin.Builtin as Builtin
import qualified Test.Kore.Builtin.Definition as Builtin
import qualified Test.Kore.Builtin.Int as Int
import qualified Test.Kore.Builtin.List as List
import qualified Test.Kore.Builtin.Map as Map
import qualified Test.Kore.Builtin.Set as Set

test_internalize :: [TestTree]
test_internalize =
    [ internalizes  "unitList"
        unitList
        (mkList [])
    , internalizes  "elementList"
        (elementList x)
        (mkList [x])
    , internalizes  "concatList(internal, internal)"
        (concatList (mkList [x]) (mkList [y]))
        (mkList [x, y])
    , internalizes  "concatList(element, element)"
        (concatList (elementList x) (elementList y))
        (mkList [x, y])
    , internalizes  "concatList(element, unit)"
        (concatList (elementList x) unitList)
        (mkList [x])
    , internalizes  "concatList(unit, element)"
        (concatList unitList (elementList y))
        (mkList [y])

    , internalizes  "unitMap"
        unitMap
        (mkMap [])
    , internalizes  "elementMap"
        (elementMap zero x)
        (mkMap [(zero, x)])
    , internalizes  "concatMap(internal, internal)"
        (concatMap (mkMap [(zero, x)]) (mkMap [(one, y)]))
        (mkMap [(zero, x), (one, y)])
    , internalizes  "concatMap(element, element)"
        (concatMap (elementMap zero x) (elementMap one y))
        (mkMap [(zero, x), (one, y)])
    , internalizes  "concatMap(element, unit)"
        (concatMap (elementMap zero x) unitMap)
        (mkMap [(zero, x)])
    , internalizes  "concatMap(unit, element)"
        (concatMap unitMap (elementMap one y))
        (mkMap [(one, y)])

    , internalizes  "unitSet"
        unitSet
        (mkSet [])
    , internalizes  "elementSet"
        (elementSet zero)
        (mkSet [zero])
    , internalizes  "concatSet(internal, internal)"
        (concatSet (mkSet [zero]) (mkSet [one]))
        (mkSet [zero, one])
    , internalizes  "concatSet(element, element)"
        (concatSet (elementSet zero) (elementSet one))
        (mkSet [zero, one])
    , internalizes  "concatSet(element, unit)"
        (concatSet (elementSet zero) unitSet)
        (mkSet [zero])
    , internalizes  "concatSet(unit, element)"
        (concatSet unitSet (elementSet one))
        (mkSet [one])
    ]
  where
    unitList = Builtin.unitList
    elementList = Builtin.elementList
    concatList = Builtin.concatList
    mkList = List.asInternal

    unitMap = Builtin.unitMap
    elementMap = Builtin.elementMap
    concatMap = Builtin.concatMap
    mkMap = Map.asInternal . Data.Map.fromList

    unitSet = Builtin.unitSet
    elementSet = Builtin.elementSet
    concatSet = Builtin.concatSet
    mkSet = Set.asInternal . Data.Set.fromList

    mkInt :: Ord variable => Integer -> TermLike variable
    mkInt = Int.asInternal
    intSort = Builtin.intSort
    zero, one :: Ord variable => TermLike variable
    zero = mkInt 0
    one = mkInt 1
    x = mkVar (varS "x" intSort)
    y = mkVar (varS "y" intSort)

withInternalized
    :: GHC.HasCallStack
    => (TermLike Variable -> Assertion)
    -> TestName
    -> TermLike Variable
    -> TestTree
withInternalized check name origin =
    testCase name (check $ Kore.internalize metadata origin)

internalizes
    :: GHC.HasCallStack
    => TestName
    -> TermLike Variable
    -> TermLike Variable
    -> TestTree
internalizes name origin expect =
    withInternalized (assertEqual "" expect) name origin

metadata :: SmtMetadataTools Attribute.Symbol
metadata = Builtin.testMetadataTools
