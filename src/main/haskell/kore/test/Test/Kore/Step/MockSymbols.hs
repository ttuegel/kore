module Test.Kore.Step.MockSymbols where

{- Intended usage:
   * Import qualified.
   * use symbolOrAliasSortsMapping and attributesMapping to build
     mock SymbolOrAliasSorts and MetadataTools.
   * Use things like a, b, c, x, y, z for testing.

   RULES:
   * Everything that does not obey the default rules must be clearly
     specified in the name, e.g. 'constantNotFunctional'.
   * constant symbols are, by default, functional.
   * constant functions are called cf, cg, ch.
   * constant constructors are called a, b, c, ...
   * one-element functions are called f, g, h.
   * constructors are called "constr<n><k>" where n is the arity and k is used
     to differentiate between them (both are one-digit).
   * functional constructors are called "functionallConstr<n><k>"
   * functional symbols are called "functional<n><k>"
   * symbols without any special attribute are called "plain<n><k>"
   * variables are called x, y, z...
-}

import qualified Data.Map.Strict as Map
import           Data.Reflection
                 ( Given )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import           Kore.AST.Common
                 ( SymbolOrAlias (..), Variable (..) )
import           Kore.AST.MetaOrObject
import           Kore.AST.Pure
                 ( asConcretePurePattern )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.ASTUtils.SmartConstructors
import           Kore.Attribute.Hook
                 ( Hook (..) )
import qualified Kore.Domain.Builtin as Domain
import           Kore.IndexedModule.MetadataTools
                 ( HeadType, SymbolOrAliasSorts )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.Sort
import           Kore.Step.Pattern
import           Kore.Step.StepperAttributes

import           Test.Kore
                 ( testId )
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock

aId :: Id Object
aId = testId "a"
aSort0Id :: Id Object
aSort0Id = testId "aSort0"
aSort1Id :: Id Object
aSort1Id = testId "aSort1"
aSubsortId :: Id Object
aSubsortId = testId "aSubSubsort"
aSubSubsortId :: Id Object
aSubSubsortId = testId "aSubSubsort"
aOtherSortId :: Id Object
aOtherSortId = testId "aOtherSort"
bId :: Id Object
bId = testId "b"
bSort0Id :: Id Object
bSort0Id = testId "bSort0"
cId :: Id Object
cId = testId "c"
dId :: Id Object
dId = testId "d"
eId :: Id Object
eId = testId "e"
fId :: Id Object
fId = testId "f"
gId :: Id Object
gId = testId "g"
hId :: Id Object
hId = testId "h"
cfId :: Id Object
cfId = testId "cf"
cfSort0Id :: Id Object
cfSort0Id = testId "cfSort0"
cfSort1Id :: Id Object
cfSort1Id = testId "cfSort1"
cgId :: Id Object
cgId = testId "cg"
cgSort0Id :: Id Object
cgSort0Id = testId "cgSort0"
chId :: Id Object
chId = testId "ch"
plain00Id :: Id Object
plain00Id = testId "plain00"
plain00Sort0Id :: Id Object
plain00Sort0Id = testId "plain00Sort0"
plain00SubsortId :: Id Object
plain00SubsortId = testId "plain00Subsort"
plain00SubSubsortId :: Id Object
plain00SubSubsortId = testId "plain00SubSubsort"
plain10Id :: Id Object
plain10Id = testId "plain10"
plain11Id :: Id Object
plain11Id = testId "plain11"
plain20Id :: Id Object
plain20Id = testId "plain20"
constr10Id :: Id Object
constr10Id = testId "constr10"
constr11Id :: Id Object
constr11Id = testId "constr11"
constr20Id :: Id Object
constr20Id = testId "constr20"
function20MapTestId :: Id Object
function20MapTestId = testId "function20MapTest"
functional00Id :: Id Object
functional00Id = testId "functional00"
functional01Id :: Id Object
functional01Id = testId "functional01"
functional10Id :: Id Object
functional10Id = testId "functional10"
functional11Id :: Id Object
functional11Id = testId "functional11"
functional20Id :: Id Object
functional20Id = testId "functional20"
functional00SubSubSortId :: Id Object
functional00SubSubSortId = testId "functional00SubSubSort"
functionalConstr10Id :: Id Object
functionalConstr10Id = testId "functionalConstr10"
functionalConstr11Id :: Id Object
functionalConstr11Id = testId "functionalConstr11"
functionalConstr20Id :: Id Object
functionalConstr20Id = testId "functionalConstr20"
functionalTopConstr20Id :: Id Object
functionalTopConstr20Id = testId "functionalTopConstr20"
functionalTopConstr21Id :: Id Object
functionalTopConstr21Id = testId "functionalTopConstr21"
injective10Id :: Id Object
injective10Id = testId "injective10"
injective11Id :: Id Object
injective11Id = testId "injective11"
sortInjectionId :: Id Object
sortInjectionId = testId "sortInjection"
unitMapId :: Id level
unitMapId = testId "unitMap"
elementMapId :: Id level
elementMapId = testId "elementMap"
concatMapId :: Id level
concatMapId = testId "concatMap"

elemListId :: Id level
elemListId = testId "elemList"
concatListId :: Id level
concatListId = testId "concatList"

aSymbol :: SymbolOrAlias Object
aSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = aId
    , symbolOrAliasParams      = []
    }
aSort0Symbol :: SymbolOrAlias Object
aSort0Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = aSort0Id
    , symbolOrAliasParams      = []
    }
aSort1Symbol :: SymbolOrAlias Object
aSort1Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = aSort1Id
    , symbolOrAliasParams      = []
    }
aSubsortSymbol :: SymbolOrAlias Object
aSubsortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = aSubsortId
    , symbolOrAliasParams      = []
    }
aSubSubsortSymbol :: SymbolOrAlias Object
aSubSubsortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = aSubSubsortId
    , symbolOrAliasParams      = []
    }
aOtherSortSymbol :: SymbolOrAlias Object
aOtherSortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = aOtherSortId
    , symbolOrAliasParams      = []
    }
bSymbol :: SymbolOrAlias Object
bSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = bId
    , symbolOrAliasParams      = []
    }
bSort0Symbol :: SymbolOrAlias Object
bSort0Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = bSort0Id
    , symbolOrAliasParams      = []
    }
cSymbol :: SymbolOrAlias Object
cSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = cId
    , symbolOrAliasParams      = []
    }
dSymbol :: SymbolOrAlias Object
dSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = dId
    , symbolOrAliasParams      = []
    }
eSymbol :: SymbolOrAlias Object
eSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = eId
    , symbolOrAliasParams      = []
    }
fSymbol :: SymbolOrAlias Object
fSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = fId
    , symbolOrAliasParams      = []
    }
gSymbol :: SymbolOrAlias Object
gSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = gId
    , symbolOrAliasParams      = []
    }
hSymbol :: SymbolOrAlias Object
hSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = hId
    , symbolOrAliasParams      = []
    }
cfSymbol :: SymbolOrAlias Object
cfSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = cfId
    , symbolOrAliasParams      = []
    }
cfSort0Symbol :: SymbolOrAlias Object
cfSort0Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = cfSort0Id
    , symbolOrAliasParams      = []
    }
cfSort1Symbol :: SymbolOrAlias Object
cfSort1Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = cfSort1Id
    , symbolOrAliasParams      = []
    }
cgSymbol :: SymbolOrAlias Object
cgSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = cgId
    , symbolOrAliasParams      = []
    }
cgSort0Symbol :: SymbolOrAlias Object
cgSort0Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = cgSort0Id
    , symbolOrAliasParams      = []
    }
chSymbol :: SymbolOrAlias Object
chSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = chId
    , symbolOrAliasParams      = []
    }
plain00Symbol :: SymbolOrAlias Object
plain00Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain00Id
    , symbolOrAliasParams      = []
    }
plain00Sort0Symbol :: SymbolOrAlias Object
plain00Sort0Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain00Sort0Id
    , symbolOrAliasParams      = []
    }
plain00SubsortSymbol :: SymbolOrAlias Object
plain00SubsortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain00SubsortId
    , symbolOrAliasParams      = []
    }
plain00SubSubsortSymbol :: SymbolOrAlias Object
plain00SubSubsortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain00SubSubsortId
    , symbolOrAliasParams      = []
    }
plain10Symbol :: SymbolOrAlias Object
plain10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain10Id
    , symbolOrAliasParams      = []
    }
plain11Symbol :: SymbolOrAlias Object
plain11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain11Id
    , symbolOrAliasParams      = []
    }
plain20Symbol :: SymbolOrAlias Object
plain20Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = plain20Id
    , symbolOrAliasParams      = []
    }
constr10Symbol :: SymbolOrAlias Object
constr10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = constr10Id
    , symbolOrAliasParams      = []
    }
constr11Symbol :: SymbolOrAlias Object
constr11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = constr11Id
    , symbolOrAliasParams      = []
    }
constr20Symbol :: SymbolOrAlias Object
constr20Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = constr20Id
    , symbolOrAliasParams      = []
    }
function20MapTestSymbol :: SymbolOrAlias Object
function20MapTestSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = function20MapTestId
    , symbolOrAliasParams      = []
    }
functional00Symbol :: SymbolOrAlias Object
functional00Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional00Id
    , symbolOrAliasParams      = []
    }
functional01Symbol :: SymbolOrAlias Object
functional01Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional01Id
    , symbolOrAliasParams      = []
    }
functional10Symbol :: SymbolOrAlias Object
functional10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional10Id
    , symbolOrAliasParams      = []
    }
functional11Symbol :: SymbolOrAlias Object
functional11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional11Id
    , symbolOrAliasParams      = []
    }
functional20Symbol :: SymbolOrAlias Object
functional20Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional20Id
    , symbolOrAliasParams      = []
    }
functional00SubSubSortSymbol :: SymbolOrAlias Object
functional00SubSubSortSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = functional00SubSubSortId
    , symbolOrAliasParams      = []
    }
functionalConstr10Symbol :: SymbolOrAlias Object
functionalConstr10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functionalConstr10Id
    , symbolOrAliasParams      = []
    }
functionalConstr11Symbol :: SymbolOrAlias Object
functionalConstr11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functionalConstr11Id
    , symbolOrAliasParams      = []
    }
functionalConstr20Symbol :: SymbolOrAlias Object
functionalConstr20Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functionalConstr20Id
    , symbolOrAliasParams      = []
    }
functionalTopConstr20Symbol :: SymbolOrAlias Object
functionalTopConstr20Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functionalTopConstr20Id
    , symbolOrAliasParams      = []
    }
functionalTopConstr21Symbol :: SymbolOrAlias Object
functionalTopConstr21Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = functionalTopConstr21Id
    , symbolOrAliasParams      = []
    }
injective10Symbol :: SymbolOrAlias Object
injective10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = injective10Id
    , symbolOrAliasParams      = []
    }
injective11Symbol :: SymbolOrAlias Object
injective11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = injective11Id
    , symbolOrAliasParams      = []
    }
sortInjection10Symbol :: SymbolOrAlias Object
sortInjection10Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [testSort0, testSort]
    }
sortInjection11Symbol :: SymbolOrAlias Object
sortInjection11Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [testSort1, testSort]
    }
sortInjection0ToTopSymbol :: SymbolOrAlias Object
sortInjection0ToTopSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [testSort0, topSort]
    }
sortInjectionSubToTopSymbol :: SymbolOrAlias Object
sortInjectionSubToTopSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [subSort, topSort]
    }
sortInjectionSubSubToTopSymbol :: SymbolOrAlias Object
sortInjectionSubSubToTopSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [subSubSort, topSort]
    }
sortInjectionSubSubToSubSymbol :: SymbolOrAlias Object
sortInjectionSubSubToSubSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [subSubSort, subSort]
    }
sortInjectionOtherToTopSymbol :: SymbolOrAlias Object
sortInjectionOtherToTopSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = sortInjectionId
    , symbolOrAliasParams      = [otherSort, topSort]
    }
unitMapSymbol :: SymbolOrAlias level
unitMapSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = unitMapId
    , symbolOrAliasParams      = []
    }
elementMapSymbol :: SymbolOrAlias level
elementMapSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = elementMapId
    , symbolOrAliasParams      = []
    }
concatMapSymbol :: SymbolOrAlias level
concatMapSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = concatMapId
    , symbolOrAliasParams      = []
    }

elemListSymbol :: SymbolOrAlias level
elemListSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = elemListId
    , symbolOrAliasParams = []
    }

concatListSymbol :: SymbolOrAlias level
concatListSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = concatListId
    , symbolOrAliasParams = []
    }

var_x_1 :: Variable Object
var_x_1 = Variable (testId "var_x_1") testSort
var_y_1 :: Variable Object
var_y_1 = Variable (testId "var_y_1") testSort
x :: Variable Object
x = Variable (testId "x") testSort
y :: Variable Object
y = Variable (testId "y") testSort
z :: Variable Object
z = Variable (testId "z") testSort
m :: Variable Object
m = Variable (testId "m") mapSort

a   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
a = mkApp aSymbol []

aConcrete :: Given (SymbolOrAliasSorts Object) => ConcreteStepPattern Object
aConcrete = let Just r = asConcretePurePattern a in r

aSort0
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
aSort0 = mkApp aSort0Symbol []

aSort1
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
aSort1 = mkApp aSort1Symbol []

aSubsort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
aSubsort = mkApp aSubsortSymbol []

aSubSubsort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
aSubSubsort = mkApp aSubSubsortSymbol []

aOtherSort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
aOtherSort = mkApp aOtherSortSymbol []

b   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
b = mkApp bSymbol []

bConcrete :: Given (SymbolOrAliasSorts Object) => ConcreteStepPattern Object
bConcrete = let Just r = asConcretePurePattern b in r

bSort0
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
bSort0 = mkApp bSort0Symbol []

c   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
c = mkApp cSymbol []

d   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
d = mkApp dSymbol []

e   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
e = mkApp eSymbol []

f   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable -> StepPattern Object variable
f arg = mkApp fSymbol [arg]

g   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable -> StepPattern Object variable
g arg = mkApp gSymbol [arg]

h   :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable -> StepPattern Object variable
h arg = mkApp hSymbol [arg]

cf  :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
cf = mkApp cfSymbol []

cfSort0
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
cfSort0 = mkApp cfSort0Symbol []

cfSort1
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
cfSort1 = mkApp cfSort1Symbol []

cg  :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
cg = mkApp cgSymbol []

cgSort0
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
cgSort0 = mkApp cgSort0Symbol []

ch  :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
ch = mkApp chSymbol []

plain00
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
plain00 = mkApp plain00Symbol []

plain00Sort0
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
plain00Sort0 = mkApp plain00Sort0Symbol []

plain00Subsort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
plain00Subsort = mkApp plain00SubsortSymbol []

plain00SubSubsort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
plain00SubSubsort = mkApp plain00SubSubsortSymbol []

plain10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable -> StepPattern Object variable
plain10 arg = mkApp plain10Symbol [arg]

plain11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable -> StepPattern Object variable
plain11 arg = mkApp plain11Symbol [arg]

plain20
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
plain20 arg1 arg2 = mkApp plain20Symbol [arg1, arg2]

constr10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
constr10 arg = mkApp constr10Symbol [arg]

constr11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
constr11 arg = mkApp constr11Symbol [arg]

constr20
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
constr20 arg1 arg2 = mkApp constr20Symbol [arg1, arg2]

function20MapTest
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
function20MapTest arg1 arg2 = mkApp function20MapTestSymbol [arg1, arg2]

functional00
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
functional00 = mkApp functional00Symbol []

functional01
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
functional01 = mkApp functional01Symbol []

functional10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
functional10 arg = mkApp functional10Symbol [arg]

functional11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
functional11 arg = mkApp functional11Symbol [arg]

functional20
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
functional20 arg1 arg2 = mkApp functional20Symbol [arg1, arg2]

functional00SubSubSort
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
functional00SubSubSort = mkApp functional00SubSubSortSymbol []

functionalConstr10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
functionalConstr10 arg = mkApp functionalConstr10Symbol [arg]

functionalConstr11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
functionalConstr11 arg = mkApp functionalConstr11Symbol [arg]

functionalConstr20
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
functionalConstr20 arg1 arg2 = mkApp functionalConstr20Symbol [arg1, arg2]

functionalTopConstr20
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
functionalTopConstr20 arg1 arg2 = mkApp functionalTopConstr20Symbol [arg1, arg2]

functionalTopConstr21
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
functionalTopConstr21 arg1 arg2 = mkApp functionalTopConstr21Symbol [arg1, arg2]

injective10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
injective10 arg = mkApp injective10Symbol [arg]

injective11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
injective11 arg = mkApp injective11Symbol [arg]

sortInjection10
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjection10 arg = mkApp sortInjection10Symbol [arg]

sortInjection11
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjection11 arg = mkApp sortInjection11Symbol [arg]

sortInjection0ToTop
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjection0ToTop arg = mkApp sortInjection0ToTopSymbol [arg]

sortInjectionSubToTop
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjectionSubToTop arg = mkApp sortInjectionSubToTopSymbol [arg]

sortInjectionSubSubToTop
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjectionSubSubToTop arg = mkApp sortInjectionSubSubToTopSymbol [arg]

sortInjectionSubSubToSub
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjectionSubSubToSub arg = mkApp sortInjectionSubSubToSubSymbol [arg]

sortInjectionOtherToTop
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
sortInjectionOtherToTop arg = mkApp sortInjectionOtherToTopSymbol [arg]

concatMap
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
concatMap m1 m2 = mkApp concatMapSymbol [m1, m2]

elementMap
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
elementMap m1 m2 = mkApp elementMapSymbol [m1, m2]

concatList
    :: Given (SymbolOrAliasSorts Object)
    => StepPattern Object variable
    -> StepPattern Object variable
    -> StepPattern Object variable
concatList l1 l2 = mkApp concatListSymbol [l1, l2]

symbolOrAliasSortsMapping :: [(SymbolOrAlias Object, ApplicationSorts Object)]
symbolOrAliasSortsMapping =
    [   ( aSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( aSort0Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort0
            }
        )
    ,   ( aSort1Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort1
            }
        )
    ,   ( aSubsortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = subSort
            }
        )
    ,   ( aSubSubsortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = subSubSort
            }
        )
    ,   ( aOtherSortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = otherSort
            }
        )
    ,   ( bSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( bSort0Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort0
            }
        )
    ,   ( cSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( dSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( eSymbol
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
    ,   ( hSymbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( cfSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( cfSort0Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort0
            }
        )
    ,   ( cfSort1Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort1
            }
        )
    ,   ( cgSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( cgSort0Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort0
            }
        )
    ,   ( chSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( plain00Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( plain00Sort0Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort0
            }
        )
    ,   ( plain00SubsortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = subSort
            }
        )
    ,   ( functional00SubSubSortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = subSubSort
            }
        )
    ,   ( plain00SubSubsortSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = subSubSort
            }
        )
    ,   ( plain10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( plain11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( plain20Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( constr10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( constr11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( constr20Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( function20MapTestSymbol
        , ApplicationSorts
            { applicationSortsOperands = [mapSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functional00Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( functional01Symbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = testSort
            }
        )
    ,   ( functional10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functional11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functional20Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functionalConstr10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functionalConstr11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functionalConstr20Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functionalTopConstr20Symbol
        , ApplicationSorts
            { applicationSortsOperands = [topSort, testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( functionalTopConstr21Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, topSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( injective10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( injective11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort]
            , applicationSortsResult = testSort
            }
        )
    ,   ( sortInjection10Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort0]
            , applicationSortsResult = testSort
            }
        )
    ,   ( sortInjection11Symbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort1]
            , applicationSortsResult = testSort
            }
        )
    ,   ( sortInjection0ToTopSymbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort0]
            , applicationSortsResult = topSort
            }
        )
    ,   ( sortInjectionSubToTopSymbol
        , ApplicationSorts
            { applicationSortsOperands = [subSort]
            , applicationSortsResult = topSort
            }
        )
    ,   ( sortInjectionSubSubToTopSymbol
        , ApplicationSorts
            { applicationSortsOperands = [subSubSort]
            , applicationSortsResult = topSort
            }
        )
    ,   ( sortInjectionSubSubToSubSymbol
        , ApplicationSorts
            { applicationSortsOperands = [subSubSort]
            , applicationSortsResult = subSort
            }
        )
    ,   ( sortInjectionOtherToTopSymbol
        , ApplicationSorts
            { applicationSortsOperands = [otherSort]
            , applicationSortsResult = topSort
            }
        )
    ,   ( unitMapSymbol
        , ApplicationSorts
            { applicationSortsOperands = []
            , applicationSortsResult = mapSort
            }
        )
    ,   ( elementMapSymbol
        , ApplicationSorts
            { applicationSortsOperands = [testSort, testSort]
            , applicationSortsResult = mapSort
            }
        )
    ,   ( concatMapSymbol
        , ApplicationSorts
            { applicationSortsOperands = [mapSort, mapSort]
            , applicationSortsResult = mapSort
            }
        )
    ,   ( concatListSymbol
        , ApplicationSorts
            { applicationSortsOperands = [listSort, listSort]
            , applicationSortsResult = listSort
            }
        )
    ]

attributesMapping :: [(SymbolOrAlias Object, StepperAttributes)]
attributesMapping =
    [   ( aSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( aSort0Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( aSort1Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( aSubsortSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( aSubSubsortSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( aOtherSortSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( bSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( bSort0Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( cSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( dSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( eSymbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( fSymbol
        , Mock.functionAttributes
        )
    ,   ( gSymbol
        , Mock.functionAttributes
        )
    ,   ( hSymbol
        , Mock.functionAttributes
        )
    ,   ( cfSymbol
        , Mock.functionAttributes
        )
    ,   ( cfSort0Symbol
        , Mock.functionAttributes
        )
    ,   ( cfSort1Symbol
        , Mock.functionAttributes
        )
    ,   ( cgSymbol
        , Mock.functionAttributes
        )
    ,   ( cgSort0Symbol
        , Mock.functionAttributes
        )
    ,   ( chSymbol
        , Mock.functionAttributes
        )
    ,   ( plain00Symbol
        , Mock.defaultAttributes
        )
    ,   ( plain00Sort0Symbol
        , Mock.defaultAttributes
        )
    ,   ( plain00SubsortSymbol
        , Mock.defaultAttributes
        )
    ,   ( plain00SubSubsortSymbol
        , Mock.defaultAttributes
        )
    ,   ( plain10Symbol
        , Mock.defaultAttributes
        )
    ,   ( plain11Symbol
        , Mock.defaultAttributes
        )
    ,   ( plain20Symbol
        , Mock.defaultAttributes
        )
    ,   ( constr10Symbol
        , Mock.constructorAttributes
        )
    ,   ( constr11Symbol
        , Mock.constructorAttributes
        )
    ,   ( constr20Symbol
        , Mock.constructorAttributes
        )
    ,   ( function20MapTestSymbol
        , Mock.functionAttributes
        )
    ,   ( functional00Symbol
        , Mock.functionalAttributes
        )
    ,   ( functional01Symbol
        , Mock.functionalAttributes
        )
    ,   ( functional10Symbol
        , Mock.functionalAttributes
        )
    ,   ( functional11Symbol
        , Mock.functionalAttributes
        )
    ,   ( functional20Symbol
        , Mock.functionalAttributes
        )
    ,   ( functional00SubSubSortSymbol
        , Mock.functionalAttributes
        )
    ,   ( functionalConstr10Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( functionalConstr11Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( functionalConstr20Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( functionalTopConstr20Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( functionalTopConstr21Symbol
        , Mock.constructorFunctionalAttributes
        )
    ,   ( injective10Symbol
        , Mock.injectiveAttributes
        )
    ,   ( injective11Symbol
        , Mock.injectiveAttributes
        )
    ,   ( sortInjection10Symbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjection11Symbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjection0ToTopSymbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjectionSubToTopSymbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjectionSubSubToTopSymbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjectionSubSubToSubSymbol
        , Mock.sortInjectionAttributes
        )
    ,   ( sortInjectionOtherToTopSymbol
        , Mock.sortInjectionAttributes
        )
    ,   ( unitMapSymbol
        , Mock.functionalAttributes { hook = Hook (Just "MAP.unit") }
        )
    ,   ( elementMapSymbol
        , Mock.functionalAttributes { hook = Hook (Just "MAP.element") }
        )
    ,   ( concatMapSymbol
        , Mock.functionalAttributes { hook = Hook (Just "MAP.concat") }
        )
    ,   ( elemListSymbol
        , Mock.functionalAttributes { hook = Hook (Just "LIST.elem") }
        )
    ,   ( concatListSymbol
        , Mock.functionalAttributes { hook = Hook (Just "LIST.concat") }
        )
    ]

headTypeMapping :: [(SymbolOrAlias Object, HeadType)]
headTypeMapping =
    [   ( aSymbol
        , HeadType.Symbol
        )
    ,   ( aSort0Symbol
        , HeadType.Symbol
        )
    ,   ( aSort1Symbol
        , HeadType.Symbol
        )
    ,   ( aSubsortSymbol
        , HeadType.Symbol
        )
    ,   ( aSubSubsortSymbol
        , HeadType.Symbol
        )
    ,   ( aOtherSortSymbol
        , HeadType.Symbol
        )
    ,   ( bSymbol
        , HeadType.Symbol
        )
    ,   ( bSort0Symbol
        , HeadType.Symbol
        )
    ,   ( cSymbol
        , HeadType.Symbol
        )
    ,   ( dSymbol
        , HeadType.Symbol
        )
    ,   ( eSymbol
        , HeadType.Symbol
        )
    ,   ( fSymbol
        , HeadType.Symbol
        )
    ,   ( gSymbol
        , HeadType.Symbol
        )
    ,   ( hSymbol
        , HeadType.Symbol
        )
    ,   ( cfSymbol
        , HeadType.Symbol
        )
    ,   ( cfSort0Symbol
        , HeadType.Symbol
        )
    ,   ( cfSort1Symbol
        , HeadType.Symbol
        )
    ,   ( cgSymbol
        , HeadType.Symbol
        )
    ,   ( cgSort0Symbol
        , HeadType.Symbol
        )
    ,   ( chSymbol
        , HeadType.Symbol
        )
    ,   ( plain00Symbol
        , HeadType.Symbol
        )
    ,   ( plain00Sort0Symbol
        , HeadType.Symbol
        )
    ,   ( plain00SubsortSymbol
        , HeadType.Symbol
        )
    ,   ( plain00SubSubsortSymbol
        , HeadType.Symbol
        )
    ,   ( plain10Symbol
        , HeadType.Symbol
        )
    ,   ( plain11Symbol
        , HeadType.Symbol
        )
    ,   ( plain20Symbol
        , HeadType.Symbol
        )
    ,   ( constr10Symbol
        , HeadType.Symbol
        )
    ,   ( constr11Symbol
        , HeadType.Symbol
        )
    ,   ( constr20Symbol
        , HeadType.Symbol
        )
    ,   ( function20MapTestSymbol
        , HeadType.Symbol
        )
    ,   ( functional00Symbol
        , HeadType.Symbol
        )
    ,   ( functional01Symbol
        , HeadType.Symbol
        )
    ,   ( functional10Symbol
        , HeadType.Symbol
        )
    ,   ( functional11Symbol
        , HeadType.Symbol
        )
    ,   ( functional20Symbol
        , HeadType.Symbol
        )
    ,   ( functional00SubSubSortSymbol
        , HeadType.Symbol
        )
    ,   ( functionalConstr10Symbol
        , HeadType.Symbol
        )
    ,   ( functionalConstr11Symbol
        , HeadType.Symbol
        )
    ,   ( functionalConstr20Symbol
        , HeadType.Symbol
        )
    ,   ( functionalTopConstr20Symbol
        , HeadType.Symbol
        )
    ,   ( functionalTopConstr21Symbol
        , HeadType.Symbol
        )
    ,   ( injective10Symbol
        , HeadType.Symbol
        )
    ,   ( injective11Symbol
        , HeadType.Symbol
        )
    ,   ( sortInjection10Symbol
        , HeadType.Symbol
        )
    ,   ( sortInjection11Symbol
        , HeadType.Symbol
        )
    ,   ( sortInjection0ToTopSymbol
        , HeadType.Symbol
        )
    ,   ( sortInjectionSubToTopSymbol
        , HeadType.Symbol
        )
    ,   ( sortInjectionSubSubToTopSymbol
        , HeadType.Symbol
        )
    ,   ( sortInjectionSubSubToSubSymbol
        , HeadType.Symbol
        )
    ,   ( sortInjectionOtherToTopSymbol
        , HeadType.Symbol
        )
    ,   ( unitMapSymbol
        , HeadType.Symbol
        )
    ,   ( elementMapSymbol
        , HeadType.Symbol
        )
    ,   ( concatMapSymbol
        , HeadType.Symbol
        )
    ,   ( elemListSymbol
        , HeadType.Symbol
        )
    ,   ( concatListSymbol
        , HeadType.Symbol
        )
    ]

testSort :: Sort Object
testSort =
    SortActualSort SortActual
        { sortActualName  = testId "testSort"
        , sortActualSorts = []
        }

testSort0 :: Sort Object
testSort0 =
    SortActualSort SortActual
        { sortActualName  = testId "testSort0"
        , sortActualSorts = []
        }

testSort1 :: Sort Object
testSort1 =
    SortActualSort SortActual
        { sortActualName  = testId "testSort1"
        , sortActualSorts = []
        }

topSort :: Sort Object
topSort =
    SortActualSort SortActual
        { sortActualName  = testId "topSort"
        , sortActualSorts = []
        }

subSort :: Sort Object
subSort =
    SortActualSort SortActual
        { sortActualName  = testId "subSort"
        , sortActualSorts = []
        }

subSubSort :: Sort Object
subSubSort =
    SortActualSort SortActual
        { sortActualName  = testId "subSubSort"
        , sortActualSorts = []
        }

otherSort :: Sort Object
otherSort =
    SortActualSort SortActual
        { sortActualName = testId "otherSort"
        , sortActualSorts = []
        }

mapSort :: Sort Object
mapSort =
    SortActualSort SortActual
        { sortActualName  = testId "mapSort"
        , sortActualSorts = []
        }

listSort :: Sort Object
listSort =
    SortActualSort SortActual
        { sortActualName  = testId "listSort"
        , sortActualSorts = []
        }

subsorts :: [(Sort Object, Sort Object)]
subsorts =
    [ (subSubSort, subSort)
    , (subSubSort, topSort)
    , (subSort, topSort)
    , (subSubSort, otherSort)
    , (otherSort, topSort)
    ]

builtinMap
    :: [(ConcreteStepPattern Object, StepPattern Object variable)]
    -> StepPattern Object variable
builtinMap = mkDomainValue mapSort . Domain.BuiltinMap . Map.fromList

builtinList
    :: [StepPattern Object variable]
    -> StepPattern Object variable
builtinList = mkDomainValue listSort . Domain.BuiltinList . Seq.fromList

builtinSet
    :: [ConcreteStepPattern Object]
    -> StepPattern Object variable
builtinSet = mkDomainValue listSort . Domain.BuiltinSet . Set.fromList
