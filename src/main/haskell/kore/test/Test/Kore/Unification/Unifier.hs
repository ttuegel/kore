module Test.Kore.Unification.Unifier (test_unification) where

import Test.Tasty
       ( TestTree, testGroup )
import Test.Tasty.HUnit
       ( testCase )
import Test.Tasty.HUnit.Extensions

import           Data.CallStack
import           Data.Default
                 ( def )
import           Data.Function
                 ( on )
import           Data.Functor.Foldable
import           Data.List
                 ( sortBy )
import qualified Data.Map as Map
import           Data.Proxy
                 ( Proxy (..) )
import           Data.Reflection
                 ( give )

import           Kore.AST.Builders
import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.AST.MLPatterns
import           Kore.AST.PureML
import           Kore.AST.Sentence
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.ASTPrettyPrint
import           Kore.ASTUtils.SmartConstructors
                 ( mkVar )
import           Kore.IndexedModule.MetadataTools
import           Kore.Predicate.Predicate
                 ( Predicate, makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.PatternAttributes
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.Simplification.Simplifier as Simplifier
import           Kore.Step.StepperAttributes
import           Kore.Unification.Error
import           Kore.Unification.UnifierImpl

import Test.Kore
import Test.Kore.AST.MLPatterns
       ( extractPurePattern )
import Test.Kore.ASTVerifier.DefinitionVerifier
import Test.Kore.Comparators ()

applyInj
    :: Sort Object
    -> Sort Object
    -> CommonPurePatternStub Object -> CommonPurePatternStub Object
applyInj sortFrom sortTo pat = applyPS symbolInj [sortFrom, sortTo] [pat]

s1, s2, s3, s4 :: Sort Object
s1 = simpleSort (SortName "s1")
s2 = simpleSort (SortName "s2")
s3 = simpleSort (SortName "s3")
s4 = simpleSort (SortName "s4")

a, a1, a2, a3, b, c, f, g, h :: PureSentenceSymbol Object
a = symbol_ "a" AstLocationTest [] s1
a1 = symbol_ "a1" AstLocationTest [] s1
a2 = symbol_ "a2" AstLocationTest [] s1
a3 = symbol_ "a3" AstLocationTest [] s1
b = symbol_ "b" AstLocationTest [] s2
c = symbol_ "c" AstLocationTest [] s3
f = symbol_ "f" AstLocationTest [s1] s2
g = symbol_ "g" AstLocationTest [s1, s2] s3
h = symbol_ "h" AstLocationTest [s1, s2, s3] s1

ef, eg, eh :: PureSentenceSymbol Object
ef = symbol_ "ef" AstLocationTest [s1, s1, s1] s1
eg = symbol_ "eg" AstLocationTest [s1] s1
eh = symbol_ "eh" AstLocationTest [s1] s1

nonLinF, nonLinG, nonLinAS :: PureSentenceSymbol Object
nonLinF = symbol_ "nonLinF" AstLocationTest [s1, s1] s1
nonLinG = symbol_ "nonLinG" AstLocationTest [s1] s1
nonLinAS = symbol_ "nonLinA" AstLocationTest [] s1

nonLinA, nonLinX, nonLinY :: CommonPurePatternStub Object
nonLinX = parameterizedVariable_ s1 "x" AstLocationTest
nonLinY = parameterizedVariable_ s1 "y" AstLocationTest

nonLinA = applyS nonLinAS []

expBin :: PureSentenceSymbol Object
expBin = symbol_ "times" AstLocationTest [s1, s1] s1

expA, expX, expY :: CommonPurePatternStub Object
expA = parameterizedVariable_ s1 "a" AstLocationTest
expX = parameterizedVariable_ s1 "x" AstLocationTest
expY = parameterizedVariable_ s1 "y" AstLocationTest

ex1, ex2, ex3, ex4 :: CommonPurePatternStub Object
ex1 = parameterizedVariable_ s1 "ex1" AstLocationTest
ex2 = parameterizedVariable_ s1 "ex2" AstLocationTest
ex3 = parameterizedVariable_ s1 "ex3" AstLocationTest
ex4 = parameterizedVariable_ s1 "ex4" AstLocationTest


dv1, dv2 :: CommonPurePatternStub Object
dv1 = parameterizedDomainValue_ s1 "dv1"
dv2 = parameterizedDomainValue_ s1 "dv2"

aA :: CommonPurePatternStub Object
aA = applyS a []

a1A :: CommonPurePatternStub Object
a1A = applyS a1 []

a2A :: CommonPurePatternStub Object
a2A = applyS a2 []

a3A :: CommonPurePatternStub Object
a3A = applyS a3 []

bA :: CommonPurePatternStub Object
bA = applyS b []

x :: CommonPurePatternStub Object
x = parameterizedVariable_ s1 "x" AstLocationTest

xs2 :: CommonPurePatternStub Object
xs2 = parameterizedVariable_ s2 "xs2" AstLocationTest

symbols :: [(SymbolOrAlias Object, PureSentenceSymbol Object)]
symbols =
    map
        (\s -> (getSentenceSymbolOrAliasHead s [], s))
        [ a, a1, a2, a3, b, c, f, g, h
        , ef, eg, eh
        , nonLinF, nonLinG, nonLinAS
        , expBin
        ]

sortParam :: String -> SortVariable level
sortParam name = sortParameter Proxy name AstLocationTest

sortParamSort :: String -> Sort level
sortParamSort = SortVariableSort . sortParam

injName :: String
injName = "inj"

symbolInj :: PureSentenceSymbol level
symbolInj =
    parameterizedSymbol_ injName AstLocationImplicit
        [sortParam "From", sortParam "To"]
        [sortParamSort "From"]
        (sortParamSort "To")

injHead :: Sort level -> Sort level -> SymbolOrAlias level
injHead sortFrom sortTo =
    getSentenceSymbolOrAliasHead symbolInj [sortFrom, sortTo]

isInjHead :: SymbolOrAlias level -> Bool
isInjHead pHead = getId (symbolOrAliasConstructor pHead) == injName

mockStepperAttributes :: SymbolOrAlias Object -> StepperAttributes
mockStepperAttributes patternHead = StepperAttributes
    { isConstructor =
        patternHead /= getSentenceSymbolOrAliasHead a2 []
        && not (isInjHead patternHead)
    , isFunctional = patternHead /= getSentenceSymbolOrAliasHead a3 []
    , isFunction = False
    , isInjective =
        patternHead /= getSentenceSymbolOrAliasHead a2 []
        || isInjHead patternHead
    , isSortInjection = isInjHead patternHead
    , hook = def
    }

mockGetArgumentSorts :: SymbolOrAlias Object -> [Sort Object]
mockGetArgumentSorts patternHead
    | isInjHead patternHead = init (symbolOrAliasParams patternHead)
    | otherwise =
        maybe
            (error ("Unexpected Head " ++  show patternHead))
            getSentenceSymbolOrAliasArgumentSorts
            (lookup patternHead symbols)

mockGetResultSort :: SymbolOrAlias Object -> Sort Object
mockGetResultSort patternHead
    | isInjHead patternHead = last (symbolOrAliasParams patternHead)
    | otherwise =
        maybe
            (error ("Unexpected Head " ++  show patternHead))
            getSentenceSymbolOrAliasResultSort
            (lookup patternHead symbols)

mockSortTools :: SortTools Object
mockSortTools pHead = ApplicationSorts
    { applicationSortsOperands = mockGetArgumentSorts pHead
    , applicationSortsResult = mockGetResultSort pHead
    }

tools :: MetadataTools Object StepperAttributes
tools = MetadataTools
    { symAttributes = mockStepperAttributes
    , sortAttributes = undefined
    , sortTools = mockSortTools
    }

unificationProblem
    :: MetaOrObject level
    => UnificationTerm level
    -> UnificationTerm level
    -> CommonPurePattern level
unificationProblem (UnificationTerm term1) (UnificationTerm term2) =
    extractPurePattern (and_ term1 term2)

type Substitution level =  [(String, CommonPurePatternStub level)]

unificationSubstitution
    :: Substitution Object
    -> [ (Variable Object, CommonPurePattern Object) ]
unificationSubstitution = map trans
  where
    trans (v, p) =
        let pp = extractPurePattern p in
            ( Variable
                { variableSort =
                    getPatternResultSort mockSortTools (project pp)
                , variableName = testId v
                }
            , pp
            )

unificationResult
    :: UnificationResultTerm Object
    -> Substitution Object
    -> UnificationSolution Object Variable
unificationResult (UnificationResultTerm pat) sub = UnificationSolution
    { unificationSolutionTerm = extractPurePattern pat
    , unificationSolutionConstraints = unificationSubstitution sub
    }

newtype UnificationTerm level =
    UnificationTerm (CommonPurePatternStub level)
newtype UnificationResultTerm level =
    UnificationResultTerm (CommonPurePatternStub level)

andSimplifySuccess
    :: (HasCallStack)
    => String
    -> UnificationTerm Object
    -> UnificationTerm Object
    -> UnificationResultTerm Object
    -> Substitution Object
    -> UnificationProof Object Variable
    -> TestTree
andSimplifySuccess message term1 term2 resultTerm subst proof =
    testCase
        message
        (assertEqualWithExplanation
            ""
            (unificationResult resultTerm subst, proof)
            (subst'', proof')
        )
  where
    Right (subst', proof') = simplifyAnd tools (unificationProblem term1 term2)
    subst'' = subst'
        { unificationSolutionConstraints =
            sortBy (compare `on` fst) (unificationSolutionConstraints subst')
        }


andSimplifyFailure
    :: (HasCallStack)
    => String
    -> UnificationTerm Object
    -> UnificationTerm Object
    -> UnificationError Object
    -> TestTree
andSimplifyFailure message term1 term2 err =
    testCase
        message
        (assertEqualWithPrinter
            prettyPrintToString
            ""
            (Left err)
            (simplifyAnd tools (unificationProblem term1 term2))
        )

unificationProcedureSuccess
    :: (HasCallStack)
    => String
    -> UnificationTerm Object
    -> UnificationTerm Object
    -> Substitution Object
    -> Predicate Object Variable
    -> UnificationProof Object Variable
    -> TestTree
unificationProcedureSuccess
    message
    (UnificationTerm term1)
    (UnificationTerm term2)
    subst
    predicate'
    proof
  = testCase
        message
        (assertEqualWithExplanation
            ""
            (unificationSubstitution subst, predicate', proof)
            (sortBy (compare `on` fst) subst', pred', proof')
        )
  where
    Right (subst', pred', proof') =
        give tools
            ( unificationProcedure
                tools
                (extractPurePattern term1)
                (extractPurePattern term2)
            )

test_unification :: [TestTree]
test_unification =
    [ andSimplifySuccess "Constant"
        (UnificationTerm aA)
        (UnificationTerm aA)
        (UnificationResultTerm aA)
        []
        (ConjunctionIdempotency (extractPurePattern aA))
    , andSimplifySuccess "Variable"
        (UnificationTerm x)
        (UnificationTerm aA)
        (UnificationResultTerm aA)
        [("x", aA)]
        (Proposition_5_24_3
            [FunctionalHead (symbolHead a)]
            (var x)
            (extractPurePattern aA)
        )
    , andSimplifySuccess "one level"
        (UnificationTerm (applyS f [x]))
        (UnificationTerm (applyS f [aA]))
        (UnificationResultTerm (applyS f [aA]))
        [("x", aA)]
        (AndDistributionAndConstraintLifting
            (getSentenceSymbolOrAliasHead f [])
            [ Proposition_5_24_3
                [FunctionalHead (symbolHead a)]
                (var x)
                (extractPurePattern aA)
            ]
        )
    , andSimplifySuccess "equal non-constructor patterns"
        (UnificationTerm a2A)
        (UnificationTerm a2A)
        (UnificationResultTerm a2A)
        []
        (ConjunctionIdempotency (extractPurePattern a2A))
    , andSimplifySuccess "variable + non-constructor pattern"
        (UnificationTerm a2A)
        (UnificationTerm x)
        (UnificationResultTerm a2A)
        [("x", a2A)]
        (Proposition_5_24_3
            [FunctionalHead (symbolHead a2)]
            (var x)
            (extractPurePattern a2A)
        )
    , andSimplifySuccess
        "https://basics.sjtu.edu.cn/seminars/c_chu/Algorithm.pdf slide 3"
        (UnificationTerm (applyS ef [ex1, applyS eh [ex1], ex2]))
        (UnificationTerm (applyS ef [applyS eg [ex3], ex4, ex3]))
        (UnificationResultTerm
            (applyS ef [applyS eg [ex3], applyS eh [ex1], ex3])
        )
        [ ("ex1", applyS eg [ex3])
        , ("ex2", ex3)
        , ("ex4", applyS eh [ex1])
        ]
        (AndDistributionAndConstraintLifting
            (getSentenceSymbolOrAliasHead ef [])
            [ Proposition_5_24_3
                [ FunctionalHead (symbolHead eg)
                , FunctionalVariable (var ex3)
                ]
                (var ex1)
                (extractPurePattern $ applyS eg [ex3])
            , Proposition_5_24_3
                [ FunctionalHead (symbolHead eh)
                , FunctionalVariable (var ex1)
                ]
                (var ex4)
                (extractPurePattern $ applyS eh [ex1])
            , Proposition_5_24_3
                [FunctionalVariable (var ex3)]
                (var ex2)
                (extractPurePattern ex3)
            ]
        )
    , andSimplifySuccess
        "f(g(X),X) = f(Y,a) https://en.wikipedia.org/wiki/Unification_(computer_science)#Examples_of_syntactic_unification_of_first-order_terms"
        (UnificationTerm
            (applyS nonLinF [applyS nonLinG [nonLinX], nonLinX])
        )
        (UnificationTerm (applyS nonLinF [nonLinY, nonLinA]))
        (UnificationResultTerm
            (applyS nonLinF [applyS nonLinG [nonLinX], nonLinA])
        )
        [ ("x", nonLinA), ("y", applyS nonLinG [nonLinX])]
        (AndDistributionAndConstraintLifting
            (symbolHead nonLinF)
            [ Proposition_5_24_3
                [ FunctionalHead (symbolHead nonLinG)
                , FunctionalVariable (var nonLinX)
                ]
                (var nonLinY)
                (extractPurePattern (applyS nonLinG [nonLinX]))
            , Proposition_5_24_3
                [ FunctionalHead (symbolHead nonLinAS) ]
                (var nonLinX)
                (extractPurePattern nonLinA)
            ]
        )
    , andSimplifySuccess
        "times(times(a, y), x) = times(x, times(y, a))"
        (UnificationTerm (applyS expBin [applyS expBin [expA, expY], expX]))
        (UnificationTerm (applyS expBin [expX, applyS expBin [expY, expA]]))
        (UnificationResultTerm (applyS
            expBin
            [ applyS expBin [expA, expY]
            , applyS expBin [expY, expA]
            ]
        ))
        [ ("x", applyS expBin [expA, expY])
        , ("x", applyS expBin [expY, expA])
        ]
        (AndDistributionAndConstraintLifting
            (symbolHead expBin)
            [ Proposition_5_24_3
                [ FunctionalHead (symbolHead expBin)
                , FunctionalVariable (var expA)
                , FunctionalVariable (var expY)
                ]
                (var expX)
                (extractPurePattern (applyS expBin [expA, expY]))
            , Proposition_5_24_3
                [ FunctionalHead (symbolHead expBin)
                , FunctionalVariable (var expY)
                , FunctionalVariable (var expA)
                ]
                (var expX)
                (extractPurePattern (applyS expBin [expY, expA]))
            ]
        )
    , unificationProcedureSuccess
        "times(x, g(x)) = times(a, a) -- cycles are resolved elsewhere"
        (UnificationTerm (applyS expBin [expX, applyS eg [expX]]))
        (UnificationTerm (applyS expBin [expA, expA]))
        [ ("a", applyS eg [expX])
        , ("x", applyS eg [expX])
        ]
        makeTruePredicate
        (CombinedUnificationProof
            [ AndDistributionAndConstraintLifting
                (symbolHead expBin)
                [ Proposition_5_24_3
                    [ FunctionalVariable (var expA)
                    ]
                    (var expX)
                    (extractPurePattern expA)
                , Proposition_5_24_3
                    [ FunctionalHead (symbolHead eg)
                    , FunctionalVariable (var expX)
                    ]
                    (var expA)
                    (extractPurePattern (applyS eg [expX]))
                ]
            , Proposition_5_24_3
                [ FunctionalHead (symbolHead eg)
                , FunctionalVariable (var expX)
                ]
                (var expX)
                (extractPurePattern (applyS eg [expX]))
            ]
        )
    , unificationProcedureSuccess
        "times(times(a, y), x) = times(x, times(y, a))"
        (UnificationTerm (applyS expBin [applyS expBin [expA, expY], expX]))
        (UnificationTerm (applyS expBin [expX, applyS expBin [expY, expA]]))
        [ ("a", expY)
        , ("x", applyS expBin [expY, expA])
        ]
        makeTruePredicate
        (CombinedUnificationProof
            [ AndDistributionAndConstraintLifting
                (symbolHead expBin)
                [ Proposition_5_24_3
                    [ FunctionalHead (symbolHead expBin)
                    , FunctionalVariable (var expA)
                    , FunctionalVariable (var expY)
                    ]
                    (var expX)
                    (extractPurePattern (applyS expBin [expA, expY]))
                , Proposition_5_24_3
                    [ FunctionalHead (symbolHead expBin)
                    , FunctionalVariable (var expY)
                    , FunctionalVariable (var expA)
                    ]
                    (var expX)
                    (extractPurePattern (applyS expBin [expY, expA]))
                ]
            , AndDistributionAndConstraintLifting
                (symbolHead expBin)
                [ Proposition_5_24_3
                    [FunctionalVariable (var expY)]
                    (var expA)
                    (extractPurePattern expY)
                , Proposition_5_24_3
                    [FunctionalVariable (var expA)]
                    (var expY)
                    (extractPurePattern expA)
                ]
            , ConjunctionIdempotency
                (extractPurePattern expY)
            ]
        )
    , testGroup "inj unification tests" injUnificationTests
    , andSimplifyFailure "Unmatching constants"
        (UnificationTerm aA)
        (UnificationTerm a1A)
        (PatternClash (HeadClash (symbolHead a)) (HeadClash (symbolHead a1)))
    , andSimplifyFailure "Unmatching domain values"
        (UnificationTerm dv1)
        (UnificationTerm dv2)
        (PatternClash (DomainValueClash "dv1") (DomainValueClash "dv2"))
    , andSimplifyFailure "Unmatching constant + domain value"
        (UnificationTerm aA)
        (UnificationTerm dv2)
        (PatternClash (HeadClash (symbolHead a)) (DomainValueClash "dv2"))
    , andSimplifyFailure "Unmatching domain value + constructor constant"
        (UnificationTerm dv1)
        (UnificationTerm a1A)
        (PatternClash (DomainValueClash "dv1") (HeadClash (symbolHead a1)))
    , andSimplifyFailure "Unmatching domain value + nonconstructor constant"
        (UnificationTerm dv1)
        (UnificationTerm a2A)
        (NonConstructorHead (symbolHead a2))
    , andSimplifyFailure "Unmatching nonconstructor constant + domain value"
        (UnificationTerm a2A)
        (UnificationTerm dv1)
        (NonConstructorHead (symbolHead a2))
    , andSimplifyFailure "non-functional pattern"
        (UnificationTerm x)
        (UnificationTerm a3A)
        NonFunctionalPattern
    , andSimplifyFailure "non-constructor symbolHead right"
        (UnificationTerm aA)
        (UnificationTerm a2A)
        (NonConstructorHead (symbolHead a2))
    , andSimplifyFailure "non-constructor symbolHead left"
        (UnificationTerm a2A)
        (UnificationTerm aA)
        (NonConstructorHead (symbolHead a2))
    , andSimplifyFailure "nested failure"
        (UnificationTerm (applyS f [aA]))
        (UnificationTerm (applyS f [a1A]))
        (PatternClash (HeadClash (symbolHead a)) (HeadClash (symbolHead a1)))
    , andSimplifyFailure "Unsupported constructs"
        (UnificationTerm (applyS f [aA]))
        (UnificationTerm (applyS f [implies_ aA (next_ a1A)]))
        UnsupportedPatterns
          {- currently this cannot even be built because of builder checkd
    , andSimplifyFailure "Unmatching sorts"
        (UnificationTerm aA)
        (UnificationTerm bA)
        UnificationError
        -}
    , testCase "Maps substitution variables"
        (assertEqualWithExplanation ""
            [(W "1", war' "2")]
            (mapSubstitutionVariables showVar
                [(V 1, var' 2)]
            )
        )
    ]

var :: CommonPurePatternStub Object -> Variable Object
var ps = case project (extractPurePattern ps) of
    VariablePattern v -> v
    _                 -> error "Expecting a variable"

symbolHead
    :: SentenceSymbolOrAlias s
    => s level (Pattern level) variable
    -> SymbolOrAlias level
symbolHead symbol = getSentenceSymbolOrAliasHead symbol []

newtype V level = V Integer
    deriving (Show, Eq, Ord)
newtype W level = W String
    deriving (Show, Eq, Ord)

instance EqualWithExplanation (V level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (W level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show


showVar :: V level -> W level
showVar (V i) = W (show i)

var' :: Integer -> PureMLPattern Meta V
var' i = give mockSortTools' (mkVar (V i))

war' :: String -> PureMLPattern Meta W
war' s = give mockSortTools' (mkVar (W s))

mockSortTools' :: SortTools Meta
mockSortTools' = const ApplicationSorts
    { applicationSortsOperands = [sortVar, sortVar]
    , applicationSortsResult = sortVar
    }

sortVar :: Sort level
sortVar = SortVariableSort (SortVariable (Id "#a" AstLocationTest))

injUnificationTests :: [TestTree]
injUnificationTests =
    [ andSimplifySuccess "Injected Variable"
        (UnificationTerm (applyInj s1 s2 x))
        (UnificationTerm (applyInj s1 s2 aA))
        (UnificationResultTerm (applyInj s1 s2 aA))
        [("x", aA)]
        (AndDistributionAndConstraintLifting
            (injHead s1 s2)
            [Proposition_5_24_3
                [FunctionalHead (symbolHead a)]
                (var x)
                (extractPurePattern aA)
            ]
        )
    , andSimplifySuccess "Variable"
        (UnificationTerm xs2)
        (UnificationTerm (applyInj s1 s2 aA))
        (UnificationResultTerm (applyInj s1 s2 aA))
        [("xs2", applyInj s1 s2 aA)]
        (Proposition_5_24_3
            [FunctionalHead (injHead s1 s2), FunctionalHead (symbolHead a)]
            (var xs2)
            (extractPurePattern (applyInj s1 s2 aA))
        )
    , andSimplifySuccess "Injected Variable vs doubly injected term"
        (UnificationTerm (applyInj s1 s2 x))
        (simplifyPattern (UnificationTerm (applyInj s3 s2 (applyInj s1 s3 aA))))
        (UnificationResultTerm (applyInj s1 s2 aA))
        [("x", aA)]
        (AndDistributionAndConstraintLifting
            (injHead s1 s2)
            [Proposition_5_24_3
                [FunctionalHead (symbolHead a)]
                (var x)
                (extractPurePattern aA)
            ]
        )
    , andSimplifySuccess "doubly injected variable vs injected term"
        (simplifyPattern (UnificationTerm (applyInj s3 s2 (applyInj s1 s3 x))))
        (UnificationTerm (applyInj s1 s2 aA))
        (UnificationResultTerm (applyInj s1 s2 aA))
        [("x", aA)]
        (AndDistributionAndConstraintLifting
            (injHead s1 s2)
            [Proposition_5_24_3
                [FunctionalHead (symbolHead a)]
                (var x)
                (extractPurePattern aA)
            ]
        )
    , andSimplifySuccess "doubly injected variable vs doubly injected term"
        (simplifyPattern (UnificationTerm (applyInj s4 s2 (applyInj s1 s4 x))))
        (simplifyPattern (UnificationTerm (applyInj s3 s2 (applyInj s1 s3 aA))))
        (UnificationResultTerm (applyInj s1 s2 aA))
        [("x", aA)]
        (AndDistributionAndConstraintLifting
            (injHead s1 s2)
            [Proposition_5_24_3
                [FunctionalHead (symbolHead a)]
                (var x)
                (extractPurePattern aA)
            ]
        )
    , andSimplifyFailure "constant vs injection"
        (UnificationTerm aA)
        (UnificationTerm (applyInj s2 s1 xs2))
        (PatternClash (HeadClash (symbolHead a)) (SortInjectionClash s2 s1))
    , andSimplifyFailure "unmatching injections"
        (UnificationTerm (applyInj s1 s3 aA))
        (UnificationTerm (applyInj s2 s3 bA))
        (PatternClash (SortInjectionClash s1 s3) (SortInjectionClash s2 s3))
    , andSimplifyFailure "unmatching nested injections"
        (simplifyPattern (UnificationTerm (applyInj s2 s4 (applyInj s1 s2 aA))))
        (simplifyPattern (UnificationTerm (applyInj s3 s4 (applyInj s2 s3 bA))))
        (PatternClash (SortInjectionClash s1 s4) (SortInjectionClash s2 s4))
    , andSimplifyFailure "unmatching injections"
        -- TODO(traiansf): this should succeed if s1 < s2 < s3
        (UnificationTerm (applyInj s1 s3 aA))
        (UnificationTerm (applyInj s2 s3 xs2))
        UnsupportedPatterns
    ]

simplifyPattern :: UnificationTerm Object -> UnificationTerm Object
simplifyPattern (UnificationTerm pStub) =
    let pat =
            project
            $ ExpandedPattern.term
            $ evalSimplifier
            $ do
                simplifiedPatterns <-
                    ExpandedPattern.simplify tools simplifier expandedPattern
                case
                    OrOfExpandedPattern.extractPatterns
                        (fst simplifiedPatterns) of
                    [] -> return ExpandedPattern.bottom
                    (config : _) -> return config
        resultSort = getPatternResultSort mockSortTools pat
    in UnificationTerm (SortedPatternStub (SortedPattern pat resultSort))
  where
    functionRegistry = Map.empty
    simplifier = Simplifier.create tools functionRegistry
    expandedPattern = ExpandedPattern
        { term = extractPurePattern pStub
        , predicate = makeTruePredicate
        , substitution = []
        }
