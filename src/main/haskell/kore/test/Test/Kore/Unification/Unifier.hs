module Test.Kore.Unification.Unifier
    ( test_unification
    , test_unsupportedConstructs
    ) where

import Test.Tasty
       ( TestName, TestTree, testGroup )
import Test.Tasty.HUnit
import Test.Tasty.HUnit.Extensions

import           Control.Exception
                 ( ErrorCall (ErrorCall), catch, evaluate )
import           Control.Monad.Except
                 ( runExceptT )
import           Data.Function
                 ( on )
import           Data.List
                 ( sortBy )
import qualified Data.Map as Map
import           Data.Reflection
                 ( give )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )

import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.AST.Valid hiding
                 ( V )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.Attribute.Constructor
import           Kore.Attribute.Function
import           Kore.Attribute.Functional
import           Kore.Attribute.Injective
import           Kore.Attribute.SortInjection
import qualified Kore.Domain.Builtin as Domain
import           Kore.IndexedModule.MetadataTools
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.Predicate.Predicate
                 ( Predicate, makeCeilPredicate, makeFalsePredicate,
                 makeTruePredicate )
import qualified Kore.Predicate.Predicate as Predicate
                 ( makeEqualsPredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
import qualified Kore.Step.Simplification.Simplifier as Simplifier
import           Kore.Step.StepperAttributes
import           Kore.Unification.Data
import           Kore.Unification.Error
import           Kore.Unification.Procedure
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.UnifierImpl
import           SMT
                 ( SMT )
import qualified SMT

import           Test.Kore
import           Test.Kore.ASTVerifier.DefinitionVerifier
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSimplifiers as Mock

applyInj
    :: Sort Object
    -> CommonStepPattern Object
    -> CommonStepPattern Object
applyInj sortTo pat =
    applySymbol' symbolInj [sortFrom, sortTo] [pat]
  where
    Valid { patternSort = sortFrom } = extract pat

s1, s2, s3, s4 :: Sort Object
s1 = simpleSort (SortName "s1")
s2 = simpleSort (SortName "s2")
s3 = simpleSort (SortName "s3")
s4 = simpleSort (SortName "s4")

a1, a2, a3, a4, a5 :: SentenceSymbol Object (CommonStepPattern Object)
a1 = mkSymbol (testId "a1") [] s1
a2 = mkSymbol (testId "a2") [] s1
a3 = mkSymbol (testId "a3") [] s1
a4 = mkSymbol (testId "a4") [] s1
a5 = mkSymbol (testId "a5") [] s1

a, b, c, f, g, h :: SentenceSymbol Object (CommonStepPattern Object)
a = mkSymbol (testId "a") [] s1
b = mkSymbol (testId "b") [] s2
c = mkSymbol (testId "c") [] s3
f = mkSymbol (testId "f") [s1] s2
g = mkSymbol (testId "g") [s1, s2] s3
h = mkSymbol (testId "h") [s1, s2, s3] s1

ef, eg, eh :: SentenceSymbol Object (CommonStepPattern Object)
ef = mkSymbol (testId "ef") [s1, s1, s1] s1
eg = mkSymbol (testId "eg") [s1] s1
eh = mkSymbol (testId "eh") [s1] s1

nonLinF, nonLinG, nonLinAS :: SentenceSymbol Object (CommonStepPattern Object)
nonLinF  = mkSymbol (testId "nonLinF") [s1, s1] s1
nonLinG  = mkSymbol (testId "nonLinG") [s1] s1
nonLinAS = mkSymbol (testId "nonLinA") [] s1

nonLinA, nonLinX, nonLinY :: CommonStepPattern Object
nonLinA = applySymbol nonLinAS []
nonLinX = mkVar Variable { variableName = testId "x", variableSort = s1 }
nonLinY = mkVar Variable { variableName = testId "y", variableSort = s1 }

expBin :: SentenceSymbol Object (CommonStepPattern Object)
expBin = mkSymbol (testId "times") [s1, s1] s1

expA, expX, expY :: CommonStepPattern Object
expA = mkVar Variable { variableName = testId "a", variableSort = s1 }
expX = mkVar Variable { variableName = testId "x", variableSort = s1 }
expY = mkVar Variable { variableName = testId "y", variableSort = s1 }

ex1, ex2, ex3, ex4 :: CommonStepPattern Object
ex1 = mkVar Variable { variableName = testId "ex1", variableSort = s1 }
ex2 = mkVar Variable { variableName = testId "ex2", variableSort = s1 }
ex3 = mkVar Variable { variableName = testId "ex3", variableSort = s1 }
ex4 = mkVar Variable { variableName = testId "ex4", variableSort = s1 }


dv1, dv2 :: CommonStepPattern Object
dv1 =
    mkDomainValue s1
    $ Domain.BuiltinPattern
    $ eraseAnnotations
    $ mkStringLiteral "dv1"
dv2 =
    mkDomainValue s1
    $ Domain.BuiltinPattern
    $ eraseAnnotations
    $ mkStringLiteral "dv2"

aA :: CommonStepPattern Object
aA = applySymbol a []

a1A :: CommonStepPattern Object
a1A = applySymbol a1 []

a2A :: CommonStepPattern Object
a2A = applySymbol a2 []

a3A :: CommonStepPattern Object
a3A = applySymbol a3 []

a4A :: CommonStepPattern Object
a4A = applySymbol a4 []

a5A :: CommonStepPattern Object
a5A = applySymbol a5 []

bA :: CommonStepPattern Object
bA = applySymbol b []

x :: CommonStepPattern Object
x = mkVar Variable { variableName = testId "x", variableSort = s1 }

xs2 :: CommonStepPattern Object
xs2 = mkVar Variable { variableName = testId "xs2", variableSort = s2 }

symbols :: [(SymbolOrAlias Object, SentenceSymbol Object (CommonStepPattern Object))]
symbols =
    map
        (\s -> (getSentenceSymbolOrAliasHead s [], s))
        [ a, a1, a2, a3, a4, a5, b, c, f, g, h
        , ef, eg, eh
        , nonLinF, nonLinG, nonLinAS
        , expBin
        ]

sortParam :: Text -> SortVariable level
sortParam name = SortVariable (testId name)

sortParamSort :: Text -> Sort level
sortParamSort = SortVariableSort . sortParam

injName :: Text
injName = "inj"

symbolInj :: SentenceSymbol Object (CommonStepPattern Object)
symbolInj =
    mkSymbol'
        (testId injName)
        [sortParam "From", sortParam "To"]
        [sortParamSort "From"]
        (sortParamSort "To")

isInjHead :: SymbolOrAlias level -> Bool
isInjHead pHead = getId (symbolOrAliasConstructor pHead) == injName

mockStepperAttributes :: SymbolOrAlias Object -> StepperAttributes
mockStepperAttributes patternHead =
    defaultStepperAttributes
        { constructor = Constructor { isConstructor }
        , functional = Functional { isDeclaredFunctional }
        , function = Function { isDeclaredFunction }
        , injective = Injective { isDeclaredInjective }
        , sortInjection = SortInjection { isSortInjection }
        }
  where
    isConstructor =
            patternHead /= getSentenceSymbolOrAliasHead a2 []
        &&  patternHead /= getSentenceSymbolOrAliasHead a4 []
        &&  patternHead /= getSentenceSymbolOrAliasHead a5 []
        &&  not (isInjHead patternHead)
    isDeclaredFunctional =
            patternHead /= getSentenceSymbolOrAliasHead a3 []
        &&  patternHead /= getSentenceSymbolOrAliasHead a5 []
    isDeclaredFunction = patternHead == getSentenceSymbolOrAliasHead a5 []
    isDeclaredInjective =
        (  patternHead /= getSentenceSymbolOrAliasHead a2 []
        && patternHead /= getSentenceSymbolOrAliasHead a5 []
        )
        || isInjHead patternHead
    isSortInjection = isInjHead patternHead

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

mockSymbolOrAliasSorts :: SymbolOrAliasSorts Object
mockSymbolOrAliasSorts pHead = ApplicationSorts
    { applicationSortsOperands = mockGetArgumentSorts pHead
    , applicationSortsResult = mockGetResultSort pHead
    }

tools :: MetadataTools Object StepperAttributes
tools = MetadataTools
    { symAttributes = mockStepperAttributes
    , symbolOrAliasType = const HeadType.Symbol
    , sortAttributes = undefined
    , symbolOrAliasSorts = mockSymbolOrAliasSorts
    , isSubsortOf = const $ const False
    , subsorts = Set.singleton
    }

unificationProblem
    :: MetaOrObject level
    => UnificationTerm level
    -> UnificationTerm level
    -> CommonStepPattern level
unificationProblem (UnificationTerm term1) (UnificationTerm term2) =
    mkAnd term1 term2

type Substitution level = [(Text, CommonStepPattern level)]

unificationSubstitution
    :: Substitution Object
    -> [ (Variable Object, CommonStepPattern Object) ]
unificationSubstitution = map trans
  where
    trans (v, p) =
        ( Variable { variableSort = getSort p, variableName = testId v }
        , p
        )

unificationResult
    :: UnificationResultTerm Object
    -> Substitution Object
    -> Predicate Object Variable
    -> ExpandedPattern Object Variable
unificationResult (UnificationResultTerm term) sub predicate =
    Predicated
        { term
        , predicate = predicate
        , substitution = Substitution.wrap $ unificationSubstitution sub
        }

newtype UnificationTerm level = UnificationTerm (CommonStepPattern level)
newtype UnificationResultTerm level =
    UnificationResultTerm (CommonStepPattern level)

andSimplifySuccess
    :: HasCallStack
    => UnificationTerm Object
    -> UnificationTerm Object
    -> UnificationResultTerm Object
    -> Substitution Object
    -> Predicate Object Variable
    -> UnificationProof Object Variable
    -> Assertion
andSimplifySuccess term1 term2 resultTerm subst predicate proof = do
    let expect = (unificationResult resultTerm subst predicate, proof)
    Right (subst', proof') <-
        runSMT
        $ evalSimplifier
        $ runExceptT
        $ simplifyAnds
            tools
            (Mock.substitutionSimplifier tools)
            [(unificationProblem term1 term2)]
    let
        subst'' =
            subst'
                { substitution =
                    sortBy
                        (compare `on` fst)
                    `Substitution.modify`
                    ExpandedPattern.substitution subst'
                }
    assertEqualWithExplanation "" expect (subst'', proof')

andSimplifyFailure
    :: HasCallStack
    => UnificationTerm Object
    -> UnificationTerm Object
    -> UnificationError
    -> Assertion
andSimplifyFailure term1 term2 err = do
    let expect = Left (UnificationError err)
    actual <-
        runSMT
        $ evalSimplifier
        $ runExceptT
        $ simplifyAnds
            tools
            (Mock.substitutionSimplifier tools)
            [(unificationProblem term1 term2)]
    assertEqualWithPrinter show "" expect actual

andSimplifyException
    :: HasCallStack
    => String
    -> UnificationTerm Object
    -> UnificationTerm Object
    -> String
    -> TestTree
andSimplifyException message term1 term2 exceptionMessage =
    testCase
        message
        ( catch test handler )
    where
        test = do
            var <-
                runSMT
                $ evalSimplifier
                $ runExceptT
                $ simplifyAnds
                    tools
                    (Mock.substitutionSimplifier tools)
                    [(unificationProblem term1 term2)]
            _ <- evaluate var
            assertFailure "This evaluation should fail"
        handler (ErrorCall s) =
            assertEqual ""
                exceptionMessage
                s

unificationProcedureSuccess
    :: HasCallStack
    => TestName
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
  =
    testCase message $ do
        Right result <-
            runSMT
            $ evalSimplifier
            $ runExceptT
            $ unificationProcedure
                tools
                (Mock.substitutionSimplifier tools)
                term1
                term2
        let
            (Predicated { substitution, predicate }, proof') = result
            actual =
                ( sortBy (compare `on` fst) $ Substitution.unwrap substitution
                , predicate
                , proof'
                )
        assertEqualWithExplanation "" expect actual
  where
    expect = (unificationSubstitution subst, predicate', proof)


test_unification :: [TestTree]
test_unification =
    [ testCase "Constant" $ do
        andSimplifySuccess
            (UnificationTerm aA)
            (UnificationTerm aA)
            (UnificationResultTerm aA)
            []
            makeTruePredicate
            EmptyUnificationProof
    , testCase "Variable" $ do
        andSimplifySuccess
            (UnificationTerm x)
            (UnificationTerm aA)
            (UnificationResultTerm aA)
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "one level" $ do
        andSimplifySuccess
            (UnificationTerm (applySymbol f [x]))
            (UnificationTerm (applySymbol f [aA]))
            (UnificationResultTerm (applySymbol f [aA]))
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "equal non-constructor patterns" $ do
        andSimplifySuccess
            (UnificationTerm a2A)
            (UnificationTerm a2A)
            (UnificationResultTerm a2A)
            []
            makeTruePredicate
            EmptyUnificationProof
    , testCase "variable + non-constructor pattern" $ do
        andSimplifySuccess
            (UnificationTerm a2A)
            (UnificationTerm x)
            (UnificationResultTerm a2A)
            [("x", a2A)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "https://basics.sjtu.edu.cn/seminars/c_chu/Algorithm.pdf slide 3" $ do
        andSimplifySuccess
            (UnificationTerm (applySymbol ef [ex1, applySymbol eh [ex1], ex2]))
            (UnificationTerm (applySymbol ef [applySymbol eg [ex3], ex4, ex3]))
            (UnificationResultTerm
                (applySymbol ef [applySymbol eg [ex3], applySymbol eh [ex1], ex3])
            )
            [ ("ex1", applySymbol eg [ex3])
            , ("ex2", ex3)
            , ("ex4", applySymbol eh [applySymbol eg [ex3]])
            ]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "f(g(X),X) = f(Y,a) https://en.wikipedia.org/wiki/Unification_(computer_science)#Examples_of_syntactic_unification_of_first-order_terms" $ do
        andSimplifySuccess

            (UnificationTerm
                (applySymbol nonLinF [applySymbol nonLinG [nonLinX], nonLinX])
            )
            (UnificationTerm (applySymbol nonLinF [nonLinY, nonLinA]))
            (UnificationResultTerm
                (applySymbol nonLinF [applySymbol nonLinG [nonLinX], nonLinA])
            )
            -- [ ("x", nonLinA), ("y", applySymbol nonLinG [nonLinX])]
            [ ("x", nonLinA)
            , ("y", applySymbol nonLinG [nonLinA])
            ]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "times(times(a, y), x) = times(x, times(y, a))" $ do
        andSimplifySuccess
            (UnificationTerm (applySymbol expBin [applySymbol expBin [expA, expY], expX]))
            (UnificationTerm (applySymbol expBin [expX, applySymbol expBin [expY, expA]]))
            (UnificationResultTerm (applySymbol
                expBin
                [ applySymbol expBin [expA, expY]
                , applySymbol expBin [expY, expA]
                ]
            ))
            [ ("a", expY)
            , ("x", applySymbol expBin [expY, expY])
            ]
            makeTruePredicate
            EmptyUnificationProof
    , unificationProcedureSuccess
        "times(x, g(x)) = times(a, a) -- cycle bottom"
        (UnificationTerm (applySymbol expBin [expX, applySymbol eg [expX]]))
        (UnificationTerm (applySymbol expBin [expA, expA]))
        []
        makeFalsePredicate
        EmptyUnificationProof
    , unificationProcedureSuccess
        "times(times(a, y), x) = times(x, times(y, a))"
        (UnificationTerm (applySymbol expBin [applySymbol expBin [expA, expY], expX]))
        (UnificationTerm (applySymbol expBin [expX, applySymbol expBin [expY, expA]]))
        [ ("a", expY)
        , ("x", applySymbol expBin [expY, expY])
        ]
        makeTruePredicate
        EmptyUnificationProof
    , unificationProcedureSuccess
        "Unifying two non-ctors results in equals predicate"
         (UnificationTerm a2A)
         (UnificationTerm a4A)
         []
         (makeEqualsPredicate a2A a4A)
         EmptyUnificationProof
    , unificationProcedureSuccess
        "Unifying function and variable results in ceil predicate"
         (UnificationTerm x)
         (UnificationTerm a5A)
         [ ("x", a5A)
         ]
         (give mockSymbolOrAliasSorts $ makeCeilPredicate a5A)
         EmptyUnificationProof
    , testGroup "inj unification tests" injUnificationTests
    , testCase "Unmatching constants is bottom" $ do
        andSimplifySuccess
            (UnificationTerm aA)
            (UnificationTerm a1A)
            (UnificationResultTerm (mkBottomOf aA))
            []
            makeFalsePredicate
            EmptyUnificationProof
    , testCase "Unmatching domain values is bottom" $ do
        andSimplifySuccess
            (UnificationTerm dv1)
            (UnificationTerm dv2)
            (UnificationResultTerm (mkBottomOf dv1))
            []
            makeFalsePredicate
            EmptyUnificationProof
    , andSimplifyException "Unmatching constructor constant + domain value"
        (UnificationTerm aA)
        (UnificationTerm dv2)
        "Cannot handle Constructor and DomainValue"
    , andSimplifyException "Unmatching domain value + constructor constant"
        (UnificationTerm dv1)
        (UnificationTerm aA)
        "Cannot handle DomainValue and Constructor"
    , testCase "Unmatching domain value + nonconstructor constant" $ do
        andSimplifySuccess
            (UnificationTerm dv1)
            (UnificationTerm a2A)
            (UnificationResultTerm dv1)
            []
            (makeEqualsPredicate dv1 a2A)
            EmptyUnificationProof
    , testCase "Unmatching nonconstructor constant + domain value" $ do
        andSimplifySuccess
            (UnificationTerm a2A)
            (UnificationTerm dv1)
            (UnificationResultTerm a2A)
            []
            (makeEqualsPredicate a2A dv1)
            EmptyUnificationProof
    , testCase "non-functional pattern" $ do
        andSimplifyFailure
            (UnificationTerm x)
            (UnificationTerm a3A)
            UnsupportedPatterns
    , testCase "non-constructor symbolHead right" $ do
        andSimplifySuccess
            (UnificationTerm aA)
            (UnificationTerm a2A)
            (UnificationResultTerm aA)
            []
            (makeEqualsPredicate aA a2A)
            EmptyUnificationProof
    , testCase "non-constructor symbolHead left" $ do
        andSimplifySuccess
            (UnificationTerm a2A)
            (UnificationTerm aA)
            (UnificationResultTerm a2A)
            []
            (makeEqualsPredicate a2A aA)
            EmptyUnificationProof
    , testCase "nested a=a1 is bottom" $ do
        andSimplifySuccess
            (UnificationTerm (applySymbol f [aA]))
            (UnificationTerm (applySymbol f [a1A]))
            (UnificationResultTerm (mkBottom s2))
            []
            makeFalsePredicate
            EmptyUnificationProof
          {- currently this cannot even be built because of builder checkd
    , andSimplifyFailure "Unmatching sorts"
        (UnificationTerm aA)
        (UnificationTerm bA)
        UnificationError
        -}
    , testCase "Maps substitution variables"
        (assertEqualWithExplanation ""
            [(W "1", war' "2")]
            (Substitution.unwrap
                . Substitution.mapVariables showVar
                . Substitution.wrap
                $ [(V 1, var' 2)]
            )
        )
    ]

test_unsupportedConstructs :: TestTree
test_unsupportedConstructs =
    testCase "Unsupported constructs" $ do
        andSimplifyFailure
            (UnificationTerm (applySymbol f [aA]))
            (UnificationTerm (applySymbol f [mkImplies aA (mkNext a1A)]))
            UnsupportedPatterns

newtype V level = V Integer
    deriving (Show, Eq, Ord)

newtype W level = W String
    deriving (Show, Eq, Ord)

instance SortedVariable V where
    sortedVariableSort _ = sortVar

instance SortedVariable W where
    sortedVariableSort _ = sortVar

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

var' :: Integer -> StepPattern Meta V
var' i = give mockSymbolOrAliasSorts' (mkVar (V i))

war' :: String -> StepPattern Meta W
war' s = give mockSymbolOrAliasSorts' (mkVar (W s))

mockSymbolOrAliasSorts' :: SymbolOrAliasSorts Meta
mockSymbolOrAliasSorts' = const ApplicationSorts
    { applicationSortsOperands = [sortVar, sortVar]
    , applicationSortsResult = sortVar
    }

sortVar :: Sort level
sortVar = SortVariableSort (SortVariable (Id "#a" AstLocationTest))

injUnificationTests :: [TestTree]
injUnificationTests =
    [ testCase "Injected Variable" $ do
        andSimplifySuccess
            (UnificationTerm (applyInj s2 x))
            (UnificationTerm (applyInj s2 aA))
            (UnificationResultTerm (applyInj s2 aA))
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "Variable" $ do
        andSimplifySuccess
            (UnificationTerm xs2)
            (UnificationTerm (applyInj s2 aA))
            (UnificationResultTerm (applyInj s2 aA))
            [("xs2", applyInj s2 aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "Injected Variable vs doubly injected term" $ do
        term2 <-
            simplifyPattern
            $ UnificationTerm (applyInj s2 (applyInj s3 aA))
        andSimplifySuccess
            (UnificationTerm (applyInj s2 x))
            term2
            (UnificationResultTerm (applyInj s2 aA))
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "doubly injected variable vs injected term" $ do
        term1 <-
            simplifyPattern
            $ UnificationTerm (applyInj s2 (applyInj s3 x))
        andSimplifySuccess
            term1
            (UnificationTerm (applyInj s2 aA))
            (UnificationResultTerm (applyInj s2 aA))
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "doubly injected variable vs doubly injected term" $ do
        term1 <-
            simplifyPattern
            $ UnificationTerm (applyInj s2 (applyInj s4 x))
        term2 <-
            simplifyPattern
            $ UnificationTerm (applyInj s2 (applyInj s3 aA))
        andSimplifySuccess
            term1
            term2
            (UnificationResultTerm (applyInj s2 aA))
            [("x", aA)]
            makeTruePredicate
            EmptyUnificationProof
    , testCase "constant vs injection is bottom" $ do
        andSimplifySuccess
            (UnificationTerm aA)
            (UnificationTerm (applyInj s1 xs2))
            (UnificationResultTerm (mkBottom s1))
            []
            makeFalsePredicate
            EmptyUnificationProof
    , testCase "unmatching nested injections" $ do
        term1 <-
            simplifyPattern
            $ UnificationTerm (applyInj s4 (applyInj s2 aA))
        term2 <-
            simplifyPattern
            $ UnificationTerm (applyInj s4 (applyInj s3 bA))
        andSimplifySuccess
            term1
            term2
            (UnificationResultTerm (mkBottom s4))
            []
            makeFalsePredicate
            EmptyUnificationProof
    , testCase "unmatching injections" $ do
        andSimplifySuccess
            -- TODO(traiansf): this should succeed if s1 < s2 < s3
            (UnificationTerm (applyInj s3 aA))
            (UnificationTerm (applyInj s3 xs2))
            (UnificationResultTerm (mkBottom s3))
            []
            makeFalsePredicate
            EmptyUnificationProof
    ]

simplifyPattern :: UnificationTerm Object -> IO (UnificationTerm Object)
simplifyPattern (UnificationTerm term) = do
    Predicated { term = term' } <- runSMT $ evalSimplifier simplifier
    return $ UnificationTerm term'
  where
    simplifier = do
        simplifiedPatterns <-
            ExpandedPattern.simplify
                tools
                (Mock.substitutionSimplifier tools)
                (Simplifier.create tools functionRegistry)
                expandedPattern
        case
            OrOfExpandedPattern.extractPatterns
                (fst simplifiedPatterns) of
            [] -> return (ExpandedPattern.bottomOf term)
            (config : _) -> return config
    functionRegistry = Map.empty
    expandedPattern = pure term

makeEqualsPredicate
    :: CommonStepPattern Object
    -> CommonStepPattern Object
    -> Predicate Object Variable
makeEqualsPredicate t1 t2 =
        give mockSymbolOrAliasSorts
            $ Predicate.makeEqualsPredicate t1 t2

runSMT :: SMT a -> IO a
runSMT = SMT.runSMT SMT.defaultConfig
