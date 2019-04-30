module Test.Kore.ASTVerifier.DefinitionVerifier where

import Test.Tasty
       ( TestTree, testGroup )
import Test.Tasty.HUnit
       ( HasCallStack, assertEqual, assertFailure, testCase )

import qualified Data.Set as Set
import           Data.Text
                 ( Text )

import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.AST.Valid
import           Kore.ASTPrettyPrint
import           Kore.ASTVerifier.DefinitionVerifier
import           Kore.ASTVerifier.Error
import qualified Kore.Attribute.Null as Attribute
import qualified Kore.Builtin as Builtin
import           Kore.Error
import qualified Kore.Parser.Pattern as Parser
import           Kore.Parser.Sentence
import           Kore.Step.TermLike hiding
                 ( freeVariables )
import           Kore.Unparser
                 ( unparseToString )

import Test.Kore

newtype ExpectedErrorMessage = ExpectedErrorMessage String
newtype ErrorStack = ErrorStack [String]

data TestData = TestData
    { testDataDescription :: !String
    , testDataError       :: !(Error VerifyError)
    , testDataDefinition  :: !ParsedDefinition
    }

addPrefixToDescription :: String -> [TestData] -> [TestData]
addPrefixToDescription prefix =
    map
        (\t -> t {testDataDescription = prefix ++ testDataDescription t})

failureTestDataGroup
    :: HasCallStack
    => String -> ExpectedErrorMessage -> ErrorStack -> [TestData] -> TestTree
failureTestDataGroup description errorMessage errorStack testData =
    testGroup
        description
        (map (failureTestData errorMessage errorStack) testData)

failureTestData
    :: HasCallStack
    => ExpectedErrorMessage -> ErrorStack -> TestData -> TestTree
failureTestData
    (ExpectedErrorMessage message)
    (ErrorStack stack)
    testData
  =
    expectFailureWithError
        (testDataDescription testData)
        err
            { errorError = message
            , errorContext = errorContext err ++ stack
            }
        (testDataDefinition testData)
  where
    err = testDataError testData

successTestDataGroup :: HasCallStack => String -> [TestData] -> TestTree
successTestDataGroup description testDatas =
    testGroup description (map successTestData testDatas)

successTestData :: HasCallStack => TestData -> TestTree
successTestData testData =
    expectSuccess (testDataDescription testData) (testDataDefinition testData)

expectSuccess
    :: HasCallStack
    => String
    -> ParsedDefinition
    -> TestTree
expectSuccess description definition =
    testCase
        description
        (assertEqual
            (  "Expecting verification success! Definition:\n"
            ++ printDefinition definition
            )
            verifySuccess
            (verifyDefinition
                attributesVerificationForTests
                Builtin.koreVerifiers
                definition
            )
        )

expectFailureWithError
    :: HasCallStack
    => String
    -> Error VerifyError
    -> ParsedDefinition
    -> TestTree
expectFailureWithError description expectedError definition =
    testCase
        description
        (case
            verifyDefinition
                attributesVerificationForTests
                Builtin.koreVerifiers
                definition
          of
            Right _ ->
                assertFailure
                    (  "Expecting verification failure! Definition:\n"
                    ++ printDefinition definition
                    )
            Left actualError ->
                assertEqual
                    ( "Expecting a certain error! Definition:\n"
                    ++ printDefinition definition
                    )
                    expectedError actualError
        )

attributesVerificationForTests
    :: AttributesVerification Attribute.Null Attribute.Null
attributesVerificationForTests = defaultNullAttributesVerification

printDefinition :: ParsedDefinition -> String
printDefinition definition =
    prettyPrintToString definition
    ++ "\n----------------------\n"
    ++ unparseToString definition
    ++ "\n----------------------"

-------------------------------------------------------------

newtype AliasName = AliasName Text
newtype SymbolName = SymbolName Text
newtype SortName = SortName Text
newtype SortVariableName = SortVariableName Text
newtype VariableName = VariableName Text
newtype NamePrefix = NamePrefix Text
newtype OperandSort level = OperandSort Sort
newtype ResultSort level = ResultSort Sort
newtype DeclaredSort level = DeclaredSort Sort
newtype TestedPatternSort level = TestedPatternSort Sort
newtype SortVariablesThatMustBeDeclared level =
    SortVariablesThatMustBeDeclared [SortVariable]

simpleDefinitionFromSentences
    :: ModuleName
    -> [ParsedSentence]
    -> ParsedDefinition
simpleDefinitionFromSentences name sentences =
    Definition
        { definitionAttributes = Attributes []
        , definitionModules =
            [ Module
                { moduleName = name
                , moduleSentences = sentences
                , moduleAttributes = Attributes []
                }
            ]
        }

-- TODO: simple meta sort sentence?
simpleSortSentence :: SortName -> ParsedSentence
simpleSortSentence (SortName name) =
    SentenceSortSentence SentenceSort
        { sentenceSortName = testId name :: Id
        , sentenceSortParameters = []
        , sentenceSortAttributes = Attributes []
        }

simpleMetaAliasSentence :: AliasName -> SortName -> ParsedSentence
simpleMetaAliasSentence alias sort =
    SentenceAliasSentence (simpleAliasSentence alias sort r)
  where
    r = (Parser.asPattern . Parser.TopF . Top) (simpleSort sort :: Sort)

simpleObjectAliasSentence :: AliasName -> SortName -> ParsedSentence
simpleObjectAliasSentence = simpleMetaAliasSentence

simpleAliasSentence
    :: AliasName
    -> SortName
    -> ParsedPattern
    -> ParsedSentenceAlias
simpleAliasSentence (AliasName name) (SortName sort) r =
    SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = []
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort =
            SortActualSort SortActual
                { sortActualName = testId sort
                , sortActualSorts = []
                }
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams = []
                        }
                , applicationChildren = []
                }
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes []
        }

simpleMetaSymbolSentence :: SymbolName -> SortName -> ParsedSentence
simpleMetaSymbolSentence = simpleObjectSymbolSentence

simpleObjectSymbolSentence :: SymbolName -> SortName -> ParsedSentence
simpleObjectSymbolSentence name sort =
    SentenceSymbolSentence (simpleSymbolSentence name sort)

simpleSymbolSentence
    :: SymbolName
    -> SortName
    -> ParsedSentenceSymbol
simpleSymbolSentence (SymbolName name) (SortName sort) =
    SentenceSymbol
        { sentenceSymbolSymbol = Symbol
            { symbolConstructor = testId name
            , symbolParams = []
            }
        , sentenceSymbolSorts = []
        , sentenceSymbolResultSort =
            SortActualSort SortActual
                { sortActualName = testId sort
                , sortActualSorts = []
                }
        , sentenceSymbolAttributes = Attributes []
        }

simpleAxiomSentence :: ParsedPattern -> ParsedSentence
simpleAxiomSentence unifiedPattern =
    SentenceAxiomSentence
        (SentenceAxiom
            { sentenceAxiomParameters = []
            , sentenceAxiomPattern = unifiedPattern
            , sentenceAxiomAttributes = Attributes []
            }
            :: ParsedSentenceAxiom
        )

importSentence :: ModuleName -> ParsedSentence
importSentence name =
    SentenceImportSentence SentenceImport
        { sentenceImportModuleName = name
        , sentenceImportAttributes = Attributes []
        }

sortSentenceWithSortParameters
    :: SortName -> [SortVariable] -> ParsedSentence
sortSentenceWithSortParameters (SortName name) parameters =
    SentenceSortSentence SentenceSort
        { sentenceSortName = testId name
        , sentenceSortParameters = parameters
        , sentenceSortAttributes = Attributes []
        }

aliasSentenceWithSort
    :: AliasName -> Sort -> ParsedSentence
aliasSentenceWithSort (AliasName name) sort =
    SentenceAliasSentence SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = []
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort = sort
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams = []
                        }
                , applicationChildren = []
                }
        , sentenceAliasRightPattern =
            (Parser.asPattern . Parser.TopF) (Top sort)
        , sentenceAliasAttributes = Attributes []
        }

metaAliasSentenceWithSortParameters
    :: AliasName -> Sort -> [SortVariable] -> ParsedSentence
metaAliasSentenceWithSortParameters
    (AliasName name) sort parameters
  =
    SentenceAliasSentence SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = parameters
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort = sort
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams =
                            SortVariableSort <$> parameters
                        }
                , applicationChildren = []
                }
        , sentenceAliasRightPattern =
            (Parser.asPattern . Parser.TopF) (Top sort)
        , sentenceAliasAttributes = Attributes []
        }


aliasSentenceWithSortParameters
    :: AliasName
    -> SortName
    -> [SortVariable]
    -> ParsedPattern
    -> ParsedSentenceAlias
aliasSentenceWithSortParameters (AliasName name) (SortName sort) parameters r =
    SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = parameters
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort =
            SortActualSort SortActual
                { sortActualName = testId sort
                , sortActualSorts = []
                }
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams = SortVariableSort <$> parameters
                        }
                , applicationChildren = []
                }
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes []
        }

sentenceAliasWithSortArgument
    :: AliasName
    -> Sort
    -> Sort
    -> [SortVariable]
    -> ParsedPattern
    -> ParsedSentenceAlias
sentenceAliasWithSortArgument
    (AliasName name)
    sortArgument
    resultSort
    parameters
    r
  =
    SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = parameters
            }
        , sentenceAliasSorts = [sortArgument]
        , sentenceAliasResultSort = resultSort
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams =
                            SortVariableSort <$> parameters
                        }
                , applicationChildren =
                    [ Variable
                        { variableName = testId "x"
                        , variableCounter = mempty
                        , variableSort = sortArgument
                        }
                    ]
                }
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes []
        }

sentenceAliasWithAttributes
    :: AliasName
    -> [SortVariable]
    -> Sort
    -> [ParsedPattern]
    -> Application SymbolOrAlias (Variable)
    -> ParsedPattern
    -> ParsedSentenceAlias
sentenceAliasWithAttributes (AliasName name) params sort attributes l r =
    SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = params
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort = sort
        , sentenceAliasLeftPattern = l
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes attributes
        }

sentenceSymbolWithAttributes
    :: SymbolName
    -> [SortVariable]
    -> Sort
    -> [ParsedPattern]
    -> ParsedSentenceSymbol
sentenceSymbolWithAttributes (SymbolName name) params sort attributes =
    SentenceSymbol
        { sentenceSymbolSymbol = Symbol
            { symbolConstructor = testId name
            , symbolParams = params
            }
        , sentenceSymbolSorts = []
        , sentenceSymbolResultSort = sort
        , sentenceSymbolAttributes = Attributes attributes
        }

metaSymbolSentenceWithSortParameters
    :: SymbolName -> Sort -> [SortVariable] -> ParsedSentence
metaSymbolSentenceWithSortParameters
    (SymbolName name) sort parameters
  =
    SentenceSymbolSentence SentenceSymbol
        { sentenceSymbolSymbol = Symbol
            { symbolConstructor = testId name
            , symbolParams = parameters
            }
        , sentenceSymbolSorts = []
        , sentenceSymbolResultSort = sort
        , sentenceSymbolAttributes = Attributes []
        }

symbolSentenceWithSortParameters
    :: SymbolName
    -> SortName
    -> [SortVariable]
    -> ParsedSentenceSymbol
symbolSentenceWithSortParameters
    (SymbolName name) (SortName sort) parameters
  =
    SentenceSymbol
        { sentenceSymbolSymbol = Symbol
            { symbolConstructor = testId name
            , symbolParams = parameters
            }
        , sentenceSymbolSorts = []
        , sentenceSymbolResultSort =
            SortActualSort SortActual
                { sortActualName = testId sort
                , sortActualSorts = []
                }
        , sentenceSymbolAttributes = Attributes []
        }

axiomSentenceWithSortParameters
    :: Parser.Pattern Variable -> [SortVariable] -> ParsedSentence
axiomSentenceWithSortParameters unifiedPattern parameters =
    SentenceAxiomSentence SentenceAxiom
        { sentenceAxiomParameters = parameters
        , sentenceAxiomPattern = unifiedPattern
        , sentenceAxiomAttributes = Attributes []
        }

axiomSentenceWithAttributes
    :: [SortVariable]
    -> ParsedPattern
    -> [ParsedPattern]
    -> ParsedSentence
axiomSentenceWithAttributes parameters unifiedPattern attributes =
    SentenceAxiomSentence SentenceAxiom
        { sentenceAxiomParameters = parameters
        , sentenceAxiomPattern = unifiedPattern
        , sentenceAxiomAttributes = Attributes attributes
        }

sentenceAliasWithResultSort
    :: AliasName
    -> Sort
    -> [SortVariable]
    -> ParsedPattern
    -> ParsedSentenceAlias
sentenceAliasWithResultSort (AliasName name) sort parameters r =
    SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = parameters
            }
        , sentenceAliasSorts = []
        , sentenceAliasResultSort = sort
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams =
                            SortVariableSort <$> parameters
                        }
                , applicationChildren = []
                }
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes []
        }

symbolSentenceWithResultSort
    :: SymbolName -> Sort -> [SortVariable] -> ParsedSentence
symbolSentenceWithResultSort
    (SymbolName name) sort parameters
  = SentenceSymbolSentence
        SentenceSymbol
            { sentenceSymbolSymbol = Symbol
                { symbolConstructor = testId name
                , symbolParams = parameters
                }
            , sentenceSymbolSorts = []
            , sentenceSymbolResultSort = sort
            , sentenceSymbolAttributes =
                Attributes [] :: Attributes
            }

objectSymbolSentenceWithArguments
    :: SymbolName -> Sort -> [Sort] -> ParsedSentence
objectSymbolSentenceWithArguments = symbolSentenceWithArguments

symbolSentenceWithArguments
    :: SymbolName -> Sort -> [Sort] -> ParsedSentence
symbolSentenceWithArguments name
  = symbolSentenceWithParametersAndArguments name []

objectSymbolSentenceWithParametersAndArguments
    :: SymbolName
    -> [SortVariable]
    -> Sort
    -> [Sort]
    -> ParsedSentence
objectSymbolSentenceWithParametersAndArguments
  = symbolSentenceWithParametersAndArguments

symbolSentenceWithParametersAndArguments
    :: SymbolName
    -> [SortVariable]
    -> Sort
    -> [Sort]
    -> ParsedSentence
symbolSentenceWithParametersAndArguments
    (SymbolName name) params sort operandSorts
  = SentenceSymbolSentence
        SentenceSymbol
            { sentenceSymbolSymbol = Symbol
                { symbolConstructor = testId name
                , symbolParams = params
                }
            , sentenceSymbolSorts = operandSorts
            , sentenceSymbolResultSort = sort
            , sentenceSymbolAttributes =
                Attributes [] :: Attributes
            }

objectAliasSentenceWithArguments
    :: AliasName -> Sort -> [Variable] -> ParsedSentence
objectAliasSentenceWithArguments a b c =
    aliasSentenceWithArguments a b c (Parser.asPattern top')
  where
    top' = Parser.TopF Top { topSort = b }

aliasSentenceWithArguments
    :: AliasName
    -> Sort
    -> [Variable]
    -> ParsedPattern
    -> ParsedSentence
aliasSentenceWithArguments (AliasName name) sort operands r =
    SentenceAliasSentence SentenceAlias
        { sentenceAliasAlias = Alias
            { aliasConstructor = testId name
            , aliasParams = []
            }
        , sentenceAliasSorts =
            variableSort <$> operands
        , sentenceAliasResultSort = sort
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = testId name
                        , symbolOrAliasParams = []
                        }
                , applicationChildren = operands
                }
        , sentenceAliasRightPattern = r
        , sentenceAliasAttributes = Attributes []
        }

simpleSortActual :: SortName -> SortActual
simpleSortActual (SortName sort) =
    SortActual
        { sortActualName = testId sort
        , sortActualSorts = []
        }

simpleSort :: SortName -> Sort
simpleSort sortName =
    SortActualSort (simpleSortActual sortName)

objectVariableSort :: Text -> Sort
objectVariableSort name = sortVariableSort name

unifiedSortVariable :: Object -> SortVariableName -> SortVariable
unifiedSortVariable _x (SortVariableName name) = sortVariable name

stringUnifiedPattern :: Text -> TermLike Variable
stringUnifiedPattern s = (mkStringLiteral s)

variable :: VariableName -> Sort -> Variable
variable (VariableName name) sort =
    Variable
        { variableName = testId name
        , variableCounter = mempty
        , variableSort = sort
        }

unifiedVariable :: VariableName -> Sort -> Variable
unifiedVariable name sort =
    variable name sort

variablePattern :: VariableName -> Sort -> Pattern level domain Variable p
variablePattern name sort =
    VariablePattern (variable name sort)

unifiedVariablePattern :: VariableName -> Sort -> TermLike Variable
unifiedVariablePattern name patternSort =
    asPurePattern (valid :< variablePattern name patternSort)
  where
    freeVariables = Set.singleton (variable name patternSort)
    valid = Valid { patternSort, freeVariables }

simpleExistsPatternF
    :: Variable
    -> Sort
    -> Parser.PatternF Variable (Parser.Pattern Variable)
simpleExistsPatternF quantifiedVariable resultSort =
    Parser.ExistsF Exists
        { existsSort = resultSort
        , existsVariable = quantifiedVariable
        , existsChild = (Parser.asPattern . Parser.VariableF) quantifiedVariable
        }

simpleExistsPattern
    :: VariableName -> Sort -> Parser.Pattern Variable
simpleExistsPattern name sort =
    Parser.asPattern $ simpleExistsPatternF (variable name sort) sort

simpleExistsEqualsUnifiedPattern
    :: VariableName
    -> OperandSort level
    -> ResultSort level
    -> TermLike Variable
simpleExistsEqualsUnifiedPattern
    (VariableName name)
    (OperandSort operandSort)
    (ResultSort resultSort)
  =
    mkExists var
    $ mkEquals resultSort variablePattern' variablePattern'
  where
    variablePattern' = mkVar var
    var =
        Variable
            { variableName = testId name
            , variableCounter = mempty
            , variableSort = operandSort
            }

applicationObjectUnifiedPatternWithChildren
    :: SymbolName -> [ParsedPattern] -> ParsedPattern
applicationObjectUnifiedPatternWithChildren name unifiedPatterns =
    asParsedPattern (applicationPatternWithChildren name unifiedPatterns)

applicationPatternWithChildren
    :: SymbolName
    -> [child]
    -> Parser.PatternF variable child
applicationPatternWithChildren (SymbolName name) unifiedPatterns =
    Parser.ApplicationF Application
        { applicationSymbolOrAlias = SymbolOrAlias
            { symbolOrAliasConstructor = testId name
            , symbolOrAliasParams = []
            }
        , applicationChildren = unifiedPatterns
        }

applicationUnifiedPatternWithParams
    :: Sort
    -> SymbolName
    -> [Sort]
    -> TermLike Variable
applicationUnifiedPatternWithParams resultSort (SymbolName name) params =
    mkApp
        resultSort
        SymbolOrAlias
            { symbolOrAliasConstructor = testId name
            , symbolOrAliasParams = params
            }
        []
