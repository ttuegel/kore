module Test.Kore.ASTVerifier.DefinitionVerifier.PatternVerifier
    ( test_patternVerifier
    , test_verifyBinder
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit

import qualified Data.List as List
import qualified Data.Set as Set

import           Kore.AST.AstWithLocation
import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.AST.Valid
import           Kore.ASTVerifier.PatternVerifier as PatternVerifier
import qualified Kore.Attribute.Hook as Attribute.Hook
import qualified Kore.Builtin as Builtin
import           Kore.Error
import           Kore.IndexedModule.Error
                 ( noSort )
import qualified Kore.Parser.Pattern as Parser

import           Test.Kore
import           Test.Kore.ASTVerifier.DefinitionVerifier as Helpers
import qualified Test.Kore.Builtin.Builtin as Builtin
import qualified Test.Kore.Builtin.Definition as Builtin

data PatternRestrict
    = NeedsInternalDefinitions
    | NeedsSortedParent

data TestPattern level = TestPattern
    { testPatternPattern
        :: !(Parser.PatternF Variable (Parser.Pattern Variable))
    , testPatternSort       :: !Sort
    , testPatternErrorStack :: !ErrorStack
    }

newtype VariableOfDeclaredSort level = VariableOfDeclaredSort (Variable)

testPatternErrorStackStrings :: TestPattern Object -> [String]
testPatternErrorStackStrings
    TestPattern {testPatternErrorStack = ErrorStack strings}
  =
    strings

testPatternUnifiedPattern :: TestPattern Object -> Parser.Pattern Variable
testPatternUnifiedPattern TestPattern { testPatternPattern } =
    Parser.asPattern testPatternPattern

test_patternVerifier :: [TestTree]
test_patternVerifier =
    [ expectSuccess "Simplest definition"
        (simpleDefinitionFromSentences (ModuleName "MODULE") [])
    , successTestsForObjectPattern "Simple object pattern"
        (simpleExistsPatternF objectVariable' objectSort)
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [objectSortSentence, anotherSortSentence]
        NeedsInternalDefinitions
    , successTestsForMetaPattern "Simple meta pattern"
        (simpleExistsPatternF metaVariable' metaSort1)
        (NamePrefix "#dummy")
        (TestedPatternSort metaSort1)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        -- TODO: Here we should be able to use NoRestrict,
        -- at least in some cases.
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Object pattern - sort not defined"
        (ExpectedErrorMessage $ noSort "ObjectSort")
        (ErrorStack
            [ "\\exists 'ObjectVariable' (<test data>)"
            , "\\exists 'ObjectVariable' (<test data>)"
            , "sort 'ObjectSort' (<test data>)"
            , "(<test data>)"
            ]
        )
        (Parser.ExistsF Exists
            { existsSort = anotherSort
            , existsVariable = anotherVariable
            , existsChild =
                Parser.asPattern
                $ simpleExistsPatternF objectVariable' objectSort
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort anotherSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [anotherSortSentence]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern
        "Object pattern - different variable sort"
        (ExpectedErrorMessage "The declared sort is different.")
        (ErrorStack
            [ "\\exists 'ObjectVariable' (<test data>)"
            , "variable 'ObjectVariable' (<test data>)"
            , "(<test data>, <test data>)"
            ]
        )
        (Parser.ExistsF Exists
            { existsSort = objectSort
            , existsVariable = objectVariable'
            , existsChild =
                (Parser.asPattern . Parser.VariableF) anotherVariable
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [objectSortSentence, anotherSortSentence]
        NeedsInternalDefinitions
    , successTestsForObjectPattern
        "Object pattern - sort variable defined"
        (simpleExistsPatternF objectVariableSortVariable objectSortVariableSort)
        (NamePrefix "dummy")
        (TestedPatternSort objectSortVariableSort)
        (SortVariablesThatMustBeDeclared [objectSortVariable])
        (DeclaredSort anotherSort)
        [anotherSortSentence]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern
        "Object pattern - sort variable not defined"
        (ExpectedErrorMessage
            "Sort variable 'ObjectSortVariable' not declared.")
        (ErrorStack
            [ "\\exists 'ObjectVariable' (<test data>)"
            , "\\exists 'ObjectVariable' (<test data>)"
            , "(<test data>)"
            ]
        )
        (Parser.ExistsF Exists
            { existsSort = objectSort
            , existsVariable = objectVariable'
            , existsChild =
                Parser.asPattern
                $ simpleExistsPatternF
                    objectVariableSortVariable
                    objectSortVariableSort
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [objectSortSentence, anotherSortSentence]
        NeedsInternalDefinitions
    , failureTestsForMetaPattern "Meta pattern - sort not defined"
        (ExpectedErrorMessage $ noSort "#InvalidMetaSort")
        (ErrorStack
            [ "\\exists '#MetaVariable' (<test data>)"
            , "sort '#InvalidMetaSort' (<test data>)"
            , "(<test data>)"
            ]
        )
        (simpleExistsPatternF metaVariable' invalidMetaSort)
        (NamePrefix "#dummy")
        (TestedPatternSort metaSort1)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Object pattern - sort not matched"
        (ExpectedErrorMessage
            "Expecting sort 'anotherSort2{}' but got 'ObjectSort{}'.")
        (ErrorStack
            [ "\\exists 'ObjectVariable' (<test data>)"
            , "(<test data>, <test data>)"
            ]
        )
        (simpleExistsPatternF objectVariable' anotherObjectSort2)
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        ]
        NeedsInternalDefinitions
    , failureTestsForMetaPattern "Meta pattern - sort not matched"
        (ExpectedErrorMessage
            "Expecting sort '#Char{}' but got '#String{}'.")
        (ErrorStack
            [ "\\exists '#MetaVariable' (<test data>)"
            , "(<test data>, <test data>)"
            ]
        )
        (simpleExistsPatternF metaVariable' anotherMetaSort2)
        (NamePrefix "#dummy")
        (TestedPatternSort metaSort1)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Application pattern - symbol"
        (applicationPatternWithChildren
            objectSymbolName
            [simpleExistsPattern objectVariableName anotherObjectSort2]
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , objectSymbolSentence
        ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Application pattern - alias"
        (applicationPatternWithChildren
            objectAliasNameAsSymbol
            [ simpleExistsPattern objectVariableName anotherObjectSort2]
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , objectAliasSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern
        "Application pattern - symbol not declared"
        (ExpectedErrorMessage "Symbol 'ObjectSymbol' not defined.")
        (ErrorStack
            [ "symbol or alias 'ObjectSymbol' (<test data>)"
            , "(<test data>)"
            ]
        )
        (applicationPatternWithChildren
            objectSymbolName
            [simpleExistsPattern objectVariableName anotherObjectSort2]
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        --, objectSymbolSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern
        "Application pattern - not enough arguments"
        (ExpectedErrorMessage "Expected 1 operands, but got 0.")
        (ErrorStack ["symbol or alias 'ObjectSymbol' (<test data>)"])
        (applicationPatternWithChildren objectSymbolName [])
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , objectSymbolSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Object pattern - too many arguments"
        (ExpectedErrorMessage "Expected 1 operands, but got 2.")
        (ErrorStack ["symbol or alias 'ObjectSymbol' (<test data>)"])
        (applicationPatternWithChildren
            objectSymbolName
            [ simpleExistsPattern objectVariableName anotherObjectSort2
            , simpleExistsPattern objectVariableName anotherObjectSort2
            ]
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , objectSymbolSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern
        "Object pattern alias - too many arguments"
        (ExpectedErrorMessage "Expected 1 operands, but got 2.")
        (ErrorStack ["symbol or alias 'ObjectAlias' (<test data>)"])
        (applicationPatternWithChildren
            objectAliasNameAsSymbol
            [ simpleExistsPattern objectVariableName anotherObjectSort2
            , simpleExistsPattern objectVariableName anotherObjectSort2
            ]
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , objectAliasSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForMetaPattern "Meta pattern - wrong argument count"
        (ExpectedErrorMessage "Expected 1 operands, but got 0.")
        (ErrorStack ["symbol or alias '#MetaSymbol' (<test data>)"])
        (applicationPatternWithChildren metaSymbolName [])
        (NamePrefix "#dummy")
        (TestedPatternSort metaSort1)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        [ metaSymbolSentence ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Application pattern - too few sorts"
        (ExpectedErrorMessage
            "Application uses less sorts than the declaration.")
        (ErrorStack ["symbol or alias 'ObjectSymbol' (<test data>)"])
        (Parser.ApplicationF Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = testId oneSortSymbolRawName
                , symbolOrAliasParams = []
                }
            , applicationChildren =
                [simpleExistsPattern objectVariableName anotherObjectSort2]
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , oneSortSymbolSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Application pattern - too many sorts"
        (ExpectedErrorMessage
            "Application uses more sorts than the declaration.")
        (ErrorStack ["symbol or alias 'ObjectSymbol' (<test data>)"])
        (Parser.ApplicationF Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = testId oneSortSymbolRawName
                , symbolOrAliasParams = [objectSort, objectSort]
                }
            , applicationChildren =
                [simpleExistsPattern objectVariableName anotherObjectSort2]
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        , anotherObjectSortSentence2
        , oneSortSymbolSentence
        ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Object pattern - unquantified variable"
        (Parser.VariableF objectVariable')
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence, anotherSortSentence ]
        NeedsInternalDefinitions
    , successTestsForMetaPattern "Meta pattern - unquantified variable"
        (Parser.VariableF metaVariable')
        (NamePrefix "#dummy")
        (TestedPatternSort metaSort1)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        NeedsInternalDefinitions
    , successTestsForMetaPattern "Simple string pattern"
        (Parser.StringLiteralF (StringLiteral "MetaString"))
        (NamePrefix "#dummy")
        (TestedPatternSort stringMetaSort)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        -- TODO: Here we should be able to use NoRestrict,
        -- at least in some cases.
        NeedsInternalDefinitions
    , successTestsForMetaPattern "Simple char pattern"
        (Parser.CharLiteralF (CharLiteral 'c'))
        (NamePrefix "#dummy")
        (TestedPatternSort charMetaSort)
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        -- TODO: Here we should be able to use NoRestrict,
        -- at least in some cases.
        NeedsInternalDefinitions
    , failureTestsForMetaPattern "String pattern - sort not matched"
        (ExpectedErrorMessage
            "Expecting sort '#Char{}' but got '#String{}'.")
        (ErrorStack
            [ "(<test data>, <implicitly defined entity>)" ]
        )
        (Parser.StringLiteralF (StringLiteral "MetaString"))
        (NamePrefix "#dummy")
        (TestedPatternSort (updateAstLocation charMetaSort AstLocationTest))
        (SortVariablesThatMustBeDeclared [])
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherMetaSort)
        (VariableOfDeclaredSort dummyMetaVariable)
        []
        -- TODO: Here we should be able to use NoRestrict,
        -- at least in some cases.
        NeedsSortedParent
    , successTestsForObjectPattern "Bottom pattern"
        (Parser.BottomF Bottom {bottomSort = objectSort})
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Top pattern"
        (Parser.TopF Top {topSort = objectSort})
        (NamePrefix "dummy")
        (TestedPatternSort objectSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort anotherSort)
        [ objectSortSentence
        , anotherSortSentence
        ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Domain value - INT.Int"
        (ExpectedErrorMessage
            "<string literal>:1:1:\n\
            \  |\n\
            \1 | abcd\n\
            \  | ^\n\
            \unexpected 'a'\n\
            \expecting '+', '-', or integer\n")
        (ErrorStack
            [ "\\dv (<test data>)"
            , "Verifying builtin sort 'INT.Int'"
            , "While parsing domain value"
            ]
        )
        (Parser.DomainValueF DomainValue
            { domainValueSort = intSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                    (StringLiteral "abcd")  -- Not a decimal integer
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort (updateAstLocation intSort AstLocationTest))
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort intSort)
        [ SentenceHookSentence intSortSentence ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Domain value - INT.Int - Negative"
        (Parser.DomainValueF DomainValue
            { domainValueSort = intSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "-256")
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort intSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort intSort)
        [ SentenceHookSentence intSortSentence ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Domain value - INT.Int - Positive (unsigned)"
        (Parser.DomainValueF DomainValue
            { domainValueSort = intSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "1024")
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort intSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort intSort)
        [ SentenceHookSentence intSortSentence ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Domain value - INT.Int - Positive (signed)"
        (Parser.DomainValueF DomainValue
            { domainValueSort = intSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "+128")
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort intSort)
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort intSort)
        [ SentenceHookSentence intSortSentence ]
        NeedsInternalDefinitions
    , failureTestsForObjectPattern "Domain value - BOOL.Bool"
        (ExpectedErrorMessage
            "<string literal>:1:1:\n\
            \  |\n\
            \1 | untrue\n\
            \  | ^^^^^\n\
            \unexpected \"untru\"\n\
            \expecting \"false\" or \"true\"\n")
        (ErrorStack
            [ "\\dv (<test data>)"
            , "Verifying builtin sort 'BOOL.Bool'"
            , "While parsing domain value"
            ]
        )
        (Parser.DomainValueF DomainValue
            { domainValueSort = boolSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "untrue")  -- Not a BOOL.Bool
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort (updateAstLocation boolSort AstLocationTest))
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort boolSort)
        [ SentenceHookSentence boolSortSentence ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Domain value - BOOL.Bool - true"
        (Parser.DomainValueF DomainValue
            { domainValueSort = boolSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "true")
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort (updateAstLocation boolSort AstLocationTest))
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort boolSort)
        [ SentenceHookSentence boolSortSentence ]
        NeedsInternalDefinitions
    , successTestsForObjectPattern "Domain value - BOOL.Bool - false"
        (Parser.DomainValueF DomainValue
            { domainValueSort = boolSort
            , domainValueChild =
                (Parser.asPattern . Parser.StringLiteralF)
                   (StringLiteral "false")
            }
        )
        (NamePrefix "dummy")
        (TestedPatternSort (updateAstLocation boolSort AstLocationTest))
        (SortVariablesThatMustBeDeclared [])
        (DeclaredSort boolSort)
        [ SentenceHookSentence boolSortSentence ]
        NeedsInternalDefinitions
    ]
  where
    objectSortName = SortName "ObjectSort"
    objectSort :: Sort
    objectSort = simpleSort objectSortName
    objectVariableName = VariableName "ObjectVariable"
    objectVariable' = variable objectVariableName objectSort
    objectSortSentence = simpleSortSentence objectSortName
    metaSort1 = updateAstLocation stringMetaSort AstLocationTest
    metaVariable' = variable (VariableName "#MetaVariable") metaSort1
    dummyMetaSort = updateAstLocation charMetaSort AstLocationTest
    dummyMetaVariable = variable (VariableName "#otherVariable") dummyMetaSort
    anotherSortName = SortName "anotherSort"
    anotherSort :: Sort
    anotherSort = simpleSort anotherSortName
    anotherVariable = variable objectVariableName anotherSort
    anotherSortSentence = simpleSortSentence anotherSortName
    anotherMetaSort = updateAstLocation stringMetaSort AstLocationTest
    anotherObjectSortName2 = SortName "anotherSort2"
    anotherObjectSort2 :: Sort
    anotherObjectSort2 = simpleSort anotherObjectSortName2
    anotherObjectSortSentence2 = simpleSortSentence anotherObjectSortName2
    invalidMetaSort :: Sort
    invalidMetaSort = simpleSort (SortName "#InvalidMetaSort")
    anotherMetaSort2 = updateAstLocation charMetaSort AstLocationTest
    objectSymbolName = SymbolName "ObjectSymbol"
    objectSymbolSentence =
        objectSymbolSentenceWithArguments
            objectSymbolName objectSort [anotherObjectSort2]
    metaSymbolName = SymbolName "#MetaSymbol"
    metaSymbolSentence =
        symbolSentenceWithArguments
            metaSymbolName metaSort1 [anotherMetaSort2]
    objectAliasName = AliasName "ObjectAlias"
    objectAliasNameAsSymbol = SymbolName "ObjectAlias"
    objectAliasSentence =
        objectAliasSentenceWithArguments
            objectAliasName
            objectSort
            [ Variable
                { variableName = testId "x"
                , variableCounter = mempty
                , variableSort = anotherObjectSort2
                }
            ]
    objectSortVariable = sortVariable "ObjectSortVariable"
    objectSortVariableSort :: Sort
    objectSortVariableSort = sortVariableSort "ObjectSortVariable"
    objectVariableSortVariable =
        variable objectVariableName objectSortVariableSort
    oneSortSymbolRawName = "ObjectSymbol"
    oneSortSymbolSentence =
        SentenceSymbolSentence SentenceSymbol
            { sentenceSymbolSymbol = Symbol
                { symbolConstructor = testId oneSortSymbolRawName
                , symbolParams = [objectSortVariable]
                }
            , sentenceSymbolSorts = [anotherObjectSort2]
            , sentenceSymbolResultSort = objectSort
            , sentenceSymbolAttributes = Attributes []
            }
    intSortName = SortName "Int"
    intSort :: Sort
    intSort = simpleSort intSortName
    intSortSentence :: ParsedSentenceHook
    intSortSentence =
        SentenceHookedSort SentenceSort
            { sentenceSortName = testId name
            , sentenceSortParameters = []
            , sentenceSortAttributes =
                Attributes [ Attribute.Hook.hookAttribute "INT.Int" ]
            }
      where
        SortName name = intSortName
    boolSortName = SortName "Int"
    boolSort :: Sort
    boolSort = simpleSort boolSortName
    boolSortSentence :: ParsedSentenceHook
    boolSortSentence =
        SentenceHookedSort SentenceSort
            { sentenceSortName = testId name
            , sentenceSortParameters = []
            , sentenceSortAttributes =
                Attributes [ Attribute.Hook.hookAttribute "BOOL.Bool" ]
            }
      where
        SortName name = boolSortName

test_verifyBinder :: [TestTree]
test_verifyBinder =
    [ testVerifyExists
    , testVerifyForall
    ]
  where
    context =
        PatternVerifier.Context
            { declaredVariables = PatternVerifier.emptyDeclaredVariables
            , declaredSortVariables = Set.empty
            , indexedModule = Builtin.indexedModule
            , builtinDomainValueVerifiers = mempty
            }
    testVerifyBinder name expect =
        testCase name $ do
            let
                original = Builtin.externalizePattern expect
                verifier = verifyStandalonePattern Nothing original
                Right actual = runPatternVerifier context verifier
            assertEqual "" expect actual
            assertEqual "" (extract expect) (extract actual)
    testVerifyExists =
        testVerifyBinder "verifyExists" expect
      where
        x = varS "x" Builtin.intSort
        expect = mkExists x (mkVar x)
    testVerifyForall =
        testVerifyBinder "verifyForall" expect
      where
        x = varS "x" Builtin.intSort
        expect = mkForall x (mkVar x)

dummyVariableAndSentences
    :: NamePrefix
    -> (Variable, [ParsedSentence])
dummyVariableAndSentences (NamePrefix namePrefix) =
    (dummyVariable, [simpleSortSentence dummySortName])
  where
    dummySortName = SortName (namePrefix <> "_OtherSort")
    dummySort' = simpleSort dummySortName
    dummyVariable =
        variable (VariableName (namePrefix <> "_OtherVariable")) dummySort'


successTestsForObjectPattern
    :: String
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> TestTree
successTestsForObjectPattern
    description
    testedPattern
    namePrefix
    testedSort
    sortVariables
    anotherSort
    sentences
    patternRestrict
  =
    successTestDataGroup description testData
  where
    (dummyVariable, dummySortSentences) =
        dummyVariableAndSentences namePrefix
    testData =
        genericPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            sortVariables
            anotherSort
            (VariableOfDeclaredSort dummyVariable)
            (dummySortSentences ++ sentences)
            patternRestrict
        ++ objectPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            anotherSort
            (dummySortSentences ++ sentences)
            patternRestrict

successTestsForMetaPattern
    :: String
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Meta
    -> SortVariablesThatMustBeDeclared Meta
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Meta
    -> VariableOfDeclaredSort Meta
    -> [ParsedSentence]
    -> PatternRestrict
    -> TestTree
successTestsForMetaPattern
    description
    testedPattern
    namePrefix
    testedSort
    sortVariables
    objectSortVariables
    anotherSort
    dummyVariable
    sentences
    patternRestrict
  =
    successTestDataGroup description testData
  where
    testData =
        genericPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            objectSortVariables
            anotherSort
            dummyVariable
            sentences
            patternRestrict

failureTestsForObjectPattern
    :: HasCallStack
    => String
    -> ExpectedErrorMessage
    -> ErrorStack
    -> (Parser.PatternF Variable) (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> TestTree
failureTestsForObjectPattern
    description
    errorMessage
    errorStackSuffix
    testedPattern
    namePrefix@(NamePrefix rawNamePrefix)
    testedSort
    sortVariables
    anotherSort
    sentences
    patternRestrict
  =
    failureTestDataGroup
        description
        errorMessage
        errorStackSuffix
        testData
  where
    dummySortName = SortName (rawNamePrefix <> "_OtherSort")
    dummySort' = simpleSort dummySortName
    dummyVariable =
        variable (VariableName (rawNamePrefix <> "_OtherVariable")) dummySort'
    dummySortSentence = simpleSortSentence dummySortName
    testData =
        genericPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            sortVariables
            anotherSort
            (VariableOfDeclaredSort dummyVariable)
            (dummySortSentence : sentences)
            patternRestrict
        ++ objectPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            anotherSort
            (dummySortSentence : sentences)
            patternRestrict

failureTestsForMetaPattern
    :: HasCallStack
    => String
    -> ExpectedErrorMessage
    -> ErrorStack
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Meta
    -> SortVariablesThatMustBeDeclared Meta
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Meta
    -> VariableOfDeclaredSort Meta
    -> [ParsedSentence]
    -> PatternRestrict
    -> TestTree
failureTestsForMetaPattern
    description
    errorMessage
    errorStackSuffix
    testedPattern
    namePrefix
    testedSort
    sortVariables
    objectSortVariables
    anotherSort
    dummyVariable
    sentences
    patternRestrict
  =
    failureTestDataGroup
        description
        errorMessage
        errorStackSuffix
        testData
  where
    testData =
        genericPatternInAllContexts
            testedPattern
            namePrefix
            testedSort
            sortVariables
            objectSortVariables
            anotherSort
            dummyVariable
            sentences
            patternRestrict

genericPatternInAllContexts
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Object
    -> VariableOfDeclaredSort Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> [TestData]
genericPatternInAllContexts
    testedPattern
    (NamePrefix namePrefix)
    (TestedPatternSort testedSort)
    sortVariables
    objectSortVariables
    (DeclaredSort anotherSort)
    dummyVariable
    sentences
    patternRestrict
  =
    patternsInAllContexts
        patternExpansion
        (NamePrefix namePrefix)
        sortVariables
        objectSortVariables
        (DeclaredSort anotherSort)
        sentences
        patternRestrict
  where
    patternExpansion =
        genericPatternInPatterns
            testedPattern
            anotherPattern
            (OperandSort testedSort)
            (Helpers.ResultSort anotherSort)
            dummyVariable
            (symbolFromSort testedSort)
            (aliasFromSort testedSort)
            patternRestrict
    anotherPattern =
        Parser.ExistsF Exists
            { existsSort = testedSort
            , existsVariable = anotherVariable
            , existsChild =
                Parser.asPattern (Parser.VariableF anotherVariable)
            }
    anotherVariable =
        Variable
            { variableName = testId (namePrefix <> "_anotherVar")
            , variableCounter = mempty
            , variableSort = testedSort
            }
    rawSymbolName = namePrefix <> "_anotherSymbol"
    rawAliasName = namePrefix <> "_anotherAlias"
    symbolFromSort sort =
        SymbolOrAlias
            { symbolOrAliasConstructor = testId rawSymbolName
            , symbolOrAliasParams = [sort]
            }
    aliasFromSort sort =
        SymbolOrAlias
            { symbolOrAliasConstructor = testId rawAliasName
            , symbolOrAliasParams = [sort]
            }

objectPatternInAllContexts
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> NamePrefix
    -> TestedPatternSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> [TestData]
objectPatternInAllContexts
    testedPattern
    (NamePrefix namePrefix)
    (TestedPatternSort testedSort)
    sortVariables
    (DeclaredSort anotherSort)
  =
    patternsInAllContexts
        patternExpansion
        (NamePrefix namePrefix)
        sortVariables
        sortVariables
        (DeclaredSort anotherSort)
  where
    patternExpansion =
        objectPatternInPatterns
            testedPattern
            anotherPattern
            (OperandSort testedSort)
    anotherPattern =
        Parser.ExistsF Exists
            { existsSort = testedSort
            , existsVariable = anotherVariable
            , existsChild = Parser.asPattern (Parser.VariableF anotherVariable)
            }
    anotherVariable =
        Variable
            { variableName = testId (namePrefix <> "_anotherVar")
            , variableCounter = mempty
            , variableSort = testedSort
            }

patternsInAllContexts
    :: [TestPattern Object]
    -> NamePrefix
    -> SortVariablesThatMustBeDeclared Object
    -> SortVariablesThatMustBeDeclared Object
    -> DeclaredSort Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> [TestData]
patternsInAllContexts
    patterns
    (NamePrefix namePrefix)
    sortVariables
    objectSortVariables
    (DeclaredSort anotherSort)
    sentences
    patternRestrict
  =
    map (\context -> context (List.head patterns)) contextExpansion
    ++ map (List.head contextExpansion) patterns
  where
    contextExpansion =
        testsForUnifiedPatternInTopLevelContext
            (NamePrefix (namePrefix <> "_piac"))
            (DeclaredSort anotherSort)
            sortVariables
            objectSortVariables
            ( symbolSentence
            : aliasSentence
            : sentences
            )
            patternRestrict
    rawSymbolName = namePrefix <> "_anotherSymbol"
    rawAliasName = namePrefix <> "_anotherAlias"
    rawSortVariableName = namePrefix <> "_sortVariable"
    symbolAliasSort = sortVariableSort rawSortVariableName
    symbolSentence =
        SentenceSymbolSentence SentenceSymbol
                { sentenceSymbolSymbol = Symbol
                    { symbolConstructor = testId rawSymbolName
                    , symbolParams = [SortVariable (testId rawSortVariableName)]
                    }
                , sentenceSymbolSorts = [symbolAliasSort]
                , sentenceSymbolResultSort = anotherSort
                , sentenceSymbolAttributes = Attributes []
                }
    aliasSentence :: ParsedSentence
    aliasSentence =
        let aliasConstructor = testId rawAliasName
            aliasParams = [SortVariable (testId rawSortVariableName)]
        in SentenceAliasSentence SentenceAlias
            { sentenceAliasAlias = Alias { aliasConstructor, aliasParams }
            , sentenceAliasSorts = [symbolAliasSort]
            , sentenceAliasResultSort = anotherSort
            , sentenceAliasLeftPattern =
                Application
                    { applicationSymbolOrAlias =
                        SymbolOrAlias
                            { symbolOrAliasConstructor = aliasConstructor
                            , symbolOrAliasParams =
                                SortVariableSort <$> aliasParams
                            }
                    , applicationChildren =
                        [ Variable
                            { variableName = testId "x"
                            , variableCounter = mempty
                            , variableSort = symbolAliasSort
                            }
                        ]
                    }
            , sentenceAliasRightPattern =
                (Parser.asPattern . Parser.TopF) (Top anotherSort)
            , sentenceAliasAttributes = Attributes []
            }

genericPatternInPatterns
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> OperandSort Object
    -> Helpers.ResultSort Object
    -> VariableOfDeclaredSort Object
    -> SymbolOrAlias
    -> SymbolOrAlias
    -> PatternRestrict
    -> [TestPattern Object]
genericPatternInPatterns
    testedPattern
    anotherPattern
    sort@(OperandSort testedSort)
    resultSort
    (VariableOfDeclaredSort dummyVariable)
    symbol
    alias
    patternRestrict
  =
    patternInQuantifiedPatterns testedPattern testedSort dummyVariable
    ++ patternInUnquantifiedGenericPatterns
        testedPattern anotherPattern sort resultSort
    ++ case patternRestrict of
        NeedsSortedParent -> []
        _ ->
            [ TestPattern
                { testPatternPattern = testedPattern
                , testPatternSort = testedSort
                , testPatternErrorStack = ErrorStack []
                }
            ]
    ++
        [ TestPattern
            { testPatternPattern =
                Parser.ApplicationF Application
                    { applicationSymbolOrAlias = symbol
                    , applicationChildren = [Parser.asPattern testedPattern]
                    }
            , testPatternSort = testedSort
            , testPatternErrorStack =
                ErrorStack
                    [ "symbol or alias '"
                        ++ getIdForError (symbolOrAliasConstructor symbol)
                        ++ "' (<test data>)"
                    ]
            }
        , TestPattern
            { testPatternPattern =
                Parser.ApplicationF Application
                    { applicationSymbolOrAlias = alias
                    , applicationChildren = [Parser.asPattern testedPattern]
                    }
            , testPatternSort = testedSort
            , testPatternErrorStack =
                ErrorStack
                    [ "symbol or alias '"
                        ++ getIdForError (symbolOrAliasConstructor alias)
                        ++ "' (<test data>)"
                    ]
            }
        ]

objectPatternInPatterns
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> OperandSort Object
    -> [TestPattern Object]
objectPatternInPatterns = patternInUnquantifiedObjectPatterns

patternInQuantifiedPatterns
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> Sort
    -> Variable
    -> [TestPattern Object]
patternInQuantifiedPatterns testedPattern testedSort quantifiedVariable =
    [ TestPattern
        { testPatternPattern =
            Parser.ExistsF Exists
                { existsSort = testedSort
                , existsVariable = quantifiedVariable
                , existsChild = Parser.asPattern testedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack =
            ErrorStack
                [ "\\exists '"
                    ++ getIdForError (variableName quantifiedVariable)
                    ++ "' (<test data>)"
                ]
        }
    , TestPattern
        { testPatternPattern =
            Parser.ForallF Forall
                { forallSort = testedSort
                , forallVariable = quantifiedVariable
                , forallChild = Parser.asPattern testedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack =
            ErrorStack
                [ "\\forall '"
                    ++ getIdForError (variableName quantifiedVariable)
                    ++ "' (<test data>)"
                ]
        }
    ]

patternInUnquantifiedGenericPatterns
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> OperandSort Object
    -> Helpers.ResultSort Object
    -> [TestPattern Object]
patternInUnquantifiedGenericPatterns
    testedPattern
    anotherPattern
    (OperandSort testedSort)
    (Helpers.ResultSort resultSort)
  =
    [ TestPattern
        { testPatternPattern =
            Parser.AndF And
                { andSort = testedSort
                , andFirst = testedUnifiedPattern
                , andSecond = anotherUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\and (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.AndF And
                { andSort = testedSort
                , andFirst = anotherUnifiedPattern
                , andSecond = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\and (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.CeilF Ceil
                { ceilOperandSort = testedSort
                , ceilResultSort = resultSort
                , ceilChild = testedUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\ceil (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.EqualsF Equals
                { equalsOperandSort = testedSort
                , equalsResultSort = resultSort
                , equalsFirst = testedUnifiedPattern
                , equalsSecond = anotherUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\equals (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.EqualsF Equals
                { equalsOperandSort = testedSort
                , equalsResultSort = resultSort
                , equalsFirst = anotherUnifiedPattern
                , equalsSecond = testedUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\equals (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.FloorF Floor
                { floorOperandSort = testedSort
                , floorResultSort = resultSort
                , floorChild = testedUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\floor (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.IffF Iff
                { iffSort = testedSort
                , iffFirst = testedUnifiedPattern
                , iffSecond = anotherUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\iff (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.IffF Iff
                { iffSort = testedSort
                , iffFirst = anotherUnifiedPattern
                , iffSecond = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\iff (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.ImpliesF Implies
                { impliesSort = testedSort
                , impliesFirst = testedUnifiedPattern
                , impliesSecond = anotherUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\implies (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.ImpliesF Implies
                { impliesSort = testedSort
                , impliesFirst = anotherUnifiedPattern
                , impliesSecond = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\implies (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.InF In
                { inOperandSort = testedSort
                , inResultSort = resultSort
                , inContainedChild = testedUnifiedPattern
                , inContainingChild = anotherUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\in (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.InF In
                { inOperandSort = testedSort
                , inResultSort = resultSort
                , inContainedChild = anotherUnifiedPattern
                , inContainingChild = testedUnifiedPattern
                }
        , testPatternSort = resultSort
        , testPatternErrorStack = ErrorStack ["\\in (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.NotF Not
                { notSort = testedSort
                , notChild = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\not (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.OrF Or
                { orSort = testedSort
                , orFirst = testedUnifiedPattern
                , orSecond = anotherUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\or (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.OrF Or
                { orSort = testedSort
                , orFirst = anotherUnifiedPattern
                , orSecond = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\or (<test data>)"]
        }
    ]
  where
    anotherUnifiedPattern = Parser.asPattern anotherPattern
    testedUnifiedPattern = Parser.asPattern testedPattern

patternInUnquantifiedObjectPatterns
    :: Parser.PatternF Variable (Parser.Pattern Variable)
    -> Parser.PatternF Variable (Parser.Pattern Variable)
    -> OperandSort Object
    -> [TestPattern Object]
patternInUnquantifiedObjectPatterns
    testedPattern
    anotherPattern
    (OperandSort testedSort)
  =
    [ TestPattern
        { testPatternPattern =
            Parser.NextF Next
                { nextSort = testedSort
                , nextChild = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\next (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.RewritesF Rewrites
                { rewritesSort = testedSort
                , rewritesFirst = testedUnifiedPattern
                , rewritesSecond = anotherUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\rewrites (<test data>)"]
        }
    , TestPattern
        { testPatternPattern =
            Parser.RewritesF Rewrites
                { rewritesSort = testedSort
                , rewritesFirst = anotherUnifiedPattern
                , rewritesSecond = testedUnifiedPattern
                }
        , testPatternSort = testedSort
        , testPatternErrorStack = ErrorStack ["\\rewrites (<test data>)"]
        }

    ]
  where
    anotherUnifiedPattern = Parser.asPattern anotherPattern
    testedUnifiedPattern = Parser.asPattern testedPattern

testsForUnifiedPatternInTopLevelContext
    :: NamePrefix
    -> DeclaredSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> SortVariablesThatMustBeDeclared Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> [TestPattern Object -> TestData]
testsForUnifiedPatternInTopLevelContext
    namePrefix
    additionalSort
    sortVariables
    _
  =
    testsForUnifiedPatternInTopLevelGenericContext
        namePrefix
        additionalSort
        sortVariables

testsForUnifiedPatternInTopLevelGenericContext
    :: NamePrefix
    -> DeclaredSort Object
    -> SortVariablesThatMustBeDeclared Object
    -> [ParsedSentence]
    -> PatternRestrict
    -> [TestPattern Object -> TestData]
testsForUnifiedPatternInTopLevelGenericContext
    (NamePrefix _)
    (DeclaredSort _)
    (SortVariablesThatMustBeDeclared sortVariables)
    additionalSentences
    patternRestrict
  =
    let
        axiomPattern testPattern = TestData
            { testDataDescription = "Pattern in axiom"
            , testDataError =
                Error
                    ( "module 'MODULE'"
                    : "axiom declaration"
                    : testPatternErrorStackStrings testPattern
                    )
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        (testPatternUnifiedPattern testPattern)
                        sortVariables
                    : additionalSentences
                    )
            }
    in case patternRestrict of
        NeedsInternalDefinitions -> [axiomPattern]
        NeedsSortedParent        -> [axiomPattern]

defaultErrorMessage :: String
defaultErrorMessage = "Replace this with a real error message."


    -- MLPatternType
    -- Application
    -- axiom
    -- attributes -- module and definition
