{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Kore.Substitution.Class (test_class) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( assertEqual, testCase )

import Test.Kore
import Test.Kore.Substitution

import           Kore.AST.Common
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.Substitution.Class
import qualified Kore.Substitution.List as S
import           Kore.Variables.Fresh

type UnifiedPatternSubstitution =
    S.Substitution (Unified Variable) CommonKorePattern

instance
    PatternSubstitutionClass
        S.Substitution Variable UnifiedPattern
  where

testSubstitute
    :: CommonKorePattern
    -> UnifiedPatternSubstitution
    -> Counting CommonKorePattern
testSubstitute = substitute

test_class :: [TestTree]
test_class =
    [ testCase "Testing substituting a variable."
        (assertEqual ""
            (objectTopPattern, Counter 2)
            (runCounting
                (testSubstitute objectVariableUnifiedPattern substitution1)
                (Counter 2)
            )
        )
    , testCase "Testing not substituting a variable."
        (assertEqual ""
            (metaVariableUnifiedPattern, Counter 2)
            (runCounting
                (testSubstitute metaVariableUnifiedPattern substitution1)
                (Counter 2)
            )
        )
    , testCase "Testing not substituting anything."
        (assertEqual ""
            (objectBottomPattern, Counter 2)
            (runCounting
                (testSubstitute objectBottomPattern substitution1)
                (Counter 2)
            )
        )
      , testCase "Testing exists => empty substitution."
        (assertEqual ""
            (existsObjectUnifiedPattern1, Counter 2)
            (runCounting
                (testSubstitute existsObjectUnifiedPattern1 substitution1)
                (Counter 2)
            )
        )
      , testCase "Testing forall."
        (assertEqual ""
            (forallObjectUnifiedPattern2, Counter 2)
            (runCounting
                (testSubstitute forallObjectUnifiedPattern1 substitution1)
                (Counter 2)
            )
        )
      , testCase "Testing binder renaming"
        (assertEqual ""
            (existsObjectUnifiedPattern1S 2, Counter 3)
            (runCounting
                (testSubstitute existsObjectUnifiedPattern1 substitution2)
                (Counter 2)
            )
        )
      , testCase "Testing binder renaming and substitution"
        (assertEqual ""
            (forallObjectUnifiedPattern1S3, Counter 6)
            (runCounting
                (testSubstitute forallObjectUnifiedPattern1 substitution3)
                (Counter 5)
            )
        )
      , testCase "Testing double binder renaming"
        (assertEqual ""
            (forallExistsObjectUnifiedPattern1S2, Counter 9)
            (runCounting
                (testSubstitute
                    forallExistsObjectUnifiedPattern1 substitution2)
                (Counter 7)
            )
        )
        , testCase "Testing double binder renaming 1"
        (assertEqual ""
            (forallExistsObjectUnifiedPattern2, Counter 17)
            (runCounting
                (testSubstitute
                    forallExistsObjectUnifiedPattern2 substitution1)
                (Counter 17)
            )
        )
        , testCase "Testing substitution state 1"
        (assertEqual ""
            (testSubstitutionStatePatternS3, Counter 18)
            (runCounting
                (testSubstitute
                    testSubstitutionStatePattern substitution3)
                (Counter 17)
            )
        )
        ]

metaVariableSubstitute :: Int -> Variable Meta
metaVariableSubstitute n =
    metaVariable
        { variableName = Id
            { getId = "#var_" ++ show n
            , idLocation = AstLocationGeneratedVariable
            }
        }

metaVariableUnifiedPatternSubstitute :: Int -> CommonKorePattern
metaVariableUnifiedPatternSubstitute =
    asKorePattern . VariablePattern . metaVariableSubstitute

objectVariableSubstitute :: Int -> Variable Object
objectVariableSubstitute n =
    objectVariable
        { variableName = Id
            { getId = "var_" ++ show n
            , idLocation = AstLocationGeneratedVariable
            }
        }

objectVariableUnifiedPatternSubstitute :: Int -> CommonKorePattern
objectVariableUnifiedPatternSubstitute =
    asKorePattern . VariablePattern . objectVariableSubstitute

substitution1 :: UnifiedPatternSubstitution
substitution1 = S.fromList
  [ (unifiedObjectVariable, objectTopPattern) ]

substitution2 :: UnifiedPatternSubstitution
substitution2 = S.fromList
  [ (unifiedMetaVariable, objectVariableUnifiedPattern) ]

substitution3 :: UnifiedPatternSubstitution
substitution3 = S.fromList
  [ (unifiedObjectVariable, metaVariableUnifiedPattern) ]

existsObjectUnifiedPattern1 :: CommonKorePattern
existsObjectUnifiedPattern1 = asKorePattern $ ExistsPattern Exists
    { existsSort = objectSort
    , existsVariable = objectVariable
    , existsChild = objectVariableUnifiedPattern
    }

existsMetaUnifiedPattern1 :: CommonKorePattern
existsMetaUnifiedPattern1 = asKorePattern $ ExistsPattern Exists
    { existsSort = metaSort
    , existsVariable = metaVariable
    , existsChild = metaVariableUnifiedPattern
    }

existsMetaUnifiedPattern1S3 :: CommonKorePattern
existsMetaUnifiedPattern1S3 = asKorePattern $ ExistsPattern Exists
    { existsSort = metaSort
    , existsVariable = metaVariableSubstitute 17
    , existsChild = metaVariableUnifiedPatternSubstitute 17
    }

existsObjectUnifiedPattern1S :: Int -> CommonKorePattern
existsObjectUnifiedPattern1S n = asKorePattern $ ExistsPattern Exists
    { existsSort = objectSort
    , existsVariable = objectVariableSubstitute n
    , existsChild = objectVariableUnifiedPatternSubstitute n
    }

forallObjectUnifiedPattern1 :: CommonKorePattern
forallObjectUnifiedPattern1 = asKorePattern $ ForallPattern Forall
    { forallSort = metaSort
    , forallVariable = metaVariable
    , forallChild = objectVariableUnifiedPattern
    }

forallObjectUnifiedPattern2 :: CommonKorePattern
forallObjectUnifiedPattern2 = asKorePattern $ ForallPattern Forall
    { forallSort = metaSort
    , forallVariable = metaVariable
    , forallChild = objectTopPattern
    }

forallObjectUnifiedPattern1S3 :: CommonKorePattern
forallObjectUnifiedPattern1S3 = asKorePattern $ ForallPattern Forall
    { forallSort = metaSort
    , forallVariable = metaVariableSubstitute 5
    , forallChild = metaVariableUnifiedPattern
    }

forallExistsObjectUnifiedPattern1 :: CommonKorePattern
forallExistsObjectUnifiedPattern1 = asKorePattern $ ForallPattern Forall
    { forallSort = objectSort
    , forallVariable = objectVariable
    , forallChild = existsObjectUnifiedPattern1
    }

forallExistsObjectUnifiedPattern2 :: CommonKorePattern
forallExistsObjectUnifiedPattern2 = asKorePattern $ ForallPattern Forall
    { forallSort = metaSort
    , forallVariable = metaVariable
    , forallChild = existsObjectUnifiedPattern1
    }

forallExistsObjectUnifiedPattern1S2 :: CommonKorePattern
forallExistsObjectUnifiedPattern1S2 = asKorePattern $ ForallPattern Forall
    { forallSort = objectSort
    , forallVariable = objectVariableSubstitute 7
    , forallChild = existsObjectUnifiedPattern1S 8
    }

testSubstitutionStatePattern :: CommonKorePattern
testSubstitutionStatePattern = asKorePattern $ ApplicationPattern Application
    { applicationSymbolOrAlias = SymbolOrAlias
        { symbolOrAliasConstructor = testId "sigma" :: Id Object
        , symbolOrAliasParams = []
        }
    , applicationChildren =
        [ existsObjectUnifiedPattern1
        , objectVariableUnifiedPattern
        , existsMetaUnifiedPattern1
        , metaVariableUnifiedPattern
        ]
    }

testSubstitutionStatePatternS3 :: CommonKorePattern
testSubstitutionStatePatternS3 = asKorePattern $ ApplicationPattern Application
    { applicationSymbolOrAlias = SymbolOrAlias
        { symbolOrAliasConstructor = testId "sigma" :: Id Object
        , symbolOrAliasParams = []
        }
    , applicationChildren =
        [ existsObjectUnifiedPattern1
        , metaVariableUnifiedPattern
        , existsMetaUnifiedPattern1S3
        , metaVariableUnifiedPattern
        ]
    }
