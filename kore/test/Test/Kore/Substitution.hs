module Test.Kore.Substitution where

import           Kore.AST.Pure
import qualified Kore.Domain.Builtin as Domain

import Test.Kore

objectSort :: Sort Object
objectSort = SortVariableSort (SortVariable (testId "s"))

objectVariable :: Variable Object
objectVariable = Variable
    { variableName = testId "v"
    , variableSort = objectSort
    , variableCounter = mempty
    }

objectVariablePattern
    :: Pattern Object Domain.Builtin Variable ParsedPattern
objectVariablePattern = VariablePattern objectVariable

objectVariableUnifiedPattern :: ParsedPattern
objectVariableUnifiedPattern = asParsedPattern objectVariablePattern

metaSort :: Sort Meta
metaSort = SortVariableSort (SortVariable (testId "#s"))

metaVariable :: Variable Meta
metaVariable = Variable
    { variableName = testId "#v"
    , variableSort = metaSort
    , variableCounter = mempty
    }

metaVariablePattern :: Pattern Meta Domain.Builtin Variable ParsedPattern
metaVariablePattern = VariablePattern metaVariable

metaVariableUnifiedPattern :: ParsedPattern
metaVariableUnifiedPattern = asParsedPattern metaVariablePattern

objectTopPattern :: ParsedPattern
objectTopPattern = asParsedPattern $ TopPattern $ Top objectSort

objectBottomPattern :: ParsedPattern
objectBottomPattern = asParsedPattern $ BottomPattern $ Bottom objectSort
