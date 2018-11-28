{-|
Module      : Kore.MetaML.AST
Description : Data Structures for representing a Meta-only version of the
              Kore language AST
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module specializes the 'Kore.AST.Pure' datastructures for
representing definitions, modules, axioms, patterns that only use 'Meta'-level
constructs.

Please refer to Section 9 (The Kore Language) of the
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf Semantics of K>.
-}
module Kore.MetaML.AST where

import           Control.Comonad.Trans.Cofree
                 ( CofreeF (..) )
import           Data.Set
                 ( Set )
import qualified Data.Text as Text

import           Kore.AST.Pure
import           Kore.AST.Sentence
import qualified Kore.Domain.Builtin as Domain
import           Kore.Variables.Free

{-|'MetaMLPattern' corresponds to "fixed point" representations
of the 'Pattern' class where the level is fixed to 'Meta'.

'var' is the type of variables.
-}
type MetaMLPattern variable = PurePattern Meta Domain.Builtin variable

-- |'MetaSentenceAxiom' is the 'Meta'-only version of 'SentenceAxiom'
type MetaSentenceAxiom = PureSentenceAxiom Meta Domain.Builtin
-- |'MetaSentenceAlias' is the 'Meta'-only version of 'SentenceAlias'
type MetaSentenceAlias = PureSentenceAlias Meta Domain.Builtin
-- |'MetaSentenceSymbol' is the 'Meta'-only version of 'SentenceSymbol'
type MetaSentenceSymbol = PureSentenceSymbol Meta Domain.Builtin
-- |'MetaSentenceImport' is the 'Meta'-only version of 'SentenceImport'
type MetaSentenceImport = PureSentenceImport Meta Domain.Builtin

-- |'MetaSentence' is the 'Meta'-only version of 'Sentence'
type MetaSentence = PureSentence Meta Domain.Builtin

-- |'MetaModule' is the 'Meta'-only version of 'Module'.
type MetaModule = PureModule Meta Domain.Builtin

-- |'MetaDefinition' is the 'Meta'-only version of 'Definition'.
type MetaDefinition = PureDefinition Meta Domain.Builtin

-- |'CommonMetaPattern' is the instantiation of 'MetaPattern' with common
-- 'Variable's.
type CommonMetaPattern = MetaMLPattern Variable ()

asCommonMetaPattern
    :: Pattern Meta Domain.Builtin Variable CommonMetaPattern
    -> CommonMetaPattern
asCommonMetaPattern = asPurePattern . (mempty :<)

type PatternMetaType =
    Pattern Meta Domain.Builtin Variable CommonMetaPattern

type MetaPatternStub =
    PatternStub Meta Domain.Builtin Variable CommonMetaPattern

-- |'metaFreeVariables' collects the free variables of a 'CommonMetaPattern'.
metaFreeVariables :: CommonMetaPattern -> Set (Variable Meta)
metaFreeVariables = freePureVariables

nilSortListHead :: SymbolOrAlias Meta
nilSortListHead = groundHead "#nilSortList" AstLocationImplicit

consSortListHead :: SymbolOrAlias Meta
consSortListHead = groundHead "#consSortList" AstLocationImplicit

nilSortListMetaPattern :: MetaMLPattern v ()
nilSortListMetaPattern = asPurePattern $ () :< constant nilSortListHead

nilPatternListHead :: SymbolOrAlias Meta
nilPatternListHead = groundHead "#nilPatternList" AstLocationImplicit

consPatternListHead :: SymbolOrAlias Meta
consPatternListHead = groundHead "#consPatternList" AstLocationImplicit

nilPatternListMetaPattern :: MetaMLPattern v ()
nilPatternListMetaPattern = asPurePattern $ () :< constant nilPatternListHead

variableHead :: SymbolOrAlias Meta
variableHead = groundHead "#variable" AstLocationImplicit

variableAsPatternHead :: SymbolOrAlias Meta
variableAsPatternHead = groundHead "#variableAsPattern" AstLocationImplicit

metaMLPatternHead :: MLPatternType -> AstLocation -> SymbolOrAlias Meta
metaMLPatternHead pt = groundHead (Text.pack $ '#' : '\\' : patternString pt)

sortDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
sortDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#sortDeclared" AstLocationImplicit
    , symbolOrAliasParams = [param]
    }

provableHead :: Sort Meta -> SymbolOrAlias Meta
provableHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#provable" AstLocationImplicit
    , symbolOrAliasParams = [param]
    }

sortsDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
sortsDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#sortsDeclared" AstLocationImplicit
    , symbolOrAliasParams = [param]
    }

symbolDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
symbolDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#symbolDeclared" AstLocationImplicit
    , symbolOrAliasParams = [param]
    }

sortHead :: SymbolOrAlias Meta
sortHead = groundHead "#sort" AstLocationImplicit

symbolHead :: SymbolOrAlias Meta
symbolHead = groundHead "#symbol" AstLocationImplicit

applicationHead :: SymbolOrAlias Meta
applicationHead = groundHead "#application" AstLocationImplicit
