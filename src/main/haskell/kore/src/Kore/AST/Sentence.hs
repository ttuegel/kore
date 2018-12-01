{-|
Module      : Kore.AST.Sentence
Description : Data Structures for representing the Kore language AST that do not
              need unified constructs (see Kore.AST.Kore for the unified
              ones).
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module includes all the data structures necessary for representing
the syntactic categories of a Kore definition that do not need unified
constructs.

Unified constructs are those that represent both meta and object versions of
an AST term in a single data type (e.g. 'UnifiedSort' that can be either
'Sort Object' or 'Sort Meta')

Please refer to Section 9 (The Kore Language) of the
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf Semantics of K>.
-}

{-# LANGUAGE UndecidableInstances #-}

module Kore.AST.Sentence where

import           Control.DeepSeq
                 ( NFData (..) )
import           Data.Default
import           Data.Hashable
                 ( Hashable (..) )
import           Data.Proxy
import           Data.Text
                 ( Text )
import qualified Data.Text as Text
import           GHC.Generics
                 ( Generic )

import           Kore.AST.Kore
import           Kore.AST.Pure
import qualified Kore.Domain.Builtin as Domain

{-|'Symbol' corresponds to the
@object-head-constructor{object-sort-variable-list}@ part of the
@object-symbol-declaration@ and @meta-symbol-declaration@ syntactic categories
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.

Note that this is very similar to 'SymbolOrAlias'.
-}
data Symbol level = Symbol
    { symbolConstructor :: !(Id level)
    , symbolParams      :: ![SortVariable level]
    }
    deriving (Show, Eq, Ord, Generic)

instance Hashable (Symbol level)

instance NFData (Symbol level)

-- |Given an 'Id', 'groundSymbol' produces the unparameterized 'Symbol'
-- corresponding to that argument.
groundSymbol :: Id level -> Symbol level
groundSymbol ctor = Symbol
    { symbolConstructor = ctor
    , symbolParams = []
    }

{-|'Alias' corresponds to the
@object-head-constructor{object-sort-variable-list}@ part of the
@object-alias-declaration@ and @meta-alias-declaration@ syntactic categories
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.

Note that this is very similar to 'SymbolOrAlias'.
-}
data Alias level = Alias
    { aliasConstructor :: !(Id level)
    , aliasParams      :: ![SortVariable level]
    }
    deriving (Show, Eq, Ord, Generic)

instance Hashable (Alias level)

instance NFData (Alias level)

{-|'Attributes' corresponds to the @attributes@ Kore syntactic declaration.
It is parameterized by the types of Patterns, @pat@.
-}

newtype Attributes =
    Attributes { getAttributes :: [CommonKorePattern] }
    deriving (Eq, Ord, Generic, Show)

instance Hashable Attributes

instance NFData Attributes

instance Default Attributes where
    def = Attributes []

{-|'SentenceAlias' corresponds to the @object-alias-declaration@ and
@meta-alias-declaration@ syntactic categories from the Semantics of K,
Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.
-}
data SentenceAlias (lvl :: *) (pat :: *) =
    SentenceAlias
        { sentenceAliasAlias        :: !(Alias lvl)
        , sentenceAliasSorts        :: ![Sort lvl]
        , sentenceAliasResultSort   :: !(Sort lvl)
        , sentenceAliasLeftPattern  :: !(Application lvl pat)
        , sentenceAliasRightPattern :: !pat
        , sentenceAliasAttributes   :: !Attributes
        }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable pat => Hashable (SentenceAlias lvl pat)

instance NFData pat => NFData (SentenceAlias lvl pat)

{-|'SentenceSymbol' corresponds to the @object-symbol-declaration@ and
@meta-symbol-declaration@ syntactic categories from the Semantics of K,
Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.
-}
data SentenceSymbol (lvl :: *) (pat :: *) =
    SentenceSymbol
        { sentenceSymbolSymbol     :: !(Symbol lvl)
        , sentenceSymbolSorts      :: ![Sort lvl]
        , sentenceSymbolResultSort :: !(Sort lvl)
        , sentenceSymbolAttributes :: !Attributes
        }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable (SentenceSymbol lvl pat)

instance NFData (SentenceSymbol lvl pat)

{-|'ModuleName' corresponds to the @module-name@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
newtype ModuleName = ModuleName { getModuleName :: Text }
    deriving (Eq, Generic, Ord, Show)

instance Hashable ModuleName

instance NFData ModuleName

getModuleNameForError :: ModuleName -> String
getModuleNameForError = Text.unpack . getModuleName

{-|'SentenceImport' corresponds to the @import-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
-- TODO (thomas.tuegel): Even though the parameters are unused, they must stay
-- to satisfy the functional dependencies on 'AsSentence' below. Because they
-- are phantom, every use of 'asSentence' for a 'SentenceImport' will require a
-- type ascription. We should refactor the class so this is not necessary and
-- remove the parameters.
data SentenceImport (pat :: *)
  = SentenceImport
    { sentenceImportModuleName :: !ModuleName
    , sentenceImportAttributes :: !Attributes
    }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable (SentenceImport pat)

instance NFData (SentenceImport pat)

{-|'SentenceSort' corresponds to the @sort-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
data SentenceSort (level :: *) (pat :: *) =
    SentenceSort
        { sentenceSortName       :: !(Id level)
        , sentenceSortParameters :: ![SortVariable level]
        , sentenceSortAttributes :: !Attributes
        }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable (SentenceSort level pat)

instance NFData (SentenceSort level pat)

{-|'SentenceAxiom' corresponds to the @axiom-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
data SentenceAxiom (param :: *) (pat :: *) =
    SentenceAxiom
        { sentenceAxiomParameters :: ![param]
        , sentenceAxiomPattern    :: !pat
        , sentenceAxiomAttributes :: !Attributes
        }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance (Hashable param, Hashable pat) => Hashable (SentenceAxiom param pat)

instance (NFData param, NFData pat) => NFData (SentenceAxiom param pat)

{-|@SentenceHook@ corresponds to @hook-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
Note that we are reusing the 'SentenceSort' and 'SentenceSymbol' structures to
represent hooked sorts and hooked symbols.
-}
data SentenceHook (pat :: *) where
    SentenceHookedSort :: !(SentenceSort Object pat) -> SentenceHook pat
    SentenceHookedSymbol :: !(SentenceSymbol Object pat) -> SentenceHook pat
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable (SentenceHook pat)

instance NFData (SentenceHook pat)

{-|The 'Sentence' type corresponds to the @declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).

The @symbol-declaration@ and @alias-declaration@ categories were also merged
into 'Sentence', using the @level@ parameter to distinguish the 'Meta' and
'Object' variants.
Since axioms and imports exist at both meta and kore levels, we use 'Meta'
to qualify them. In contrast, since sort declarations are not available
at the meta level, we qualify them with 'Object'.
-}
data Sentence
    (lvl :: *)
    (param :: *)
    (pat :: *)
  where
    SentenceAliasSentence
        :: !(SentenceAlias level pat)
        -> Sentence level param pat
    SentenceSymbolSentence
        :: !(SentenceSymbol level pat)
        -> Sentence level param pat
    SentenceImportSentence
        :: !(SentenceImport pat)
        -> Sentence Meta param pat
    SentenceAxiomSentence
        :: !(SentenceAxiom param pat)
        -> Sentence Meta param pat
    SentenceClaimSentence
        :: !(SentenceAxiom param pat)
        -> Sentence Meta param pat
    SentenceSortSentence
        :: !(SentenceSort level pat)
        -> Sentence level param pat
    SentenceHookSentence
        :: !(SentenceHook pat)
        -> Sentence Object param pat

deriving instance (Eq param, Eq pat) => Eq (Sentence level param pat)

deriving instance Foldable (Sentence level param)

deriving instance Functor (Sentence level param)

deriving instance (Ord param, Ord pat) => Ord (Sentence level param pat)

deriving instance (Show param, Show pat) => Show (Sentence level param pat)

deriving instance Traversable (Sentence level param)

instance (NFData param, NFData pat) => NFData (Sentence level param pat) where
    rnf =
        \case
            SentenceAliasSentence p -> rnf p
            SentenceSymbolSentence p -> rnf p
            SentenceImportSentence p -> rnf p
            SentenceAxiomSentence p -> rnf p
            SentenceClaimSentence p -> rnf p
            SentenceSortSentence p -> rnf p
            SentenceHookSentence p -> rnf p

{- | The attributes associated with a sentence.

    Every sentence type has attributes, so this operation is total.

 -}
sentenceAttributes :: Sentence level param pat -> Attributes
sentenceAttributes =
    \case
        SentenceAliasSentence
            SentenceAlias { sentenceAliasAttributes } ->
                sentenceAliasAttributes
        SentenceSymbolSentence
            SentenceSymbol { sentenceSymbolAttributes } ->
                sentenceSymbolAttributes
        SentenceImportSentence
            SentenceImport { sentenceImportAttributes } ->
                sentenceImportAttributes
        SentenceAxiomSentence
            SentenceAxiom { sentenceAxiomAttributes } ->
                sentenceAxiomAttributes
        SentenceClaimSentence
            SentenceAxiom { sentenceAxiomAttributes } ->
                sentenceAxiomAttributes
        SentenceSortSentence
            SentenceSort { sentenceSortAttributes } ->
                sentenceSortAttributes
        SentenceHookSentence sentence ->
            case sentence of
                SentenceHookedSort
                    SentenceSort { sentenceSortAttributes } ->
                        sentenceSortAttributes
                SentenceHookedSymbol
                    SentenceSymbol { sentenceSymbolAttributes } ->
                        sentenceSymbolAttributes

{-|A 'Module' consists of a 'ModuleName' a list of 'Sentence's and some
'Attributes'.

They correspond to the second, third and forth non-terminals of the @definition@
syntactic category from the Semantics of K, Section 9.1.6
(Declaration and Definitions).
-}
data Module (sentence :: *) =
    Module
        { moduleName       :: !ModuleName
        , moduleSentences  :: ![sentence]
        , moduleAttributes :: !Attributes
        }
    deriving (Eq, Generic, Show)

instance Hashable sentence => Hashable (Module sentence)

instance NFData sentence => NFData (Module sentence)

{-|Currently, a 'Definition' consists of some 'Attributes' and a 'Module'

Because there are plans to extend this to a list of 'Module's, the @definition@
syntactic category from the Semantics of K, Section 9.1.6
(Declaration and Definitions) is splitted here into 'Definition' and 'Module'.

'definitionAttributes' corresponds to the first non-terminal of @definition@,
while the remaining three are grouped into 'definitionModules'.
-}
data Definition (sentence :: *) =
    Definition
        { definitionAttributes :: !Attributes
        , definitionModules    :: ![Module sentence]
        }
    deriving (Eq, Generic, Show)

instance Hashable sentence => Hashable (Definition sentence)

instance NFData sentence => NFData (Definition sentence)

class SentenceSymbolOrAlias (sentence :: * -> * -> *) where
    getSentenceSymbolOrAliasConstructor
        :: sentence level pat -> Id level
    getSentenceSymbolOrAliasSortParams
        :: sentence level pat -> [SortVariable level]
    getSentenceSymbolOrAliasArgumentSorts
        :: sentence level pat -> [Sort level]
    getSentenceSymbolOrAliasResultSort
        :: sentence level pat -> Sort level
    getSentenceSymbolOrAliasAttributes
        :: sentence level pat -> Attributes
    getSentenceSymbolOrAliasSentenceName
        :: sentence level pat -> String
    getSentenceSymbolOrAliasHead
        :: sentence level pat
        -> [Sort level]
        -> SymbolOrAlias level
    getSentenceSymbolOrAliasHead sentence sortParameters = SymbolOrAlias
        { symbolOrAliasConstructor =
            getSentenceSymbolOrAliasConstructor sentence
        , symbolOrAliasParams = sortParameters
        }

instance SentenceSymbolOrAlias SentenceAlias where
    getSentenceSymbolOrAliasConstructor = aliasConstructor . sentenceAliasAlias
    getSentenceSymbolOrAliasSortParams = aliasParams . sentenceAliasAlias
    getSentenceSymbolOrAliasArgumentSorts = sentenceAliasSorts
    getSentenceSymbolOrAliasResultSort = sentenceAliasResultSort
    getSentenceSymbolOrAliasAttributes = sentenceAliasAttributes
    getSentenceSymbolOrAliasSentenceName _ = "alias"

instance SentenceSymbolOrAlias SentenceSymbol where
    getSentenceSymbolOrAliasConstructor =
        symbolConstructor . sentenceSymbolSymbol
    getSentenceSymbolOrAliasSortParams = symbolParams . sentenceSymbolSymbol
    getSentenceSymbolOrAliasArgumentSorts = sentenceSymbolSorts
    getSentenceSymbolOrAliasResultSort = sentenceSymbolResultSort
    getSentenceSymbolOrAliasAttributes = sentenceSymbolAttributes
    getSentenceSymbolOrAliasSentenceName _ = "symbol"

class AsSentence sentenceType s | s -> sentenceType where
    asSentence :: s -> sentenceType

-- |'KoreSentenceAlias' is the Kore ('Meta' and 'Object') version of
-- 'SentenceAlias'
type KoreSentenceAlias level = SentenceAlias level CommonKorePattern

-- |'KoreSentenceSymbol' is the Kore ('Meta' and 'Object') version of
-- 'SentenceSymbol'
type KoreSentenceSymbol level = SentenceSymbol level CommonKorePattern

-- |'KoreSentenceImport' is the Kore ('Meta' and 'Object') version of
-- 'SentenceImport'
type KoreSentenceImport = SentenceImport CommonKorePattern

-- |'KoreSentenceAxiom' is the Kore ('Meta' and 'Object') version of
-- 'SentenceAxiom'
type KoreSentenceAxiom = SentenceAxiom UnifiedSortVariable CommonKorePattern

-- |'KoreSentenceSort' is the Kore ('Meta' and 'Object') version of
-- 'SentenceSort'
type KoreSentenceSort level = SentenceSort level CommonKorePattern

-- |'KoreSentenceHook' Kore ('Meta' and 'Object') version of
-- 'SentenceHook'
type KoreSentenceHook = SentenceHook CommonKorePattern

{-|'UnifiedPattern' is joining the 'Meta' and 'Object' versions of 'Sentence',
to allow using toghether both 'Meta' and 'Object' sentences.
-}
data UnifiedSentence param pat where
    UnifiedMetaSentence
        :: !(Sentence Meta param pat)
        -> UnifiedSentence param pat

    UnifiedObjectSentence
        :: !(Sentence Object param pat)
        -> UnifiedSentence param pat
    deriving (Eq, Ord, Show)

instance (NFData param, NFData pat) => NFData (UnifiedSentence param pat) where
    rnf =
        \case
            UnifiedMetaSentence metaS -> rnf metaS
            UnifiedObjectSentence objectS -> rnf objectS

-- |'KoreSentence' instantiates 'UnifiedSentence' to describe sentences fully
-- corresponding to the @declaration@ syntactic category
-- from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
type KoreSentence = UnifiedSentence UnifiedSortVariable CommonKorePattern

constructUnifiedSentence
    ::  forall a level param pat.
        MetaOrObject level
    => (a -> Sentence level param pat)
    -> (a -> UnifiedSentence param pat)
constructUnifiedSentence ctor =
    case isMetaOrObject (Proxy :: Proxy level) of
        IsMeta -> UnifiedMetaSentence . ctor
        IsObject -> UnifiedObjectSentence . ctor

-- |Given functions appliable to 'Meta' 'Sentence's and 'Object' 'Sentences's,
-- builds a combined function which can be applied on 'UnifiedSentence's.
applyUnifiedSentence
    :: (Sentence Meta param pat -> b)
    -> (Sentence Object param pat -> b)
    -> (UnifiedSentence param pat -> b)
applyUnifiedSentence metaT objectT =
    \case
        UnifiedMetaSentence metaS -> metaT metaS
        UnifiedObjectSentence objectS -> objectT objectS

-- |'KoreModule' fully instantiates 'Module' to correspond to the second, third,
-- and forth non-terminals of the @definition@ syntactic category from the
-- Semantics of K, Section 9.1.6 (Declaration and Definitions).
type KoreModule = Module KoreSentence

type KoreDefinition = Definition KoreSentence

instance
    ( MetaOrObject level
    , param ~ UnifiedSortVariable
    ) =>
    AsSentence
        (UnifiedSentence param (KorePattern dom var ann))
        (SentenceAlias level (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceAliasSentence

instance
    ( MetaOrObject level
    , param ~ UnifiedSortVariable
    ) =>
    AsSentence
        (UnifiedSentence param (KorePattern dom var ann))
        (SentenceSymbol level (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceSymbolSentence

instance
    AsSentence
        (UnifiedSentence UnifiedSortVariable (KorePattern dom var ann))
        (SentenceImport (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceImportSentence

instance
    (param ~ UnifiedSortVariable) =>
    AsSentence
        (UnifiedSentence param (KorePattern dom var ann))
        (SentenceAxiom param (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceAxiomSentence

instance
    ( MetaOrObject level
    , param ~ UnifiedSortVariable
    ) =>
    AsSentence
        (UnifiedSentence param (KorePattern dom var ann))
        (SentenceSort level (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceSortSentence


instance
    (param ~ UnifiedSortVariable) =>
    AsSentence
        (UnifiedSentence param (KorePattern dom var ann))
        (SentenceHook (KorePattern dom var ann))
  where
    asSentence = constructUnifiedSentence SentenceHookSentence

-- |'PureSentenceAxiom' is the pure (fixed-@level@) version of 'SentenceAxiom'
type PureSentenceAxiom level domain =
    SentenceAxiom (SortVariable level) (CommonPurePattern level domain ())

-- |'PureSentenceAlias' is the pure (fixed-@level@) version of 'SentenceAlias'
type PureSentenceAlias level domain =
    SentenceAlias level (CommonPurePattern level domain ())

-- |'PureSentenceSymbol' is the pure (fixed-@level@) version of 'SentenceSymbol'
type PureSentenceSymbol level domain =
    SentenceSymbol level (CommonPurePattern level domain ())

-- |'PureSentenceImport' is the pure (fixed-@level@) version of 'SentenceImport'
type PureSentenceImport level domain =
    SentenceImport (CommonPurePattern level domain ())

-- |'PureSentence' is the pure (fixed-@level@) version of 'Sentence'
type PureSentence level domain =
    Sentence level (SortVariable level) (CommonPurePattern level domain ())

instance
    ( MetaOrObject lvl
    , param ~ SortVariable lvl
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceAlias lvl (PurePattern lvl dom var ann))
  where
    asSentence = SentenceAliasSentence

instance
    ( MetaOrObject lvl
    , param ~ SortVariable lvl
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceSymbol lvl (PurePattern lvl dom var ann))
  where
    asSentence = SentenceSymbolSentence

instance
    ( param ~ SortVariable lvl
    , lvl ~ Meta
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceImport (PurePattern lvl dom var ann))
  where
    asSentence = SentenceImportSentence

instance
    ( lvl ~ Meta
    , param ~ SortVariable lvl
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceAxiom param (PurePattern lvl dom var ann))
  where
    asSentence = SentenceAxiomSentence

instance
    ( MetaOrObject lvl
    , param ~ SortVariable lvl
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceSort lvl (PurePattern lvl dom var ann))
  where
    asSentence = SentenceSortSentence


instance
    ( lvl ~ Object
    , param ~ SortVariable lvl
    ) =>
    AsSentence
        (Sentence lvl param (PurePattern lvl dom var ann))
        (SentenceHook (PurePattern lvl dom var ann))
  where
    asSentence = SentenceHookSentence

-- |'PureModule' is the pure (fixed-@level@) version of 'Module'
type PureModule level domain = Module (PureSentence level domain)

-- |'PureDefinition' is the pure (fixed-@level@) version of 'Definition'
type PureDefinition level domain = Definition (PureSentence level domain)
