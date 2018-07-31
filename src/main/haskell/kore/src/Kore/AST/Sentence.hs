{-|
Module      : Kore.AST.Common
Description : Data Structures for representing the Kore language AST that do not
              need unified constructs (see Kore.AST.Kore for the unified
              ones).
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module includes all the data structures necessary for representing
the syntactic categories of a Kore definition that do not need unified
constructs.

Unified constructs are those that represent both meta and object versions of
an AST term in a single data type (e.g. 'UnifiedSort' that can be either
'Sort Object' or 'Sort Meta')

Please refer to Section 9 (The Kore Language) of
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>.
-}
module Kore.AST.Sentence
    (
      -- * Attributes
      Attributes (..)
    , AnnotatedAttributes (..)
    , annotateAttributes
    , unannotateAttributes
      -- * Aliases
    , SentenceAlias (..)
    , KoreSentenceAlias
    , AnnotatedSentenceAlias (..)
    , AnnotatedKoreSentenceAlias
    , unannotateSentenceAlias
      -- * Symbols
    , SentenceSymbol (..)
    , KoreSentenceSymbol
    , AnnotatedSentenceSymbol (..)
    , AnnotatedKoreSentenceSymbol
    , unannotateSentenceSymbol
      -- * Imports
    , ModuleName (..)
    , SentenceImport (..)
    , KoreSentenceImport
    , AnnotatedSentenceImport (..)
    , AnnotatedKoreSentenceImport
    , unannotateSentenceImport
      -- * Sorts
    , SentenceSort (..)
    , KoreSentenceSort
    , AnnotatedSentenceSort (..)
    , AnnotatedKoreSentenceSort
    , unannotateSentenceSort
      -- * Axioms
    , SentenceAxiom (..)
    , KoreSentenceAxiom
    , AnnotatedSentenceAxiom (..)
    , AnnotatedKoreSentenceAxiom
    , unannotateSentenceAxiom
      -- * Hooks
    , SentenceHook (..)
    , KoreSentenceHook
    , AnnotatedSentenceHook (..)
    , AnnotatedKoreSentenceHook
    , unannotateSentenceHook
      -- * Sentences
    , Sentence (..)
    , UnifiedSentence (..)
    , constructUnifiedSentence
    , applyUnifiedSentence
    , SentenceSymbolOrAlias (..)
    , AsSentence (..)
    , KoreSentence
    , AnnotatedSentence (..)
    , AnnotatedKoreSentence
    , unannotateSentence
      -- * Modules
    , Module (..)
    , KoreModule
      -- * Definitions
    , Definition (..)
    , KoreDefinition
    ) where

import Control.Comonad.Cofree
       ( Cofree )
import Control.Comonad.Trans.Cofree
       ( CofreeF ((:<)) )
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Maybe
       ( catMaybes )

import           Data.Functor.Impredicative
                 ( Rotate41 (..) )
import           Data.Text.Prettyprint.Doc.Orphans ()
import           Kore.AST.Common
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.AST.Pretty
                 ( Pretty (..), (<+>), (<>) )
import qualified Kore.AST.Pretty as Pretty

{- | Kore attributes.

'Attributes' corresponds to the @attribute@ syntactic category in
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>,
Section 9.1.5 (Attributes).

See also: 'AnnotatedAttributes'

-}

newtype Attributes =
    Attributes { getAttributes :: [CommonKorePattern] }

deriving instance Eq Attributes

deriving instance Show Attributes

instance Pretty Attributes where
    pretty = Pretty.attributes . getAttributes

{- | Kore attributes with annotations.

See also: 'Attributes', 'AnnotatedCommonKorePattern'

-}
newtype AnnotatedAttributes annotation =
    AnnotatedAttributes
    { getAnnotatedAttributes :: [AnnotatedCommonKorePattern annotation] }

deriving instance Eq annotation => Eq (AnnotatedAttributes annotation)

deriving instance Show annotation => Show (AnnotatedAttributes annotation)

instance Pretty (AnnotatedAttributes annotation) where
    pretty =
          Pretty.attributes
        . map unannotateKorePattern
        . getAnnotatedAttributes

unannotateAttributes :: AnnotatedAttributes annotation -> Attributes
unannotateAttributes =
    Attributes . map unannotateKorePattern . getAnnotatedAttributes

annotateAttributes
    :: (UnifiedPattern Variable annotation -> annotation)
       -- ^ generate an annotation at a node, given the child annotations
    -> Attributes
       -- ^ 'Attributes' to annotate
    -> AnnotatedAttributes annotation
annotateAttributes annotate =
    AnnotatedAttributes . map (annotateKorePattern annotate) . getAttributes

{-| Alias declarations at the meta- and object-level.

'SentenceAlias' corresponds to the @object-alias-declaration@ and
@meta-alias-declaration@ syntactic categories in
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>,
Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.

-}
data SentenceAlias level (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = SentenceAlias
    { sentenceAliasAlias        :: !(Alias level)
    , sentenceAliasSorts        :: ![Sort level]
    , sentenceAliasResultSort   :: !(Sort level)
    , sentenceAliasLeftPattern  :: !(Pattern level variable (Fix (pat variable)))
    , sentenceAliasRightPattern :: !(Pattern level variable (Fix (pat variable)))
    , sentenceAliasAttributes   :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq (variable level)
    )
    => Eq (SentenceAlias level pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show (variable level)
    )
    => Show (SentenceAlias level pat variable)

instance (Pretty (variable level), Pretty (Fix (pat variable))) =>
    Pretty (SentenceAlias level pat variable) where
    pretty SentenceAlias {..} =
        Pretty.fillSep
        [ "alias"
        , pretty sentenceAliasAlias <> Pretty.arguments sentenceAliasSorts
        , ":"
        , pretty sentenceAliasResultSort
        , "where"
        , pretty sentenceAliasLeftPattern
        , ":="
        , pretty sentenceAliasRightPattern
        , pretty sentenceAliasAttributes
        ]

{-| Alias declarations at the meta- and object-level with annotations.

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.

See also: 'SentenceAlias'

-}
data AnnotatedSentenceAlias
    annotation
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    AnnotatedSentenceAlias
    { annotatedSentenceAliasAlias        :: !(Alias level)
    , annotatedSentenceAliasSorts        :: ![Sort level]
    , annotatedSentenceAliasResultSort   :: !(Sort level)
    , annotatedSentenceAliasLeftPattern  ::
        !(CofreeF
          (Pattern level variable)
          annotation
          (Cofree (pat variable) annotation))
    , annotatedSentenceAliasRightPattern ::
        !(CofreeF
          (Pattern level variable)
          annotation
          (Cofree (pat variable) annotation))
    , annotatedSentenceAliasAttributes   :: !(AnnotatedAttributes annotation)
    , annotatedSentenceAliasAnnotation   :: annotation
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq (variable level)
    , Eq annotation
    )
    => Eq (AnnotatedSentenceAlias annotation level pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show (variable level)
    , Show annotation
    )
    => Show (AnnotatedSentenceAlias annotation level pat variable)

instance
    ( Functor (pat variable)
    , Pretty (variable level), Pretty (Fix (pat variable))
    ) =>
    Pretty (AnnotatedSentenceAlias annotation level pat variable)
  where
    pretty = pretty . unannotateSentenceAlias

unannotateSentenceAlias
    :: Functor (pat variable)
    => AnnotatedSentenceAlias annotation level pat variable
    -> SentenceAlias level pat variable
unannotateSentenceAlias
    AnnotatedSentenceAlias
    { annotatedSentenceAliasAlias
    , annotatedSentenceAliasSorts
    , annotatedSentenceAliasResultSort
    , annotatedSentenceAliasLeftPattern
    , annotatedSentenceAliasRightPattern
    , annotatedSentenceAliasAttributes
    }
  =
    SentenceAlias
    { sentenceAliasAlias = annotatedSentenceAliasAlias
    , sentenceAliasSorts = annotatedSentenceAliasSorts
    , sentenceAliasResultSort = annotatedSentenceAliasResultSort
    , sentenceAliasLeftPattern =
        let
            _ :< _pat = annotatedSentenceAliasLeftPattern
        in
            unfold (\ann -> let _ :< _pat = project ann in _pat) <$> _pat
    , sentenceAliasRightPattern =
        let
            _ :< _pat = annotatedSentenceAliasRightPattern
        in
            unfold (\ann -> let _ :< _pat = project ann in _pat) <$> _pat
    , sentenceAliasAttributes = unannotateAttributes annotatedSentenceAliasAttributes
    }

{-|'SentenceSymbol' corresponds to the @object-symbol-declaration@ and
@meta-symbol-declaration@ syntactic categories from the Semantics of K,
Section 9.1.6 (Declaration and Definitions).

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.
-}
data SentenceSymbol level (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = SentenceSymbol
    { sentenceSymbolSymbol     :: !(Symbol level)
    , sentenceSymbolSorts      :: ![Sort level]
    , sentenceSymbolResultSort :: !(Sort level)
    , sentenceSymbolAttributes :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable) ) =>
    Eq (SentenceSymbol level pat variable)

deriving instance
    ( Show1 (pat variable) ) =>
    Show (SentenceSymbol level pat variable)

instance Pretty (Fix (pat variable)) =>
    Pretty (SentenceSymbol level pat variable) where
    pretty SentenceSymbol {..} =
        Pretty.fillSep
        [ "symbol"
        , pretty sentenceSymbolSymbol <> Pretty.arguments sentenceSymbolSorts
        , ":"
        , pretty sentenceSymbolResultSort
        , pretty sentenceSymbolAttributes
        ]

{- | Symbol declarations at the meta- and object-level with annotations.

See also: 'SentenceSymbol'

-}
data AnnotatedSentenceSymbol
    annotation
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    AnnotatedSentenceSymbol
    { annotatedSentenceSymbolSymbol     :: !(Symbol level)
    , annotatedSentenceSymbolSorts      :: ![Sort level]
    , annotatedSentenceSymbolResultSort :: !(Sort level)
    , annotatedSentenceSymbolAttributes :: !(AnnotatedAttributes annotation)
    , annotatedSentenceSymbolAnnotation :: !annotation
    }

deriving instance
    ( Eq annotation ) =>
    Eq (AnnotatedSentenceSymbol annotation level pat variable)

deriving instance
    ( Show annotation ) =>
    Show (AnnotatedSentenceSymbol annotation level pat variable)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (AnnotatedSentenceSymbol annotation level pat variable)
  where
    pretty = pretty . unannotateSentenceSymbol

unannotateSentenceSymbol
    :: AnnotatedSentenceSymbol annotation level pat variable
    -> SentenceSymbol level pat variable
unannotateSentenceSymbol
    AnnotatedSentenceSymbol
    { annotatedSentenceSymbolSymbol
    , annotatedSentenceSymbolSorts
    , annotatedSentenceSymbolResultSort
    , annotatedSentenceSymbolAttributes
    }
  =
    SentenceSymbol
    { sentenceSymbolSymbol = annotatedSentenceSymbolSymbol
    , sentenceSymbolSorts = annotatedSentenceSymbolSorts
    , sentenceSymbolResultSort = annotatedSentenceSymbolResultSort
    , sentenceSymbolAttributes =
        unannotateAttributes annotatedSentenceSymbolAttributes
    }

{-|'ModuleName' corresponds to the @module-name@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
newtype ModuleName = ModuleName { getModuleName :: String }
    deriving (Show, Eq, Ord)

instance Pretty ModuleName where
    pretty = Pretty.fromString . getModuleName

{-|'SentenceImport' corresponds to the @import-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
data SentenceImport (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = SentenceImport
    { sentenceImportModuleName :: !ModuleName
    , sentenceImportAttributes :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable) ) =>
    Eq (SentenceImport pat variable)

deriving instance
    ( Show1 (pat variable) ) =>
    Show (SentenceImport pat variable)

instance Pretty (Fix (pat variable)) =>
    Pretty (SentenceImport pat variable) where
    pretty SentenceImport {..} =
        Pretty.fillSep
        [ "import", pretty sentenceImportModuleName
        , pretty sentenceImportAttributes
        ]

{- | Import declarations with annotations.

See also: 'SentenceImport'

-}
data AnnotatedSentenceImport
    annotation
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    AnnotatedSentenceImport
    { annotatedSentenceImportModuleName :: !ModuleName
    , annotatedSentenceImportAttributes :: !(AnnotatedAttributes annotation)
    , annotatedSentenceImportAnnotation :: !annotation
    }

deriving instance
    ( Eq annotation ) =>
    Eq (AnnotatedSentenceImport annotation pat variable)

deriving instance
    ( Show annotation ) =>
    Show (AnnotatedSentenceImport annotation pat variable)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (AnnotatedSentenceImport annotation pat variable)
  where
    pretty = pretty . unannotateSentenceImport

unannotateSentenceImport
    :: AnnotatedSentenceImport annotation pat variable
    -> SentenceImport pat variable
unannotateSentenceImport
    AnnotatedSentenceImport
    { annotatedSentenceImportModuleName
    , annotatedSentenceImportAttributes
    }
  =
    SentenceImport
    { sentenceImportModuleName = annotatedSentenceImportModuleName
    , sentenceImportAttributes =
        unannotateAttributes annotatedSentenceImportAttributes
    }

{-|'SentenceSort' corresponds to the @sort-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
data SentenceSort level (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = SentenceSort
    { sentenceSortName       :: !(Id level)
    , sentenceSortParameters :: ![SortVariable level]
    , sentenceSortAttributes :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable) ) =>
    Eq (SentenceSort level pat variable)

deriving instance
    ( Show1 (pat variable) ) =>
    Show (SentenceSort level pat variable)

instance Pretty (Fix (pat variable)) =>
    Pretty (SentenceSort level pat variable) where
    pretty SentenceSort {..} =
        Pretty.fillSep
        [ "sort"
        , pretty sentenceSortName <> Pretty.parameters sentenceSortParameters
        , pretty sentenceSortAttributes
        ]

{- | Sort declarations with annotations.

See also: 'SentenceSort'

-}
data AnnotatedSentenceSort
    annotation
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    AnnotatedSentenceSort
    { annotatedSentenceSortName       :: !(Id level)
    , annotatedSentenceSortParameters :: ![SortVariable level]
    , annotatedSentenceSortAttributes :: !(AnnotatedAttributes annotation)
    , annotatedSentenceSortAnnotation :: !annotation
    }

deriving instance
    ( Eq annotation ) =>
    Eq (AnnotatedSentenceSort annotation level pat variable)

deriving instance
    ( Show annotation ) =>
    Show (AnnotatedSentenceSort annotation level pat variable)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (AnnotatedSentenceSort annotated level pat variable) where
    pretty = pretty . unannotateSentenceSort

unannotateSentenceSort
    :: AnnotatedSentenceSort annotation level pat variable
    -> SentenceSort level pat variable
unannotateSentenceSort
    AnnotatedSentenceSort
    { annotatedSentenceSortName
    , annotatedSentenceSortParameters
    , annotatedSentenceSortAttributes
    }
  =
    SentenceSort
    { sentenceSortName = annotatedSentenceSortName
    , sentenceSortParameters = annotatedSentenceSortParameters
    , sentenceSortAttributes =
        unannotateAttributes annotatedSentenceSortAttributes
    }

{-|'SentenceAxiom' corresponds to the @axiom-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
-}
data SentenceAxiom sortParam (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = SentenceAxiom
    { sentenceAxiomParameters :: ![sortParam]
    , sentenceAxiomPattern    :: !(Fix (pat variable))
    , sentenceAxiomAttributes :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq sortParam
    )  => Eq (SentenceAxiom sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show sortParam
    ) => Show (SentenceAxiom sortParam pat variable)

instance
    ( Pretty param
    , Pretty (Fix (pat variable))
    ) => Pretty (SentenceAxiom param pat variable)
  where
    pretty SentenceAxiom {..} =
        Pretty.fillSep
        [ "axiom"
        , Pretty.parameters sentenceAxiomParameters
        , pretty sentenceAxiomPattern
        , pretty sentenceAxiomAttributes
        ]

{- | Axiom declarations with annotations.

See also: 'SentenceAxiom'

-}
data AnnotatedSentenceAxiom
    annotation
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    AnnotatedSentenceAxiom
    { annotatedSentenceAxiomParameters :: ![sortParam]
    , annotatedSentenceAxiomPattern    :: !(Cofree (pat variable) annotation)
    , annotatedSentenceAxiomAttributes :: !(AnnotatedAttributes annotation)
    , annotatedSentenceAxiomAnnotation :: !annotation
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq sortParam
    , Eq annotation
    )  => Eq (AnnotatedSentenceAxiom annotation sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show sortParam
    , Show annotation
    ) => Show (AnnotatedSentenceAxiom annotation sortParam pat variable)

instance
    ( Pretty param
    , Pretty (Fix (pat variable))
    , Functor (pat variable)
    ) => Pretty (AnnotatedSentenceAxiom annotation param pat variable)
  where
    pretty = pretty . unannotateSentenceAxiom

unannotateSentenceAxiom
    :: Functor (pat variable)
    => AnnotatedSentenceAxiom annotation param pat variable
    -> SentenceAxiom param pat variable
unannotateSentenceAxiom
    AnnotatedSentenceAxiom
    { annotatedSentenceAxiomParameters
    , annotatedSentenceAxiomPattern
    , annotatedSentenceAxiomAttributes
    }
  =
    SentenceAxiom
    { sentenceAxiomParameters = annotatedSentenceAxiomParameters
    , sentenceAxiomPattern =
        unfold (\ann -> let _ :< pat = project ann in pat) annotatedSentenceAxiomPattern
    , sentenceAxiomAttributes =
        unannotateAttributes annotatedSentenceAxiomAttributes
    }

{-|@SentenceHook@ corresponds to @hook-declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
Note that we are reusing the 'SentenceSort' and 'SentenceSymbol' structures to
represent hooked sorts and hooked symbols.
-}
data SentenceHook level (pat :: (* -> *) -> * -> *) (variable :: * -> *)
    = SentenceHookedSort !(SentenceSort level pat variable)
    | SentenceHookedSymbol !(SentenceSymbol level pat variable)

deriving instance
    ( Eq1 (pat variable) ) =>
    Eq (SentenceHook level pat variable)

deriving instance
    ( Show1 (pat variable) ) =>
    Show (SentenceHook level pat variable)

instance
    Pretty (Fix (pat variable) )
    => Pretty (SentenceHook level pat variable)
  where
    pretty (SentenceHookedSort a)   = "hooked-" <> pretty a
    pretty (SentenceHookedSymbol a) = "hooked-" <> pretty a

{-| Hook declarations with annotations.

See also: 'SentenceHook'

-}
data AnnotatedSentenceHook
    annotation
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
      AnnotatedSentenceHookedSort
        !(AnnotatedSentenceSort annotation level pat variable)
    | AnnotatedSentenceHookedSymbol
        !(AnnotatedSentenceSymbol annotation level pat variable)

deriving instance
    ( Eq annotation ) =>
    Eq (AnnotatedSentenceHook annotation level pat variable)

deriving instance
    ( Show annotation ) =>
    Show (AnnotatedSentenceHook annotation level pat variable)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (AnnotatedSentenceHook annotation level pat variable)
  where
    pretty = pretty . unannotateSentenceHook

unannotateSentenceHook
    :: AnnotatedSentenceHook annotation level pat variable
    -> SentenceHook level pat variable
unannotateSentenceHook (AnnotatedSentenceHookedSort ann) =
    SentenceHookedSort (unannotateSentenceSort ann)
unannotateSentenceHook (AnnotatedSentenceHookedSymbol ann) =
    SentenceHookedSymbol (unannotateSentenceSymbol ann)

{-|The 'Sentence' type corresponds to the @declaration@ syntactic category
from the Semantics of K, Section 9.1.6 (Declaration and Definitions).

The @symbol-declaration@ and @alias-declaration@ categories were also merged
into 'Sentence', using the @level@ parameter to distinguish the 'Meta' and
'Object' variants.
Since axioms and imports exist at both meta and kore levels, we use 'Meta'
to qualify them. In contrast, since sort declarations are not available
at the meta level, we qualify them with 'Object'.
-}
data Sentence level sortParam (pat :: (* -> *) -> * -> *) (variable :: * -> *) where
    SentenceAliasSentence
        :: !(SentenceAlias level pat variable)
        -> Sentence level sortParam pat variable
    SentenceSymbolSentence
        :: !(SentenceSymbol level pat variable)
        -> Sentence level sortParam pat variable
    SentenceImportSentence
        :: !(SentenceImport pat variable)
        -> Sentence Meta sortParam pat variable
    SentenceAxiomSentence
        :: !(SentenceAxiom sortParam pat variable)
        -> Sentence Meta sortParam pat variable
    SentenceSortSentence
        :: !(SentenceSort level pat variable)
        -> Sentence level sortParam pat variable
    SentenceHookSentence
        :: !(SentenceHook Object pat variable)
        -> Sentence Object sortParam pat variable

deriving instance
    ( Eq1 (pat variable)
    , Eq sortParam
    , Eq (variable level)
    ) => Eq (Sentence level sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show sortParam
    , Show (variable level)
    ) => Show (Sentence level sortParam pat variable)

instance
    ( Pretty sortParam
    , Pretty (Fix (pat variable))
    , Pretty (variable level)
    ) => Pretty (Sentence level sortParam pat variable)
  where
    pretty (SentenceAliasSentence s)  = pretty s
    pretty (SentenceSymbolSentence s) = pretty s
    pretty (SentenceImportSentence s) = pretty s
    pretty (SentenceAxiomSentence s)  = pretty s
    pretty (SentenceSortSentence s)   = pretty s
    pretty (SentenceHookSentence s)   = pretty s

{- | Kore declarations with annotations.

See also: 'Sentence'

-}
data AnnotatedSentence
    annotation
    level
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  where
    AnnotatedSentenceAliasSentence
        :: !(AnnotatedSentenceAlias annotation level pat variable)
        -> AnnotatedSentence annotation level sortParam pat variable
    AnnotatedSentenceSymbolSentence
        :: !(AnnotatedSentenceSymbol annotation level pat variable)
        -> AnnotatedSentence annotation level sortParam pat variable
    AnnotatedSentenceImportSentence
        :: !(AnnotatedSentenceImport annotation pat variable)
        -> AnnotatedSentence annotation Meta sortParam pat variable
    AnnotatedSentenceAxiomSentence
        :: !(AnnotatedSentenceAxiom annotation sortParam pat variable)
        -> AnnotatedSentence annotation Meta sortParam pat variable
    AnnotatedSentenceSortSentence
        :: !(AnnotatedSentenceSort annotation level pat variable)
        -> AnnotatedSentence annotation level sortParam pat variable
    AnnotatedSentenceHookSentence
        :: !(AnnotatedSentenceHook annotation Object pat variable)
        -> AnnotatedSentence annotation Object sortParam pat variable

deriving instance
    ( Eq1 (pat variable)
    , Eq sortParam
    , Eq (variable level)
    , Eq annotation
    ) => Eq (AnnotatedSentence annotation level sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show sortParam
    , Show (variable level)
    , Show annotation
    ) => Show (AnnotatedSentence annotation level sortParam pat variable)

instance
    ( Pretty sortParam
    , Pretty (Fix (pat variable))
    , Pretty (variable level)
    , Functor (pat variable)
    ) => Pretty (AnnotatedSentence annotation level sortParam pat variable)
  where
    pretty = pretty . unannotateSentence

unannotateSentence
    :: Functor (pat variable)
    => AnnotatedSentence annotation level param pat variable
    -> Sentence level param pat variable
unannotateSentence (AnnotatedSentenceHookSentence ann) =
    SentenceHookSentence (unannotateSentenceHook ann)
unannotateSentence (AnnotatedSentenceAliasSentence ann) =
    SentenceAliasSentence (unannotateSentenceAlias ann)
unannotateSentence (AnnotatedSentenceSymbolSentence ann) =
    SentenceSymbolSentence (unannotateSentenceSymbol ann)
unannotateSentence (AnnotatedSentenceImportSentence ann) =
    SentenceImportSentence (unannotateSentenceImport ann)
unannotateSentence (AnnotatedSentenceAxiomSentence ann) =
    SentenceAxiomSentence (unannotateSentenceAxiom ann)
unannotateSentence (AnnotatedSentenceSortSentence ann) =
    SentenceSortSentence (unannotateSentenceSort ann)

{-|A 'Module' consists of a 'ModuleName' a list of 'Sentence's and some
'Attributes'.

They correspond to the second, third and forth non-terminals of the @definition@
syntactic category from the Semantics of K, Section 9.1.6
(Declaration and Definitions).
-}
data Module sentence sortParam (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = Module
    { moduleName       :: !ModuleName
    , moduleSentences  :: ![sentence sortParam pat variable]
    , moduleAttributes :: !Attributes
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq (sentence sortParam pat variable)
    ) => Eq (Module sentence sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show (sentence sortParam pat variable)
    ) => Show (Module sentence sortParam pat variable)

instance
    ( Pretty (sentence sort pat variable)
    , Pretty (Fix (pat variable))
    ) => Pretty (Module sentence sort pat variable)
  where
    pretty Module {..} =
        (Pretty.vsep . catMaybes)
        [ Just ("module" <+> pretty moduleName)
        , case moduleSentences of
            [] -> Nothing
            _ -> Just ((Pretty.indent 4 . Pretty.vsep) (pretty <$> moduleSentences))
        , Just "endmodule"
        , Just (pretty moduleAttributes)
        ]

{-|Currently, a 'Definition' consists of some 'Attributes' and a 'Module'

Because there are plans to extend this to a list of 'Module's, the @definition@
syntactic category from the Semantics of K, Section 9.1.6
(Declaration and Definitions) is splitted here into 'Definition' and 'Module'.

'definitionAttributes' corresponds to the first non-terminal of @definition@,
while the remaining three are grouped into 'definitionModules'.
-}
data Definition sentence sortParam (pat :: (* -> *) -> * -> *) (variable :: * -> *)
 = Definition
    { definitionAttributes :: !Attributes
    , definitionModules    :: ![Module sentence sortParam pat variable]
    }

deriving instance
    ( Eq1 (pat variable)
    , Eq (sentence sortParam pat variable)
    ) => Eq (Definition sentence sortParam pat variable)

deriving instance
    ( Show1 (pat variable)
    , Show (sentence sortParam pat variable)
    ) => Show (Definition sentence sortParam pat variable)

instance
    ( Pretty (sentence sort pat variable)
    , Pretty (Fix (pat variable))
    ) => Pretty (Definition sentence sort pat variable)
  where
    pretty Definition {..} =
        Pretty.vsep (pretty definitionAttributes : map pretty definitionModules)

class SentenceSymbolOrAlias (sentence :: * -> ((* -> *) -> * -> *) -> (* -> *) -> *) where
    getSentenceSymbolOrAliasConstructor
        :: sentence level pat variable -> Id level
    getSentenceSymbolOrAliasSortParams
        :: sentence level pat variable -> [SortVariable level]
    getSentenceSymbolOrAliasArgumentSorts
        :: sentence level pat variable -> [Sort level]
    getSentenceSymbolOrAliasResultSort
        :: sentence level pat variable -> Sort level
    getSentenceSymbolOrAliasAttributes
        :: sentence level pat variable -> Attributes
    getSentenceSymbolOrAliasSentenceName
        :: sentence level pat variable -> String
    getSentenceSymbolOrAliasHead
        :: sentence level pat variable -> [Sort level] -> SymbolOrAlias level
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
type KoreSentenceAlias level = SentenceAlias level UnifiedPattern Variable

-- |'KoreSentenceSymbol' is the Kore ('Meta' and 'Object') version of
-- 'SentenceSymbol'
type KoreSentenceSymbol level = SentenceSymbol level UnifiedPattern Variable

-- |'KoreSentenceImport' is the Kore ('Meta' and 'Object') version of
-- 'SentenceImport'
type KoreSentenceImport = SentenceImport UnifiedPattern Variable

-- |'KoreSentenceAxiom' is the Kore ('Meta' and 'Object') version of
-- 'SentenceAxiom'
type KoreSentenceAxiom = SentenceAxiom UnifiedSortVariable UnifiedPattern Variable

-- |'KoreSentenceSort' is the Kore ('Meta' and 'Object') version of
-- 'SentenceSort'
type KoreSentenceSort level = SentenceSort level UnifiedPattern Variable

-- |'KoreSentenceHook' Kore ('Meta' and 'Object') version of
-- 'SentenceHook'
type KoreSentenceHook = SentenceHook Object UnifiedPattern Variable

type AnnotatedKoreSentenceAlias annotation level =
    AnnotatedSentenceAlias annotation level UnifiedPattern Variable

type AnnotatedKoreSentenceSymbol annotation level =
    AnnotatedSentenceSymbol annotation level UnifiedPattern Variable

type AnnotatedKoreSentenceImport annotation =
    AnnotatedSentenceImport annotation UnifiedPattern Variable

type AnnotatedKoreSentenceAxiom annotation =
    AnnotatedSentenceAxiom annotation UnifiedSortVariable UnifiedPattern Variable

type AnnotatedKoreSentenceSort annotation level =
    AnnotatedSentenceSort annotation level UnifiedPattern Variable

type AnnotatedKoreSentenceHook annotation =
    AnnotatedSentenceHook annotation Object UnifiedPattern Variable

{-|'UnifiedPattern' is joining the 'Meta' and 'Object' versions of 'Sentence',
to allow using toghether both 'Meta' and 'Object' sentences.
-}
newtype UnifiedSentence sortParam pat variable = UnifiedSentence
    { getUnifiedSentence :: Unified (Rotate41 Sentence sortParam pat variable) }

deriving instance
    ( Eq1 (pat variable)
    , Eq sortParam
    , EqMetaOrObject variable
    ) => Eq (UnifiedSentence sortParam pat variable)

deriving instance
    ( Show1 (pat variable), Show (pat variable (Fix (pat variable)))
    , Show sortParam
    , ShowMetaOrObject variable
    ) => Show (UnifiedSentence sortParam pat variable)

instance
    ( Pretty sortParam
    , Pretty (Fix (pat variable))
    , Pretty (variable Meta)
    , Pretty (variable Object)
    ) => Pretty (UnifiedSentence sortParam pat variable)
  where
    pretty = applyUnifiedSentence pretty pretty


-- |'KoreSentence' instantiates 'UnifiedSentence' to describe sentences fully
-- corresponding to the @declaration@ syntactic category
-- from the Semantics of K, Section 9.1.6 (Declaration and Definitions).
type KoreSentence = UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

type AnnotatedKoreSentence = UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

constructUnifiedSentence
    :: (MetaOrObject level)
    => (a -> Sentence level sortParam pat variable)
    -> (a -> UnifiedSentence sortParam pat variable)
constructUnifiedSentence ctor = UnifiedSentence . asUnified . Rotate41 . ctor

-- |Given functions appliable to 'Meta' 'Sentence's and 'Object' 'Sentences's,
-- builds a combined function which can be applied on 'UnifiedSentence's.
applyUnifiedSentence
    :: (Sentence Meta sortParam pat variable -> b)
    -> (Sentence Object sortParam pat variable -> b)
    -> (UnifiedSentence sortParam pat variable -> b)
applyUnifiedSentence metaT _ (UnifiedSentence (UnifiedMeta rs)) =
    metaT (unRotate41 rs)
applyUnifiedSentence _ objectT (UnifiedSentence (UnifiedObject rs)) =
    objectT (unRotate41 rs)


-- |'KoreModule' fully instantiates 'Module' to correspond to the second, third,
-- and forth non-terminals of the @definition@ syntactic category from the
-- Semantics of K, Section 9.1.6 (Declaration and Definitions).
type KoreModule =
    Module UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

type KoreDefinition =
    Definition UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

instance
    ( MetaOrObject level
    ) => AsSentence KoreSentence (KoreSentenceAlias level)
  where
    asSentence = constructUnifiedSentence SentenceAliasSentence

instance
    ( MetaOrObject level
    ) => AsSentence KoreSentence (KoreSentenceSymbol level)
  where
    asSentence = constructUnifiedSentence SentenceSymbolSentence

instance AsSentence KoreSentence KoreSentenceImport where
    asSentence = constructUnifiedSentence SentenceImportSentence

instance AsSentence KoreSentence KoreSentenceAxiom where
    asSentence = constructUnifiedSentence SentenceAxiomSentence

instance
  ( MetaOrObject level
  ) => AsSentence KoreSentence (KoreSentenceSort level) where
    asSentence = constructUnifiedSentence SentenceSortSentence

instance AsSentence KoreSentence KoreSentenceHook where
    asSentence = constructUnifiedSentence SentenceHookSentence
