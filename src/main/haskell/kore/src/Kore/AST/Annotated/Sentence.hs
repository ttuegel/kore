{-|
Module      : Kore.AST.Annotated.Sentence
Description : Annotated Kore definitions
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

Please refer to Section 9 (The Kore Language) of
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>.
-}
module Kore.AST.Annotated.Sentence
    (
      module Kore.AST.Sentence
      -- * Attributes
    , Attributes (..)
    , annotateAttributes
    , unannotateAttributes
      -- * Aliases
    , SentenceAlias (..)
    , KoreSentenceAlias
    , unannotateSentenceAlias
      -- * Symbols
    , SentenceSymbol (..)
    , KoreSentenceSymbol
    , unannotateSentenceSymbol
      -- * Imports
    , SentenceImport (..)
    , KoreSentenceImport
    , unannotateSentenceImport
      -- * Sorts
    , SentenceSort (..)
    , KoreSentenceSort
    , unannotateSentenceSort
      -- * Axioms
    , SentenceAxiom (..)
    , KoreSentenceAxiom
    , unannotateSentenceAxiom
      -- * Hooks
    , SentenceHook (..)
    , KoreSentenceHook
    , unannotateSentenceHook
      -- * Sentences
    , UnifiedSentence (..)
    , constructUnifiedSentence
    , applyUnifiedSentence
    , KoreSentence
    , Sentence (..)
    , unannotateSentence
    , unannotateUnifiedSentence
      -- * Modules
    , Module (..)
    , KoreModule
    , unannotateModule
      -- * Definitions
    , Definition (..)
    , KoreDefinition
    , unannotateDefinition
    ) where

import Control.Comonad.Trans.Cofree
       ( Cofree, CofreeF ((:<)), tailF )
import Control.DeepSeq
       ( NFData (..) )
import Control.DeepSeq.Orphans ()
import Data.Functor.Compose
       ( Compose (..) )
import Data.Functor.Foldable
import Data.Functor.Identity
       ( Identity (..) )
import Data.Maybe
       ( catMaybes )
import GHC.Generics
       ( Generic )

import           Data.Annotation
                 ( Annotation (..) )
import           Data.Functor.Impredicative
                 ( Rotate41 (..) )
import           Data.Text.Prettyprint.Doc.Orphans ()
import           Kore.AST.Annotated.Kore
import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.AST.Pretty
                 ( Pretty (..), (<+>) )
import qualified Kore.AST.Pretty as Pretty
import           Kore.AST.Sentence
                 ( AsSentence (..), ModuleName (..) )
import qualified Kore.AST.Sentence as Unannotated

{- | Annotated Kore attributes.

See also: 'Unannotated.Attributes', 'CommonKorePattern'

-}
newtype Attributes ann = Attributes { getAttributes :: [CommonKorePattern ann] }
  deriving (Eq, Generic, Show)

instance NFData ann => NFData (Attributes ann)

instance Pretty (Attributes annotation) where
    pretty =
          Pretty.attributes
        . map unannotateKorePattern
        . getAttributes

unannotateAttributes :: Attributes ann -> Unannotated.Attributes
unannotateAttributes =
    Unannotated.Attributes . map unannotateKorePattern . getAttributes

annotateAttributes
    :: (UnifiedPattern Variable ann -> ann)
       -- ^ generate an annotation at a node, given the child annotations
    -> Unannotated.Attributes
       -- ^ 'Unannotated.Attributes' to annotate
    -> Attributes ann
annotateAttributes annotate =
    Attributes . map (annotateKorePattern annotate) . Unannotated.getAttributes

{-| Alias declarations at the meta- and object-level with annotations.

The 'level' type parameter is used to distiguish between the meta- and object-
versions of symbol declarations. It should verify 'MetaOrObject level'.

See also: 'SentenceAlias'

-}
data SentenceAlias
    ann
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    SentenceAlias
    { sentenceAliasAlias        :: !(Alias level)
    , sentenceAliasSorts        :: ![Sort level]
    , sentenceAliasResultSort   :: !(Sort level)
    , sentenceAliasLeftPattern  ::
        !(CofreeF
          (Pattern level variable)
          (Annotation ann)
          (Cofree (pat variable) (Annotation ann)))
    , sentenceAliasRightPattern ::
        !(CofreeF
          (Pattern level variable)
          (Annotation ann)
          (Cofree (pat variable) (Annotation ann)))
    , sentenceAliasAttributes   :: !(Attributes ann)
    }
  deriving (Generic)

deriving instance
    ( Eq (var level)
    , Eq (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Eq (SentenceAlias ann level pat var)

deriving instance
    ( Show ann, Show (var level)
    , Show (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Show (SentenceAlias ann level pat var)

instance
    ( Functor (pat var)
    , NFData ann
    , NFData (var level)
    , NFData (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    NFData (SentenceAlias ann level pat var)

instance
    ( Functor (pat variable)
    , Pretty (variable level), Pretty (Fix (pat variable))
    ) =>
    Pretty (SentenceAlias ann level pat variable)
  where
    pretty = pretty . unannotateSentenceAlias

unannotateSentenceAlias
    :: Functor (pat variable)
    => SentenceAlias ann level pat variable
    -> Unannotated.SentenceAlias level pat variable
unannotateSentenceAlias
    SentenceAlias
    { sentenceAliasAlias
    , sentenceAliasSorts
    , sentenceAliasResultSort
    , sentenceAliasLeftPattern
    , sentenceAliasRightPattern
    , sentenceAliasAttributes
    }
  =
    Unannotated.SentenceAlias
    { sentenceAliasAlias = sentenceAliasAlias
    , sentenceAliasSorts = sentenceAliasSorts
    , sentenceAliasResultSort = sentenceAliasResultSort
    , sentenceAliasLeftPattern =
        let
            unannotate ann =
                let
                    Compose (Identity (_ :< pat)) = project ann
                in
                    pat
        in
            unfold unannotate <$> tailF sentenceAliasLeftPattern
    , sentenceAliasRightPattern =
        let
            unannotate ann =
                let
                    Compose (Identity (_ :< pat)) = project ann
                in
                    pat
        in
            unfold unannotate <$> tailF sentenceAliasRightPattern
    , sentenceAliasAttributes = unannotateAttributes sentenceAliasAttributes
    }

{- | Symbol declarations at the meta- and object-level with annotations.

See also: 'Unannotated.SentenceSymbol'

-}
data SentenceSymbol
    ann
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    SentenceSymbol
    { sentenceSymbolSymbol     :: !(Symbol level)
    , sentenceSymbolSorts      :: ![Sort level]
    , sentenceSymbolResultSort :: !(Sort level)
    , sentenceSymbolAttributes :: !(Attributes ann)
    }
  deriving (Eq, Generic, Show)

instance NFData ann => NFData (SentenceSymbol ann level pat var)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (SentenceSymbol ann level pat variable)
  where
    pretty = pretty . unannotateSentenceSymbol

unannotateSentenceSymbol
    :: SentenceSymbol ann level pat variable
    -> Unannotated.SentenceSymbol level pat variable
unannotateSentenceSymbol
    SentenceSymbol
    { sentenceSymbolSymbol
    , sentenceSymbolSorts
    , sentenceSymbolResultSort
    , sentenceSymbolAttributes
    }
  =
    Unannotated.SentenceSymbol
    { sentenceSymbolSymbol = sentenceSymbolSymbol
    , sentenceSymbolSorts = sentenceSymbolSorts
    , sentenceSymbolResultSort = sentenceSymbolResultSort
    , sentenceSymbolAttributes =
        unannotateAttributes sentenceSymbolAttributes
    }

{- | Import declarations with annotations.

See also: 'Unannotated.SentenceImport'

-}
data SentenceImport
    ann
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    SentenceImport
    { sentenceImportModuleName :: !ModuleName
    , sentenceImportAttributes :: !(Attributes ann)
    }
  deriving (Eq, Generic, Show)

instance NFData ann => NFData (SentenceImport ann pat var)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (SentenceImport ann pat variable)
  where
    pretty = pretty . unannotateSentenceImport

unannotateSentenceImport
    :: SentenceImport ann pat variable
    -> Unannotated.SentenceImport pat variable
unannotateSentenceImport
    SentenceImport
    { sentenceImportModuleName
    , sentenceImportAttributes
    }
  =
    Unannotated.SentenceImport
    { sentenceImportModuleName = sentenceImportModuleName
    , sentenceImportAttributes =
        unannotateAttributes sentenceImportAttributes
    }

{- | Sort declarations with annotations.

See also: 'SentenceSort'

-}
data SentenceSort
    ann
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    SentenceSort
    { sentenceSortName       :: !(Id level)
    , sentenceSortParameters :: ![SortVariable level]
    , sentenceSortAttributes :: !(Attributes ann)
    }
  deriving (Eq, Generic, Show)

instance NFData ann => NFData (SentenceSort ann level pat var)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (SentenceSort ann level pat variable)
  where
    pretty = pretty . unannotateSentenceSort

unannotateSentenceSort
    :: SentenceSort ann level pat variable
    -> Unannotated.SentenceSort level pat variable
unannotateSentenceSort
    SentenceSort
    { sentenceSortName
    , sentenceSortParameters
    , sentenceSortAttributes
    }
  =
    Unannotated.SentenceSort
    { sentenceSortName = sentenceSortName
    , sentenceSortParameters = sentenceSortParameters
    , sentenceSortAttributes =
        unannotateAttributes sentenceSortAttributes
    }

{- | Axiom declarations with annotations.

See also: 'Unannotated.SentenceAxiom'

-}
data SentenceAxiom
    ann
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    SentenceAxiom
    { sentenceAxiomParameters :: ![sortParam]
    , sentenceAxiomPattern    :: !(Cofree (pat variable) (Annotation ann))
    , sentenceAxiomAttributes :: !(Attributes ann)
    }
  deriving (Generic)

deriving instance
    ( Eq sorts
    , Eq (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Eq (SentenceAxiom ann sorts pat var)

deriving instance
    ( Show ann, Show sorts
    , Show (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Show (SentenceAxiom ann sorts pat var)

instance
    ( Functor (pat var)
    , NFData ann
    , NFData sorts
    , NFData (pat var (Cofree (pat var) (Annotation ann)))
    ) => NFData (SentenceAxiom ann sorts pat var)

instance
    ( Pretty param
    , Pretty (Fix (pat variable))
    , Functor (pat variable)
    ) => Pretty (SentenceAxiom ann param pat variable)
  where
    pretty = pretty . unannotateSentenceAxiom

unannotateSentenceAxiom
    :: Functor (pat variable)
    => SentenceAxiom ann param pat variable
    -> Unannotated.SentenceAxiom param pat variable
unannotateSentenceAxiom
    SentenceAxiom
    { sentenceAxiomParameters
    , sentenceAxiomPattern
    , sentenceAxiomAttributes
    }
  =
    Unannotated.SentenceAxiom
    { sentenceAxiomParameters = sentenceAxiomParameters
    , sentenceAxiomPattern =
        let
            unannotate ann =
                let Compose (Identity (_ :< pat)) = project ann
                in pat
        in
            unfold unannotate sentenceAxiomPattern
    , sentenceAxiomAttributes =
        unannotateAttributes sentenceAxiomAttributes
    }


{-| Hook declarations with annotations.

See also: 'SentenceHook'

-}
data SentenceHook
    ann
    level
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
      SentenceHookedSort !(SentenceSort ann level pat variable)
    | SentenceHookedSymbol !(SentenceSymbol ann level pat variable)
  deriving (Eq, Generic, Show)

instance NFData ann => NFData (SentenceHook ann level pat var)

instance
    ( Functor (pat variable)
    , Pretty (Fix (pat variable))
    ) =>
    Pretty (SentenceHook ann level pat variable)
  where
    pretty = pretty . unannotateSentenceHook

unannotateSentenceHook
    :: SentenceHook ann level pat variable
    -> Unannotated.SentenceHook level pat variable
unannotateSentenceHook (SentenceHookedSort ann) =
    Unannotated.SentenceHookedSort (unannotateSentenceSort ann)
unannotateSentenceHook (SentenceHookedSymbol ann) =
    Unannotated.SentenceHookedSymbol (unannotateSentenceSymbol ann)

{- | Kore declarations with annotations.

See also: 'Unannotated.Sentence'

-}
data Sentence
    ann
    level
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  where
    SentenceAliasSentence
        :: !(SentenceAlias ann level pat variable)
        -> Sentence ann level sortParam pat variable
    SentenceSymbolSentence
        :: !(SentenceSymbol ann level pat variable)
        -> Sentence ann level sortParam pat variable
    SentenceImportSentence
        :: !(SentenceImport ann pat variable)
        -> Sentence ann Meta sortParam pat variable
    SentenceAxiomSentence
        :: !(SentenceAxiom ann sortParam pat variable)
        -> Sentence ann Meta sortParam pat variable
    SentenceSortSentence
        :: !(SentenceSort ann level pat variable)
        -> Sentence ann level sortParam pat variable
    SentenceHookSentence
        :: !(SentenceHook ann Object pat variable)
        -> Sentence ann Object sortParam pat variable

deriving instance
    ( Eq sorts, Eq (var level)
    , Eq (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Eq (Sentence ann level sorts pat var)

deriving instance
    ( Show ann, Show sorts, Show (var level)
    , Show (pat var (Cofree (pat var) (Annotation ann)))
    ) =>
    Show (Sentence ann level sorts pat var)

instance
    ( Functor (pat var)
    , NFData ann
    , NFData sorts
    , NFData (var level)
    , NFData (pat var (Cofree (pat var) (Annotation ann)))
    ) => NFData (Sentence ann level sorts pat var)
  where
    rnf (SentenceHookSentence s) = rnf s
    rnf (SentenceAliasSentence s) = rnf s
    rnf (SentenceSymbolSentence s) = rnf s
    rnf (SentenceImportSentence s) = rnf s
    rnf (SentenceAxiomSentence s) = rnf s
    rnf (SentenceSortSentence s) = rnf s

instance
    ( Pretty sortParam
    , Pretty (Fix (pat variable))
    , Pretty (variable level)
    , Functor (pat variable)
    ) => Pretty (Sentence ann level sortParam pat variable)
  where
    pretty = pretty . unannotateSentence

unannotateSentence
    :: Functor (pat variable)
    => Sentence annotation level param pat variable
    -> Unannotated.Sentence level param pat variable
unannotateSentence (SentenceHookSentence ann) =
    Unannotated.SentenceHookSentence (unannotateSentenceHook ann)
unannotateSentence (SentenceAliasSentence ann) =
    Unannotated.SentenceAliasSentence (unannotateSentenceAlias ann)
unannotateSentence (SentenceSymbolSentence ann) =
    Unannotated.SentenceSymbolSentence (unannotateSentenceSymbol ann)
unannotateSentence (SentenceImportSentence ann) =
    Unannotated.SentenceImportSentence (unannotateSentenceImport ann)
unannotateSentence (SentenceAxiomSentence ann) =
    Unannotated.SentenceAxiomSentence (unannotateSentenceAxiom ann)
unannotateSentence (SentenceSortSentence ann) =
    Unannotated.SentenceSortSentence (unannotateSentenceSort ann)

unannotateUnifiedSentence
    :: Functor (pat var)
    => UnifiedSentence ann param pat var
    -> Unannotated.UnifiedSentence param pat var
unannotateUnifiedSentence =
    applyUnifiedSentence
    (Unannotated.UnifiedMetaSentence . unannotateSentence)
    (Unannotated.UnifiedObjectSentence . unannotateSentence)

{- | Kore module with annotations.

See also: 'Unannotated.Module'

-}
data Module
    ann
    sentence
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    Module
    { moduleName       :: !ModuleName
    , moduleSentences  :: ![(Annotation ann, sentence ann sortParam pat variable)]
    , moduleAttributes :: !(Attributes ann)
    }
  deriving (Eq, Generic, Show)

instance
    (NFData ann, NFData (sentence ann sortParam pat variable)) =>
    NFData (Module ann sentence sortParam pat variable)

instance
    ( Pretty (sentence ann sort pat variable)
    , Pretty (Fix (pat variable))
    ) => Pretty (Module ann sentence sort pat variable)
  where
    pretty Module {..} =
        (Pretty.vsep . catMaybes)
        [ Just ("module" <+> pretty moduleName)
        , case moduleSentences of
            [] -> Nothing
            _ -> (Just . Pretty.indent 4 . Pretty.vsep)
                 (pretty . snd <$> moduleSentences)
        , Just "endmodule"
        , Just (pretty moduleAttributes)
        ]

unannotateModule
    :: Functor (pat var)
    => Module ann UnifiedSentence param pat var
    -> Unannotated.Module Unannotated.UnifiedSentence param pat var
unannotateModule
    Module
    { moduleName
    , moduleSentences
    , moduleAttributes
    }
  =
    Unannotated.Module
    { moduleName
    , moduleSentences = unannotateUnifiedSentence . snd <$> moduleSentences
    , moduleAttributes = unannotateAttributes moduleAttributes
    }

{-| Annotated Kore definitions.

See also: 'Unannotated.Definition'

-}
data Definition
    ann
    sentence
    sortParam
    (pat :: (* -> *) -> * -> *)
    (variable :: * -> *)
  =
    Definition
    { definitionAttributes :: !(Attributes ann)
    , definitionModules    :: ![(Annotation ann, Module ann sentence sortParam pat variable)]
    }
  deriving (Eq, Generic, Show)

instance
    (NFData ann, NFData (sentence ann sortParam pat variable)) =>
    NFData (Definition ann sentence sortParam pat variable)

instance
    ( Pretty (sentence ann sort pat variable)
    , Pretty (Fix (pat variable))
    ) => Pretty (Definition ann sentence sort pat variable)
  where
    pretty Definition {..} =
        Pretty.vsep
        (pretty definitionAttributes : map (pretty . snd) definitionModules)

unannotateDefinition
    :: Functor (pat var)
    => Definition ann UnifiedSentence sorts pat var
    -> Unannotated.Definition Unannotated.UnifiedSentence sorts pat var
unannotateDefinition
    Definition
    { definitionAttributes
    , definitionModules
    }
  =
    Unannotated.Definition
    { definitionAttributes = unannotateAttributes definitionAttributes
    , definitionModules = unannotateModule . snd <$> definitionModules
    }

type KoreSentenceAlias ann level =
    SentenceAlias ann level UnifiedPattern Variable

type KoreSentenceSymbol ann level =
    SentenceSymbol ann level UnifiedPattern Variable

type KoreSentenceImport ann =
    SentenceImport ann UnifiedPattern Variable

type KoreSentenceAxiom ann =
    SentenceAxiom ann UnifiedSortVariable UnifiedPattern Variable

type KoreSentenceSort ann level =
    SentenceSort ann level UnifiedPattern Variable

type KoreSentenceHook ann =
    SentenceHook ann Object UnifiedPattern Variable


newtype UnifiedSentence ann sortParam pat variable =
    UnifiedSentence
    { getUnifiedSentence ::
        Unified (Rotate41 (Sentence ann) sortParam pat variable)
    }
  deriving (Generic)

instance
    ( NFData (Sentence ann Meta sorts pat var)
    , NFData (Sentence ann Object sorts pat var)
    ) =>
    NFData (UnifiedSentence ann sorts pat var)

{-# COMPLETE UnifiedMetaSentence, UnifiedObjectSentence #-}

pattern UnifiedMetaSentence
    :: Sentence ann Meta sorts pat var -> UnifiedSentence ann sorts pat var
pattern UnifiedMetaSentence pat = UnifiedSentence (UnifiedMeta (Rotate41 pat))

pattern UnifiedObjectSentence
    :: Sentence ann Object sorts pat var -> UnifiedSentence ann sorts pat var
pattern UnifiedObjectSentence pat = UnifiedSentence (UnifiedObject (Rotate41 pat))

instance
    ( Pretty sortParam
    , Pretty (Fix (pat variable))
    , Pretty (variable Meta)
    , Pretty (variable Object)
    , Functor (pat variable)
    ) => Pretty (UnifiedSentence annotation sortParam pat variable)
  where
    pretty = applyUnifiedSentence pretty pretty

type KoreSentence ann =
    UnifiedSentence
        ann
        UnifiedSortVariable
        UnifiedPattern
        Variable

constructUnifiedSentence
    :: (MetaOrObject level)
    => (a -> Sentence ann level sortParam pat variable)
    -> (a -> UnifiedSentence ann sortParam pat variable)
constructUnifiedSentence ctor =
    UnifiedSentence . asUnified . Rotate41 . ctor

-- | Given functions appliable to 'Meta' 'Sentence's and 'Object' 'Sentences's,
-- builds a combined function which can be applied on 'UnifiedSentence's.
applyUnifiedSentence
    :: (Sentence ann Meta sortParam pat variable -> b)
    -> (Sentence ann Object sortParam pat variable -> b)
    -> (UnifiedSentence ann sortParam pat variable -> b)
applyUnifiedSentence metaT _ (UnifiedMetaSentence sentence) = metaT sentence
applyUnifiedSentence _ objectT (UnifiedObjectSentence sentence) = objectT sentence

type KoreModule ann =
    Module ann UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

type KoreDefinition ann =
    Definition ann UnifiedSentence UnifiedSortVariable UnifiedPattern Variable

instance
    ( MetaOrObject level
    ) => AsSentence (KoreSentence ann) (KoreSentenceAlias ann level)
  where
    asSentence = constructUnifiedSentence SentenceAliasSentence

instance
    ( MetaOrObject level
    ) => AsSentence (KoreSentence ann) (KoreSentenceSymbol ann level)
  where
    asSentence = constructUnifiedSentence SentenceSymbolSentence

instance AsSentence (KoreSentence ann) (KoreSentenceImport ann) where
    asSentence = constructUnifiedSentence SentenceImportSentence

instance AsSentence (KoreSentence ann) (KoreSentenceAxiom ann) where
    asSentence = constructUnifiedSentence SentenceAxiomSentence

instance
    ( MetaOrObject level
    ) => AsSentence (KoreSentence ann) (KoreSentenceSort ann level)
  where
    asSentence = constructUnifiedSentence SentenceSortSentence

instance AsSentence (KoreSentence ann) (KoreSentenceHook ann) where
    asSentence = constructUnifiedSentence SentenceHookSentence
