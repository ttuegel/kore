{-|
Module      : Kore.AST.Kore
Description : Data Structures for representing the Kore language AST with
              unified constructs.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module includes all the data structures necessary for representing
the syntactic categories of a Kore definition that need unified
constructs.

Unified constructs are those that represent both meta and object versions of
an AST term in a single data type (e.g. 'UnifiedSort' that can be either
'Sort Object' or 'Sort Meta')

Please refer to Section 9 (The Kore Language) of
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>.
-}
module Kore.AST.Kore
    (
      -- * Kore patterns
      CommonKorePattern
    , KorePattern
    , asKorePattern
    , asMetaKorePattern
    , asObjectKorePattern
    , applyKorePattern
      -- * Kore patterns with annotations
    , AnnotatedKorePattern
    , AnnotatedCommonKorePattern
    , asAnnotatedKorePattern
    , asAnnotatedMetaKorePattern
    , asAnnotatedObjectKorePattern
    , applyAnnotatedKorePattern
    , annotateKorePattern
    , unannotateKorePattern
      -- * Unified patterns
    , UnifiedPattern (..)
    , UnifiedSortVariable
    , UnifiedSort
    , asUnifiedPattern
    , transformUnifiedPattern
    ) where

import Control.Comonad.Cofree
       ( Cofree )
import Control.Comonad.Trans.Cofree
       ( CofreeF ((:<)) )
import Data.Functor.Classes
import Data.Functor.Foldable

import Data.Functor.Impredicative
       ( Rotate31 (..) )
import Kore.AST.Common
import Kore.AST.MetaOrObject
import Kore.AST.Pretty
       ( Pretty (..) )



{-|'UnifiedPattern' is joining the 'Meta' and 'Object' versions of 'Pattern', to
allow using toghether both 'Meta' and 'Object' patterns.
-}
newtype UnifiedPattern variable child = UnifiedPattern
    { getUnifiedPattern :: Unified (Rotate31 Pattern variable child) }

instance (EqMetaOrObject variable) => Eq1 (UnifiedPattern variable) where
    liftEq liftedEq (UnifiedPattern a) (UnifiedPattern b) =
       case (a, b) of
          (UnifiedMeta a', UnifiedMeta b') ->
              liftEq liftedEq (unRotate31 a') (unRotate31 b')
          (UnifiedObject a', UnifiedObject b') ->
              liftEq liftedEq (unRotate31 a') (unRotate31 b')
          _ -> False

instance (ShowMetaOrObject variable) => Show1 (UnifiedPattern variable) where
    liftShowsPrec :: forall a.
                     (Int -> a -> ShowS) -> ([a] -> ShowS)
                  -> Int -> UnifiedPattern variable a
                  -> ShowS
    liftShowsPrec showsPrec_ showList_ _ up =
        showString "UnifiedPattern { getUnifiedPattern = "
        . applyUnified
            (\t -> showString "UnifiedMeta " . liftShowsPrecRotate31 t)
            (\t -> showString "UnifiedObject " . liftShowsPrecRotate31 t)
            (getUnifiedPattern up)
        . showString " }"
      where
        liftShowsPrecRotate31 :: Show (variable level)
                              => Rotate31 Pattern variable a level
                              -> ShowS
        liftShowsPrecRotate31 r =
            showString "Rotate31 { unRotate31 = "
            . liftShowsPrec showsPrec_ showList_ 0 (unRotate31 r)
            . showString " }"

instance (Pretty child, Pretty (variable Meta), Pretty (variable Object)) =>
    Pretty (UnifiedPattern variable child) where
    pretty = applyUnified (pretty . unRotate31) (pretty . unRotate31) . getUnifiedPattern

-- |View a 'Meta' or an 'Object' 'Pattern' as an 'UnifiedPattern'
asUnifiedPattern
    :: (MetaOrObject level)
    => Pattern level variable child -> UnifiedPattern variable child
asUnifiedPattern = UnifiedPattern . asUnified . Rotate31

-- |Given a function appliable on all 'Meta' or 'Object' 'Pattern's,
-- apply it on an 'UnifiedPattern'.
transformUnifiedPattern
    :: (forall level . MetaOrObject level => Pattern level variable a -> b)
    -> (UnifiedPattern variable a -> b)
transformUnifiedPattern f =
    transformUnified (f . unRotate31) . getUnifiedPattern

deriving instance
    ( Eq child
    , EqMetaOrObject variable
    ) => Eq (UnifiedPattern variable child)

deriving instance
    ( Show child
    , ShowMetaOrObject variable
    ) => Show (UnifiedPattern variable child)

instance Functor (UnifiedPattern variable) where
    fmap f =
        UnifiedPattern
        . mapUnified (Rotate31 . fmap f . unRotate31)
        . getUnifiedPattern
instance Foldable (UnifiedPattern variable) where
    foldMap f =
        transformUnified (foldMap f . unRotate31)
        . getUnifiedPattern
instance Traversable (UnifiedPattern variable) where
    sequenceA =
        fmap UnifiedPattern
        . sequenceUnified
            (fmap Rotate31 . sequenceA . unRotate31)
        . getUnifiedPattern

{-| Kore patterns at the 'Meta' and 'Object' level.

@KorePattern@ corresponds to the @pattern@ syntactic category in
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf The Semantics of K>,
Section 9.1.4 (Patterns).

See also: 'AnnotatedKorePattern'

-}
type KorePattern variable = Fix (UnifiedPattern variable)

-- | View a 'Meta' or an 'Object' 'Pattern' as a 'KorePattern'.
asKorePattern
    :: (MetaOrObject level)
    => Pattern level variable (KorePattern variable)
    -> KorePattern variable
asKorePattern pat = embed (asUnifiedPattern pat)

-- |View a 'Meta' 'Pattern' as a 'KorePattern'
asMetaKorePattern
    :: Pattern Meta variable (KorePattern variable)
    -> KorePattern variable
asMetaKorePattern = asKorePattern

-- |View a 'Object' 'Pattern' as a 'KorePattern'
asObjectKorePattern
    :: Pattern Object variable (KorePattern variable)
    -> KorePattern variable
asObjectKorePattern = asKorePattern

instance
    UnifiedPatternInterface UnifiedPattern
  where
    unifyPattern = asUnifiedPattern
    unifiedPatternApply = transformUnifiedPattern

-- |'CommonKorePattern' is the instantiation of 'KorePattern' with common
-- 'Variable's.
type CommonKorePattern = KorePattern Variable

{- | Given functions appliable to 'Meta' 'Pattern's and 'Object' 'Pattern's,
builds a combined function which can be applied on an 'KorePattern'.

See also: 'applyAnnotatedKorePattern'

-}
applyKorePattern
    :: (Pattern Meta variable (KorePattern variable) -> b)
    -> (Pattern Object variable (KorePattern variable) -> b)
    -> (KorePattern variable -> b)
applyKorePattern metaT objectT korePattern =
    case getUnifiedPattern (project korePattern) of
        UnifiedMeta rp   -> metaT (unRotate31 rp)
        UnifiedObject rp -> objectT (unRotate31 rp)

type UnifiedSortVariable = Unified SortVariable
type UnifiedSort = Unified Sort

{-| Kore patterns at the 'Meta' and 'Object' level with annotations.

Annotated Kore patterns are defined as a @Cofree@ comonad to hold annotations at
each node. The type of annotation has been omitted from the definition to allow
@AnnotatedKorePattern@ to be partially applied; if all type parameters were
specified explicitly, the definition would be:

@
type AnnotatedKorePattern variable annotation =
    Cofree (UnifiedPattern variable) annotation
@

Construct an @AnnotatedKorePattern@ using 'asAnnotatedKorePattern' or
'Data.Functor.Foldable.Corecursive'.

Deconstruct an @AnnotatedKorePattern@ using 'Data.Functor.Foldable.Recursive'.

-}
type AnnotatedKorePattern variable {- annotation -} =
    Cofree (UnifiedPattern variable) {- annotation -}

{- | Specialize 'AnnotatedKorePattern' to common 'Variable's.

See also: 'CommonKorePattern'

-}
type AnnotatedCommonKorePattern = AnnotatedKorePattern Variable

{- | View a 'Meta' or an 'Object' 'Pattern' as an 'AnnotatedKorePattern'.

The provided annotation is used for the top-level node.

-}
asAnnotatedKorePattern
    :: (MetaOrObject level)
    => annotation
    -> Pattern level variable (AnnotatedKorePattern variable annotation)
    -> AnnotatedKorePattern variable annotation
asAnnotatedKorePattern annotation pat =
    embed (annotation :< asUnifiedPattern pat)

{- | View a 'Meta'-level 'Pattern' as an 'AnnotatedKorePattern'.

The provided annotation is used for the top-level node.

Specializes: 'asAnnotatedKorePattern'.

-}
asAnnotatedMetaKorePattern
    :: annotation
    -> Pattern Meta variable (AnnotatedKorePattern variable annotation)
    -> AnnotatedKorePattern variable annotation
asAnnotatedMetaKorePattern = asAnnotatedKorePattern

{- | View an 'Object'-level 'Pattern' as an 'AnnotatedKorePattern'

The provided annotation is used for the top-level node.

Specializes: 'asAnnotatedKorePattern'.

-}
asAnnotatedObjectKorePattern
    :: annotation
    -> Pattern Object variable (AnnotatedKorePattern variable annotation)
    -> AnnotatedKorePattern variable annotation
asAnnotatedObjectKorePattern = asAnnotatedKorePattern

{- | Apply a pair of functions to both levels of an 'AnnotatedKorePattern'.

See also: 'applyKorePattern'

-}
applyAnnotatedKorePattern
    :: (   annotation
        -> Pattern Meta variable (AnnotatedKorePattern variable annotation)
        -> b
       )
       -- ^ function applied to meta-level patterns
    -> (   annotation
        -> Pattern Object variable (AnnotatedKorePattern variable annotation)
        -> b
       )
       -- ^ function applied to object-level patterns
    -> (AnnotatedKorePattern variable annotation -> b)
applyAnnotatedKorePattern metaT objectT korePattern =
    let
        (annotation :< unifiedPattern) = project korePattern
    in
        case getUnifiedPattern unifiedPattern of
            UnifiedMeta metaPattern -> metaT annotation (unRotate31 metaPattern)
            UnifiedObject objectPattern ->
                objectT annotation (unRotate31 objectPattern)

{- | Apply annotations to a 'KorePattern'.

See also: 'unannotateKorePattern'

-}
annotateKorePattern
    :: (UnifiedPattern var annotation -> annotation)
       -- ^ generate an annotation at a node, given the child annotations
    -> KorePattern var
       -- ^ 'KorePattern' to annotate
    -> AnnotatedKorePattern var annotation
annotateKorePattern annotate =
    unfold (\pat -> fold annotate pat :< project pat)

{- | Forget the annotations of an 'AnnotatedKorePattern'.

See also: 'annotateKorePattern'

-}
unannotateKorePattern
    :: AnnotatedKorePattern var annotation
    -> KorePattern var
unannotateKorePattern =
    unfold (\ann -> let _ :< pat = project ann in pat)
