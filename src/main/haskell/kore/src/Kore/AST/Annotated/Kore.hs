{-|
Module      : Kore.AST.Annotated.Kore
Description : Annotated Kore patterns at any level level
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.AST.Annotated.Kore
    ( module Kore.AST.Kore
    , KorePattern
    , CommonKorePattern
    , asKorePattern
    , asMetaKorePattern
    , asObjectKorePattern
    , applyKorePattern
    , annotateKorePattern
    , unannotateKorePattern
    ) where

import Control.Comonad
import Control.Comonad.Trans.Cofree
       ( Cofree, CofreeF ((:<)) )
import Data.Functor.Compose
       ( Compose (..) )
import Data.Functor.Foldable
import Data.Functor.Identity
       ( Identity (..) )

import Data.Annotation
       ( Annotation (..) )
import Kore.AST.Common
import Kore.AST.MetaOrObject
       ( MetaOrObject, Meta, Object )
import Kore.AST.Kore
       ( UnifiedPattern (..), pattern UnifiedObjectPattern,
       pattern UnifiedMetaPattern, asUnifiedPattern, UnifiedSortVariable )
import qualified Kore.AST.Kore as Unannotated

{-| Annotated Kore patterns at the 'Meta' and 'Object' level.

Annotated Kore patterns are defined as a @Cofree@ comonad to hold annotations @'Annotation' ann@ at
each node.

See also: 'Unannotated.KorePattern', 'asKorePattern' (construction),
'Data.Functor.Foldable.Corecursive' (construction),
'Data.Functor.Foldable.Recursive' (deconstruction).

-}
type KorePattern ann variable =
    Cofree (UnifiedPattern variable) (Annotation ann)

{- | Specialize 'KorePattern' to common 'Variable's.

See also: 'Unannotated.CommonKorePattern'

-}
type CommonKorePattern ann = KorePattern ann Variable

{- | View a 'Meta' or an 'Object' 'Pattern' as a 'KorePattern'.

The provided annotation is used for the top-level node.

-}
asKorePattern
    :: (MetaOrObject level)
    => ann
    -> Pattern level variable (KorePattern ann variable)
    -> KorePattern ann variable
asKorePattern ann pat =
    (embed . Compose . Identity) (Annotation ann :< asUnifiedPattern pat)

{- | View a 'Meta'-level 'Pattern' as a 'KorePattern'.

The provided annotation is used for the top-level node.

Specializes: 'asKorePattern'.

-}
asMetaKorePattern
    :: ann
    -> Pattern Meta variable (KorePattern ann variable)
    -> KorePattern ann variable
asMetaKorePattern = asKorePattern

{- | View an 'Object'-level 'Pattern' as a 'KorePattern'

The provided annotation is used for the top-level node.

Specializes: 'asKorePattern'.

-}
asObjectKorePattern
    :: ann
    -> Pattern Object variable (KorePattern ann variable)
    -> KorePattern ann variable
asObjectKorePattern = asKorePattern

{- | Apply a pair of functions to both levels of an 'KorePattern'.

See also: 'Unannotated.applyKorePattern'

-}
applyKorePattern
    :: (ann -> Pattern Meta variable (KorePattern ann variable) -> b)
       -- ^ function applied to meta-level patterns
    -> (ann -> Pattern Object variable (KorePattern ann variable) -> b)
       -- ^ function applied to object-level patterns
    -> (KorePattern ann variable -> b)
applyKorePattern metaT objectT korePattern =
    let
        Compose (Identity (Annotation ann :< unifiedPattern)) =
            project korePattern
    in
        case unifiedPattern of
            UnifiedMetaPattern pat -> metaT ann pat
            UnifiedObjectPattern pat -> objectT ann pat

{- | Apply annotations to a 'KorePattern'.

See also: 'unannotateKorePattern'

-}
annotateKorePattern
    :: forall var ann.
       (UnifiedPattern var ann -> ann)
       -- ^ generate an annotation at a node, given the child annotations
    -> Unannotated.KorePattern var
       -- ^ 'KorePattern' to annotate
    -> KorePattern ann var
annotateKorePattern annotate =
    -- fold: build the new tree starting at the bottom of the old tree
    fold annotate1
  where
    annotate1
        :: UnifiedPattern var (KorePattern ann var)
        -> KorePattern ann var
    annotate1 pat =
        let
            ann = annotate (getAnnotation . extract <$> pat)
        in
            embed (Compose (Identity (Annotation ann :< pat)))

{- | Forget the annotations of an 'AnnotatedKorePattern'.

See also: 'annotateKorePattern'

-}
unannotateKorePattern
    :: KorePattern ann var
    -> Unannotated.KorePattern var
unannotateKorePattern =
    unfold (\ann -> let Compose (Identity (_ :< pat)) = project ann in pat)
