{-|
Module      : Kore.AST.PureML
Description : Annotated matching logic patterns at a fixed level
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.AST.Annotated.PureML
    ( PureMLPattern
    , CommonPurePattern
    , CommonMetaPattern
    , asPurePattern
    , fromPurePattern
    , annotatePurePattern
    , unannotatePurePattern
    ) where

import Control.Comonad
import Control.Comonad.Trans.Cofree
       ( Cofree, CofreeF ((:<)) )
import Data.Functor.Compose
       ( Compose (..) )
import Data.Functor.Identity
       ( Identity (..) )
import Data.Functor.Foldable

import Data.Annotation
       ( Annotation (..) )
import Kore.AST.Common
import Kore.AST.MetaOrObject
       ( Meta )
import qualified Kore.AST.PureML as Unannotated

{-| Annotated matching logic patterns at a fixed, specified level.

Annotated patterns are defined as a 'Cofree' comonad to hold annotations @'Annotation' ann@ at
each node.

See also: 'Unannotated.PureMLPattern'

-}
type PureMLPattern ann level variable =
    Cofree (Pattern level variable) (Annotation ann)

type CommonPurePattern ann level = PureMLPattern ann level Variable
type CommonMetaPattern ann = CommonPurePattern ann Meta

asPurePattern
    :: ann
    -> Pattern level var (PureMLPattern ann level var)
    -> PureMLPattern ann level var
asPurePattern ann pat = (embed . Compose . Identity) (Annotation ann :< pat)

fromPurePattern
    :: PureMLPattern ann level var
    -> Pattern level var (PureMLPattern ann level var)
fromPurePattern purePattern =
    let Compose (Identity (_ :< pat)) = project purePattern in pat

{- | Apply annotations to a 'Unannotated.PureMLPattern'.

See also: 'unannotatePurePattern'

-}
annotatePurePattern
    :: forall level var ann.
       (Pattern level var ann -> ann)
       -- ^ generate an annotation at a node, given the child annotations
    -> Unannotated.PureMLPattern level var
       -- ^ 'Unannotated.PureMLPattern' to annotate
    -> PureMLPattern ann level var
annotatePurePattern annotate =
    -- fold: build the new tree starting at the bottom of the old tree
    fold annotate1
  where
    annotate1
        :: Pattern level var (PureMLPattern ann level var)
        -> PureMLPattern ann level var
    annotate1 pat =
        let
            ann = annotate (getAnnotation . extract <$> pat)
        in
            embed (Compose (Identity (Annotation ann :< pat)))

{- | Forget the annotations of an 'PureMLPattern'.

See also: 'annotatePurePattern'

-}
unannotatePurePattern
    :: PureMLPattern ann level var
    -> Unannotated.PureMLPattern level var
unannotatePurePattern =
    unfold (\ann -> let Compose (Identity (_ :< pat)) = project ann in pat)
