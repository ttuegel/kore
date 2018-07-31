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
    , asPurePattern
    , fromPurePattern
    , annotatePurePattern
    , unannotatePurePattern
    ) where

import Control.Comonad.Cofree
       ( Cofree )
import Control.Comonad.Trans.Cofree
       ( CofreeF ((:<)) )
import Data.Functor.Foldable

import Data.Annotation
       ( Annotation (..) )
import Kore.AST.Common
import qualified Kore.AST.PureML as Unannotated

{-| Annotated matching logic patterns at a fixed, specified level.

Annotated patterns are defined as a 'Cofree' comonad to hold annotations @'Annotation' ann@ at
each node.

See also: 'Unannotated.PureMLPattern'

-}
type PureMLPattern ann level variable =
    Cofree (Pattern level variable) (Annotation ann)

asPurePattern
    :: ann
    -> Pattern level var (PureMLPattern ann level var)
    -> PureMLPattern ann level var
asPurePattern ann pat = embed (Annotation ann :< pat)

fromPurePattern
    :: PureMLPattern ann level var
    -> Pattern level var (PureMLPattern ann level var)
fromPurePattern purePattern = let (_ :< pat) = project purePattern in pat

{- | Apply annotations to a 'Unannotated.PureMLPattern'.

See also: 'unannotatePurePattern'

-}
annotatePurePattern
    :: (Pattern level var ann -> ann)
       -- ^ generate an annotation at a node, given the child annotations
    -> Unannotated.PureMLPattern level var
       -- ^ 'Unannotated.PureMLPattern' to annotate
    -> PureMLPattern ann level var
annotatePurePattern annotate =
    unfold annotate1
  where
    annotate1 pat =
        fold (Annotation . annotate . fmap getAnnotation) pat :< project pat

{- | Forget the annotations of an 'PureMLPattern'.

See also: 'annotatePurePattern'

-}
unannotatePurePattern
    :: PureMLPattern ann level var
    -> Unannotated.PureMLPattern level var
unannotatePurePattern =
    unfold (\ann -> let _ :< pat = project ann in pat)
