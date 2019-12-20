{-|
Module      : ListT
Description : List monad transformer
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module implements the list monad transformer.

-}

{-# LANGUAGE UndecidableInstances #-}

module ListT
    ( ListT
    , cons
    , gather
    , scatter
    , mapListT
    , foldM
    -- * Re-exports
    , Alternative (..), MonadPlus (..)
    ) where

import Control.Applicative
import Control.Monad hiding
    ( foldM
    )
import Control.Monad.Morph
import Pipes
    ( ListT
    )
import qualified Pipes
import qualified Pipes.Prelude as Pipes

cons :: Functor m => a -> ListT m a -> ListT m a
cons a as = Pipes.Select $ do
    Pipes.yield a
    Pipes.enumerate as

{- | Collect all values produced by a @'ListT' m@ as a list in @m@.
 -}
gather :: Monad m => ListT m a -> m [a]
gather = Pipes.toListM . Pipes.enumerate

{- | Distribute a 'Foldable' collection of values as a @'ListT' m@ stream.

Usually, @f ~ []@.

 -}
scatter :: (Foldable f, Functor m) => f a -> ListT m a
scatter = Pipes.Select . Pipes.each

{- | Apply a transformation of the 'Monad' @m@ underlying @'ListT' m@.

The transformation is applied to the entire list, i.e. given

@
mapListT (\during -> before >> during >> after)
@

the actions @before@ and @after@ are sequenced before and after evaluating the
contents of the list, respectively.

 -}
mapListT
    :: (Monad m, Monad n)
    => (forall x. m x -> n x)
    -> ListT m a
    -> ListT n a
mapListT mapping as = (lift . mapping) (gather as) >>= scatter

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ListT m a -> m b
foldM yielding zero done listT =
    Pipes.foldM yielding zero done (Pipes.enumerate listT)
