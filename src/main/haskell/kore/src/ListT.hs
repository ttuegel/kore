{-|
Module      : ListT
Description : List monad transformer
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module implements the codensity formulation of the list monad transformer.

-}


module ListT
    ( ListT (..)
    , toListM
    -- * Re-exports
    , Alternative (..), MonadPlus (..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Typeable

-- | The codensity formulation of the list monad transformer.
newtype ListT m a =
    ListT { getListT :: forall r. (a -> m r -> m r) -> m r -> m r }
    deriving (Typeable)

instance Functor (ListT m) where
    fmap f as = ListT $ \yield -> getListT as (yield . f)
    {-# INLINE fmap #-}

instance Applicative (ListT m) where
    pure a = ListT $ \yield -> yield a
    {-# INLINE pure #-}

    (<*>) fs as =
        ListT $ \yield ->
            getListT fs $ \f ->
                getListT as $ \a ->
                    yield (f a)
    {-# INLINE (<*>) #-}

instance Alternative (ListT m) where
    empty = ListT $ \_ next -> next
    {-# INLINE empty #-}

    (<|>) as bs = ListT $ \yield -> getListT as yield . getListT bs yield
    {-# INLINE (<|>) #-}

instance Monad (ListT m) where
    return = pure
    {-# INLINE return #-}

    (>>=) as k =
        ListT $ \yield ->
            getListT as $ \a ->
                getListT (k a) yield
    {-# INLINE (>>=) #-}

instance MonadPlus (ListT m)

instance MonadTrans ListT where
    lift m = ListT $ \yield next -> m >>= \a -> yield a next
    {-# INLINE lift #-}

instance MonadReader r m => MonadReader r (ListT m) where
    ask = lift ask
    {-# INLINE ask #-}

    reader f = lift (reader f)
    {-# INLINE reader #-}

    local f as = ListT $ \yield -> local f . getListT as yield
    {-# INLINE local #-}

instance MonadState s m => MonadState s (ListT m) where
    get = lift get
    {-# INLINE get #-}

    put s = lift (put s)
    {-# INLINE put #-}

    state f = lift (state f)
    {-# INLINE state #-}

instance MonadIO m => MonadIO (ListT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Applicative f, Foldable f) => Foldable (ListT f) where
    foldMap f as =
        fold (getListT as (\a r -> mappend (f a) <$> r) (pure mempty))
    {-# INLINE foldMap #-}

{- | Collect all values produced by a @'ListT' m@ as a list in @m@.
 -}
toListM :: Applicative m => ListT m a -> m [a]
toListM as = getListT as (\a mr -> (a :) <$> mr) (pure [])
