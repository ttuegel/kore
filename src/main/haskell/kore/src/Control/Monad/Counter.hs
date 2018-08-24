module Control.Monad.Counter
    ( Counter (..)
    , MonadCounter (..)
    , findState
    ) where

import qualified Control.Monad.Except as Monad.Except
import qualified Control.Monad.Identity as Monad.Identity
import qualified Control.Monad.RWS.Lazy as Monad.RWS.Lazy
import qualified Control.Monad.RWS.Strict as Monad.RWS.Strict
import qualified Control.Monad.Reader as Monad.Reader
import qualified Control.Monad.State.Lazy as Monad.State.Lazy
import qualified Control.Monad.State.Strict as Monad.State.Strict
import qualified Control.Monad.Trans as Monad.Trans
import qualified Control.Monad.Writer.Lazy as Monad.Writer.Lazy
import qualified Control.Monad.Writer.Strict as Monad.Writer.Strict

newtype Counter = Counter { getCounter :: Int }
  deriving (Enum, Eq, Ord, Show)

class Monad m => MonadCounter m where
    get :: m Counter
    put :: Counter -> m ()

    modify :: (Counter -> Counter) -> m ()
    modify f = get >>= \c -> put $ f c

    -- | Increment the counter and return the previous value.
    increment :: m Counter
    increment =
        do
            n <- get
            modify succ
            return n

{- | Locate the first action in the list satisfying the predicate.

  The state is reset before each action, the effect being that only the selected
  action is evaluated.

 -}
findState
    :: MonadCounter m
    => (a -> Bool) -> [m a] -> m (Maybe a)
findState predicate (h : t) = do
    oldCounter <- get
    condVal <- h
    if predicate condVal
        then return (Just condVal)
        else do
            put oldCounter
            findState predicate t
findState _ [] = return Nothing

instance MonadCounter m => MonadCounter (Monad.Except.ExceptT e m) where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance MonadCounter m => MonadCounter (Monad.Identity.IdentityT m) where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.RWS.Lazy.RWST r w s m)
  where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.RWS.Strict.RWST r w s m)
  where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance MonadCounter m => MonadCounter (Monad.Reader.ReaderT r m) where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance (Monad m, s ~ Counter) => MonadCounter (Monad.State.Lazy.StateT s m) where
    get = Monad.State.Lazy.get
    put = Monad.State.Lazy.put
    modify = Monad.State.Lazy.modify

instance (Monad m, s ~ Counter) => MonadCounter (Monad.State.Strict.StateT s m) where
    get = Monad.State.Strict.get
    put = Monad.State.Strict.put
    modify = Monad.State.Strict.modify

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.Writer.Lazy.WriterT w m)
  where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.Writer.Strict.WriterT w m)
  where
    get = Monad.Trans.lift get
    put c = Monad.Trans.lift (put c)
    modify f = Monad.Trans.lift (modify f)
    increment = Monad.Trans.lift increment
