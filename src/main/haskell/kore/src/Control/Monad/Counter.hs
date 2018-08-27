module Control.Monad.Counter
    ( Counter (..)
    , Counting, runCounting, evalCounting
    , MonadCounter (..)
    , findState
    ) where

import qualified Control.Monad.Except as Monad.Except
import qualified Control.Monad.Identity as Monad.Identity
import qualified Control.Monad.List as Monad.List
import qualified Control.Monad.Reader as Monad.Reader
import qualified Control.Monad.RWS.Lazy as Monad.RWS.Lazy
import qualified Control.Monad.RWS.Strict as Monad.RWS.Strict
import           Control.Monad.State
                 ( MonadState )
import qualified Control.Monad.State.Class as Monad.State
import qualified Control.Monad.State.Lazy as Monad.State.Lazy
import qualified Control.Monad.State.Strict as Monad.State.Strict
import qualified Control.Monad.Trans as Monad.Trans
import qualified Control.Monad.Writer.Lazy as Monad.Writer.Lazy
import qualified Control.Monad.Writer.Strict as Monad.Writer.Strict

{- | An enumerable counter.

  The @newtype@ wrapper is to avoid confusion in complex states.

 -}
newtype Counter = Counter { getCounter :: Int }
  deriving (Enum, Eq, Ord, Show)

newtype Counting a = Counting (Monad.State.Strict.State Counter a)
  deriving (Applicative, Functor, Monad)

-- | The @MonadState@ instance must not be used to carelessly reset the counter!
deriving instance MonadState Counter Counting

instance MonadCounter Counting where
    increment = do
        n <- Monad.State.get
        Monad.State.modify' succ
        return n

{- | Run a computation using a @Counter@.

  The counter is initialized to the given value. The final result and @Counter@
  are returned.

 -}
runCounting
    :: Counting a
    -- ^ computation
    -> Counter
    -- ^ initial counter
    -> (a, Counter)
runCounting (Counting counting) = Monad.State.Strict.runState counting

{- | Evaluate a computation using a @Counter@, returning only the final result.

  The counter is initialized to @Counter 0@.

 -}
evalCounting :: Counting a -> a
evalCounting (Counting counting) =
    let (a, _) = Monad.State.Strict.runState counting (Counter 0)
    in a

{- | @MonadCounter@ abstracts a state monad carrying a counter.

  The counter is generally used for fresh variable generation. The interface is
  intended to be safer than a 'MonadState' instance, which could accidentally be
  reset. @MonadCounter@ also allows access to /only/ the counter in a monad with
  more complex state.

 -}
class Monad m => MonadCounter m where
    {- | Increment the counter and return the prior value.

      Using the @MonadCounter@ interface instead of the 'MonadState' instance
      ensures that the counter cannot accidentally be reset, which could
      generate duplicate fresh variables.
     -}
    increment :: m Counter

instance MonadCounter m => MonadCounter (Monad.Except.ExceptT e m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance MonadCounter m => MonadCounter (Monad.Identity.IdentityT m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance MonadCounter m => MonadCounter (Monad.List.ListT m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.RWS.Lazy.RWST r w s m)
  where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.RWS.Strict.RWST r w s m)
  where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance MonadCounter m => MonadCounter (Monad.Reader.ReaderT r m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance MonadCounter m => MonadCounter (Monad.State.Lazy.StateT s m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance MonadCounter m => MonadCounter (Monad.State.Strict.StateT s m) where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.Writer.Lazy.WriterT w m)
  where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

instance
    (MonadCounter m, Monoid w) =>
    MonadCounter (Monad.Writer.Strict.WriterT w m)
  where
    increment = Monad.Trans.lift increment
    {-# INLINE increment #-}

{- | Execute a list of actions until one satisfies the given predicate.

  The `MonadPlus` constraint ensures that execution rewinds after any action
  that does not satisfy the predicate.

 -}
findState
    :: MonadState s m
    => (a -> Bool)
    -- ^ predicate
    -> [m a]
    -- ^ actions
    -> m (Maybe a)
findState predicate = findState0
  where
    findState0 [] = return Nothing
    findState0 (action : actions) =
        do
            s <- Monad.State.get
            a <- action
            if predicate a
                then return (Just a)
                else do
                    Monad.State.put s
                    findState0 actions
