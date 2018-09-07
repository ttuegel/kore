{-|
Module      : Kore.Variables.Fresh.IntCounter
Description : Defines an 'IntCounter' 'Monad' encapsulating an integer counter.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.Variables.Fresh.IntCounter
    ( IntCounter
    , findState
    , runIntCounter
    ) where

import Control.Monad.State
       ( MonadState (get, put), State, runState )

-- |'IntCounter' is a monad encapsulating an integer counter
newtype IntCounter a = IntCounter { intCounterState :: State Int a }
  deriving (Applicative, Functor, Monad)

{-|'runIntCounter' evaluates the computation with the given initial counter
and yields a value containing the state.
-}
runIntCounter :: IntCounter a -> Int -> (a, Int)
runIntCounter = runState . intCounterState

deriving instance MonadState Int IntCounter

{-|@findState@ takes a predicate and a list of 'MonadState' actions, and
locates the first action whose result satisfies the predicate, resetting the
state before executing each action.
The effect of this is that only the selected action is evaluated.
-}
findState
    :: MonadState state m
    => (a -> Bool) -> [m a] -> m (Maybe a)
findState _ [] = return Nothing
findState predicate (h:t)
  = do
    oldState <- get
    condVal <- h
    if predicate condVal then return (Just condVal)
    else do
        put oldState
        findState predicate t
