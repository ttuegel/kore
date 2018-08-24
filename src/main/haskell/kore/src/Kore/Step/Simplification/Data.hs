{-|
Module      : Kore.Step.Simplification.Data
Description : Data structures used for term simplification.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Data
    ( SimplificationError
    , Simplifier
    , SimplifierState (..)
    , evalSimplifier
    , runSimplifier
    , PureMLPatternSimplifier (..)
    , CommonPureMLPatternSimplifier
    , SimplificationProof (..)
    ) where

import           Control.Monad.Except
                 ( Except, MonadError, runExcept )
import qualified Control.Monad.Except as Monad.Except
import           Control.Monad.State.Strict
                 ( MonadState, StateT, evalStateT, runStateT )
import qualified Control.Monad.State.Strict as Monad.State

import Control.Monad.Counter
import Kore.AST.Common
       ( Variable )
import Kore.AST.PureML
       ( PureMLPattern )
import Kore.Error
       ( Error )
import Kore.Step.OrOfExpandedPattern
       ( OrOfExpandedPattern )


{- | A tag for errors during simplification

  See also: 'Error'

 -}
data SimplificationError

{-| 'SimplificationProof' is a placeholder for proofs showing that the
simplification of a MetaMLPattern was correct.
-}
data SimplificationProof level = SimplificationProof
    deriving (Show, Eq)

{- | The concrete monad in which simplification occurs.

 -}
-- TODO (thomas.tuegel): Replace Counter with a single state carrying both
-- the counter and the proof.
newtype Simplifier a =
    Simplifier
    { getSimplifier
        :: StateT SimplifierState (Except (Error SimplificationError)) a
    }
  deriving (Applicative, Functor, Monad)

instance MonadState SimplifierState Simplifier where
    state f = Simplifier (Monad.State.state f)

instance MonadCounter Simplifier where
    get = Simplifier (Monad.State.gets simplifierCounter)
    put c = Simplifier (Monad.State.modify modify0)
      where
        modify0 state = state { simplifierCounter = c }

instance MonadError (Error SimplificationError) Simplifier where
    throwError e = Simplifier (Monad.Except.throwError e)
    catchError a h =
        Simplifier (Monad.Except.catchError a' h')
      where
        a' = getSimplifier a
        h' e = getSimplifier (h e)

newtype SimplifierState =
    SimplifierState { simplifierCounter :: Counter }

{- | Evaluate a simplifier computation.

  Only the result (or error) is returned. The 'IntCounter' is discarded.

  -}
evalSimplifier :: Simplifier a -> Either (Error SimplificationError) a
evalSimplifier simp =
    runExcept (evalStateT (getSimplifier simp) state)
  where
    state = SimplifierState { simplifierCounter = Counter 0 }

runSimplifier
    :: Simplifier a
    -> SimplifierState
    -> Either (Error SimplificationError) (a, SimplifierState)
runSimplifier simplifier state =
    runExcept (runStateT (getSimplifier simplifier) state)

{-| 'PureMLPatternSimplifier' wraps a function that evaluates
Kore functions on PureMLPatterns.
-}
newtype PureMLPatternSimplifier level variable =
    PureMLPatternSimplifier
        ( PureMLPattern level variable
        -> Simplifier
            ( OrOfExpandedPattern level variable
            , SimplificationProof level
            )
        )

{-| 'CommonPurePatternFunctionEvaluator' wraps a function that evaluates
Kore functions on CommonPurePatterns.
-}
type CommonPureMLPatternSimplifier level =
    PureMLPatternSimplifier level Variable
