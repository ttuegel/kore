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
    (  Simplifier (..)
    , runSimplifier
    , evalSimplifier
    , liftCounter
    , PureMLPatternSimplifier (..)
    , CommonPureMLPatternSimplifier
    , SimplificationProof (..)
    ) where

import           Control.Monad.State.Class
                 ( MonadState )
import           Control.Monad.State.Strict
                 ( State, runState )
import qualified Control.Monad.State.Strict as Monad.State

import Kore.AST.Common
       ( Variable )
import Kore.AST.PureML
       ( PureMLPattern )
import Kore.Step.OrOfExpandedPattern
       ( OrOfExpandedPattern )
import Kore.Variables.Fresh

{-| 'SimplificationProof' is a placeholder for proofs showing that the
simplification of a MetaMLPattern was correct.
-}
data SimplificationProof level = SimplificationProof
    deriving (Show, Eq)

{- | The concrete monad in which simplification occurs.

  'increment' accesses a 'Counter' for generating fresh variables.

  'empty' indicates an expected error, e.g. the selected simplification rule
  does not apply.  '<|>' composes two simplification rules by keeping the first
  rule which succeeds; when a rule fails, the 'Counter' is automatically reset
  before trying the next rule.

 -}
newtype Simplifier a =
    Simplifier
    { getSimplifier :: State Natural a
    }
  deriving (Applicative, Functor, Monad)

deriving instance MonadState Natural Simplifier

instance MonadCounter Simplifier

{- | Lift a computation in the 'Counter' monad into the 'Simplifier' monad.

  The salient difference is that 'Counter' does not encompass failure, so this
  is a faithful lifting.
 -}
liftCounter :: Counter a -> Simplifier a
liftCounter counting = do
    counter0 <- Monad.State.get
    let (a, !counter1) = runCounter counting counter0
    Monad.State.put counter1
    return a

{- | Run a simplifier computation.

  The result (or error) is returned along with the final 'Counter'. A 'Left'
  result indicates an error terminating simplification, while a result @Right
  Nothing@ indicates that the simplifier did not apply (but no error was
  encountered).

 -}
runSimplifier
    :: Simplifier a
    -> Natural
    -> (a, Natural)
runSimplifier simplifier =
    runState (getSimplifier simplifier)

{- | Evaluate a simplifier computation.

  Only the result (or error) is returned. A result 'Nothing' indicates that the
  simplifier did not apply, but did not encounter an unexpected error. The
  'Counter' is discarded.

  -}
evalSimplifier :: Simplifier a -> a
evalSimplifier simplifier =
    fst $ runSimplifier simplifier 0

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
