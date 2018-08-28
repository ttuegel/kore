module Kore.Step.Stepper
    ( MaxStepCount (..)
    , predMaxStepCount
    , StepperError
    , StepperState (..)
    , Stepper
    , runStepper, evalStepper
    , liftSimplifier
    , decrementStepCount
    ) where

import           Control.Monad.Error.Class
                 ( MonadError )
import           Control.Monad.Except
                 ( Except, runExcept )
import qualified Control.Monad.Except as Monad.Except
import           Control.Monad.State.Class
                 ( MonadState )
import           Control.Monad.State.Strict
                 ( StateT, runStateT )
import qualified Control.Monad.State.Strict as Monad.State
import           Data.Bifunctor
                 ( bimap )

import           Control.Monad.Counter
import           Kore.Error
                 ( Error )
import qualified Kore.Error
import           Kore.Step.Simplification.Data
                 ( Simplifier, runSimplifier )

{- | The maximum number of steps that the 'Stepper' is allowed to take.

  Either an 'Interger', or the step count is unlimited.

 -}
data MaxStepCount
    = MaxStepCount Integer
    | AnyStepCount

{- | Reduce a 'MaxStepCount' by one step, if possible.

  The step count must remain non-negative, or 'Nothing' is returned. An
  unlimited step count remains unlimited.

  See also: 'pred'

 -}
predMaxStepCount :: MaxStepCount -> Maybe MaxStepCount
predMaxStepCount AnyStepCount = Just AnyStepCount
predMaxStepCount (MaxStepCount n)
    | n > 0 = Just (MaxStepCount (n - 1))
    | otherwise = Nothing

{- | A tag for the type of error thrown in 'Stepper'.

  See also: 'Kore.Error.koreFail'

 -}
data StepperError

{- | The state carried by 'Stepper'.
 -}
data StepperState =
    StepperState
        { stepperCounter :: !Counter
        , stepperMaxStepCount :: !MaxStepCount
        }

{- | A computation which carries out Kore execution steps.
 -}
newtype Stepper a =
    Stepper
      { getStepper
          :: StateT StepperState (Except (Error StepperError)) a
      }
  deriving (Applicative, Functor, Monad)

instance MonadState StepperState Stepper where
    state f = Stepper (Monad.State.state f)

instance MonadCounter Stepper where
    increment = do
        StepperState { stepperCounter } <- Monad.State.get
        let increment0 state =
                state { stepperCounter = succ stepperCounter }
        Monad.State.modify' increment0
        return stepperCounter

instance MonadError (Error StepperError) Stepper where
    throwError e = Stepper (Monad.Except.throwError e)
    catchError a h =
        Stepper (Monad.Except.catchError a' h')
      where
        a' = getStepper a
        h' e = getStepper (h e)

{- | Run a 'Stepper' computation using the given initial state.

  The final result and state (or error) is returned.

 -}
runStepper
    :: Stepper a
    -> StepperState
    -> Either (Error StepperError) (a, StepperState)
runStepper stepper state0 =
    runExcept (runStateT (getStepper stepper) state0)

{- | Evaluate a 'Stepper' computation using the given 'MaxStepCount'.

  The 'Counter' is initialized to zero. Only the final result (or error) is
  returned.

  See also: 'runStepper'

 -}
evalStepper :: Stepper a -> MaxStepCount -> Either (Error StepperError) a
evalStepper stepper maxSteps =
    let
        state0 =
            StepperState
                { stepperCounter = Counter 0
                , stepperMaxStepCount = maxSteps
                }
    in
      bimap id fst (runStepper stepper state0)

{- | Lift a 'Simplifier' into the 'Stepper' monad.

  If the @Simplifier@ throws an error, it will be cast into the appropriate
  error type. The @Simplifier@ will inherit the current 'Counter', which will be
  updated after the @Simplifier@ returns.

  See also: 'Kore.Error.castError'

 -}
liftSimplifier :: Simplifier a -> Stepper a
liftSimplifier simplifier = do
    stepperState0@StepperState { stepperCounter } <- Monad.State.get
    let
        counter0 = stepperCounter
        (result, counter1) = runSimplifier simplifier counter0
    Monad.State.put (stepperState0 { stepperCounter = counter1 })
    either Monad.Except.throwError return (Kore.Error.castError result)

{- | Reduce the step count by one step and return the new count.

  Return 'Nothing' if the maximum step count has been exceeded; in this case the
  stepper should terminate.

  See also: 'predMaxStepCount'

 -}
decrementStepCount :: Stepper (Maybe MaxStepCount)
decrementStepCount = do
    StepperState { stepperMaxStepCount = count0 } <- Monad.State.get
    case predMaxStepCount count0 of
        Nothing -> return Nothing
        Just count1 -> do
            let decrement0 state =
                    state { stepperMaxStepCount = count1 }
            Monad.State.modify' decrement0
            return (Just count1)
