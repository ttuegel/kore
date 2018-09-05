module Kore.Step.Stepper
    ( Limit (..)
    , withinLimit
    , incrementStepCount
    , limitSteps
    , StepperError
    , StepperState (..)
    , initialStepperState
    , HeatCool (..)
    , isHeatingRule, isCoolingRule, isNormalRule
    , Stepper
    , runStepper, evalStepper
    , liftSimplifier
    , appliedRule
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
import           Control.Monad.Trans.Maybe
                 ( MaybeT )

import Control.Monad.Counter
import Kore.Error
       ( Error )
import Kore.Step.AxiomPatterns
       ( HeatCool (..), isCoolingRule, isHeatingRule, isNormalRule )
import Kore.Step.Simplification.Data
       ( Simplifier, runSimplifier )

{- | A limit on a quantity in @a@.

  Rather than a finite limit, the quantity may also be unlimited.

 -}
data Limit a
    = Limit !a
      -- ^ Inclusive upper bound
    | Unlimited
      -- ^ No limit
  deriving (Eq)

instance Ord a => Ord (Limit a) where
    compare (Limit a) (Limit b) = compare a b
    compare Unlimited (Limit _) = GT
    compare (Limit _) Unlimited = LT
    compare Unlimited Unlimited = EQ

{- | Is the given value within the limit?

  The upper bound on 'Limit' is considered inclusive.

 -}
withinLimit :: Ord a => Limit a -> a -> Bool
withinLimit Unlimited _ = True
withinLimit (Limit a) b = b <= a

{- | Increment the step counter carried by 'Stepper'.

  The new value is returned.

 -}
incrementStepCount :: Stepper Natural
incrementStepCount = do
    StepperState { stepperStepCount = count0 } <- Monad.State.get
    let count1 = succ count0
        increment0 state = state { stepperStepCount = count1 }
    Monad.State.modify' increment0
    return count1

{- | Modify a 'Stepper' to allow a limited number of steps.

  If the step limit is exceeded, the resulting stepper returns its argument
  unmodified.

 -}
limitSteps
    :: Limit Natural
    -- ^ limit
    -> (a -> Stepper a)
    -- ^ stepper
    -> (a -> Stepper a)
limitSteps stepLimit step a = do
    n <- incrementStepCount
    if withinLimit stepLimit n then step a else pure a

{- | A tag for the type of error thrown in 'Stepper'.

  See also: 'Kore.Error.koreFail'

 -}
data StepperError

{- | The state carried by 'Stepper'.
 -}
data StepperState =
    StepperState
        { stepperCounter :: !Natural
          -- ^ 'Counter' for fresh variable generation
        , stepperStepCount :: !Natural
          -- ^ Number of steps taken
        , stepperLastApplied :: !(Maybe HeatCool)
          -- ^ Mark if the last applied rule was a 'Heat' or 'Cool' rule (or
          -- neither) to detect if we are stuck.
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

initialStepperState :: StepperState
initialStepperState =
    StepperState
        { stepperCounter = 0
        , stepperStepCount = 0
        , stepperLastApplied = Nothing
        }

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
evalStepper :: Stepper a -> Either (Error StepperError) a
evalStepper stepper =
    fst <$> runStepper stepper initialStepperState

{- | Lift a 'Simplifier' into the 'Stepper' monad.

  The @Simplifier@ will inherit the current 'Counter', which will be
  updated after the @Simplifier@ returns.

 -}
liftSimplifier :: Simplifier a -> Stepper a
liftSimplifier simplifier = do
    stepperState0@StepperState { stepperCounter = counter0 } <- Monad.State.get
    let (result, counter1) = runSimplifier simplifier counter0
    Monad.State.put (stepperState0 { stepperCounter = counter1 })
    return result

appliedRule :: Maybe HeatCool -> MaybeT Stepper ()
appliedRule applied =
    Monad.State.modify' (\state -> state { stepperLastApplied = applied })
