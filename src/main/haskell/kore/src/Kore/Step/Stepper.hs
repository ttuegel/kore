module Kore.Step.Stepper
    ( Limit (..)
    , withinLimit
    , incrementStepCount
    , limitSteps
    , StepperState (..)
    , initialStepperState
    , HeatCool (..)
    , isHeatingRule, isCoolingRule, isNormalRule
    , Stepper
    , runStepper, evalStepper
    , liftSimplifier
    , appliedRule
    , parallel
    , chooseAny
    , manySteps
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Monad
                 ( MonadPlus (..) )
import           Control.Monad.State.Class
                 ( MonadState )
import           Control.Monad.State.Strict
                 ( StateT (..), runStateT )
import qualified Control.Monad.State.Strict as Monad.State
import qualified Control.Monad.Trans as Monad.Trans
import           Control.Monad.Trans.Maybe
                 ( MaybeT )
import           ListT
                 ( ListT (..) )
import qualified ListT

import Control.Monad.Counter
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
    -> Stepper a
    -- ^ stepper
    -> Stepper a
limitSteps stepLimit step = do
    n <- incrementStepCount
    if withinLimit stepLimit n then step else empty

{- | The state carried by 'Stepper'.
 -}
data StepperState =
    StepperState
        { stepperStepCount :: !Natural
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
          :: StateT StepperState (ListT Simplifier) a
      }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus)

instance MonadState StepperState Stepper where
    state f = Stepper (Monad.State.state f)

instance MonadCounter Stepper where
    increment = liftSimplifier increment

initialStepperState :: StepperState
initialStepperState =
    StepperState
        { stepperStepCount = 0
        , stepperLastApplied = Nothing
        }

{- | Run a 'Stepper' computation using the given initial state.

  The final result and state (or error) is returned.

 -}
runStepper
    :: Stepper a
    -> StepperState
    -> Natural
    -> ([(a, StepperState)], Natural)
runStepper stepper state0 =
    runSimplifier (ListT.toList $ runStateT (getStepper stepper) state0)

{- | Evaluate a 'Stepper' computation using the given 'MaxStepCount'.

  The 'Counter' is initialized to zero. Only the final result (or error) is
  returned.

  See also: 'runStepper'

 -}
evalStepper :: Stepper a -> [a]
evalStepper stepper =
    fst <$> fst (runStepper stepper initialStepperState 0)

{- | Lift a 'Simplifier' into the 'Stepper' monad.

  The @Simplifier@ will inherit the current 'Counter', which will be
  updated after the @Simplifier@ returns.

 -}
liftSimplifier :: Simplifier a -> Stepper a
liftSimplifier simplifier =
    Stepper (Monad.Trans.lift (Monad.Trans.lift simplifier))

appliedRule :: Maybe HeatCool -> MaybeT Stepper ()
appliedRule applied =
    Monad.State.modify' (\state -> state { stepperLastApplied = applied })

{- | Consider several 'Stepper's in parallel.
 -}
parallel :: [Stepper a] -> Stepper a
parallel = foldr (<|>) empty

{- | Take the results of the first non-empty 'Stepper'.

  If the first argument returns any results, the second argument is ignored.

 -}
chooseAny :: Stepper a -> Stepper a -> Stepper a
chooseAny step1 step2 =
    Stepper $ StateT $ \s -> ListT $
        ListT.uncons (runStateT (getStepper step1) s) >>=
            \case
                Nothing -> ListT.uncons (runStateT (getStepper step2) s)
                Just r -> pure (Just r)

{- | Run the provided step zero or more times.
 -}
manySteps :: (a -> Stepper a) -> (a -> Stepper a)
manySteps step = \a -> chooseAny (step a >>= manySteps step) (pure a)
