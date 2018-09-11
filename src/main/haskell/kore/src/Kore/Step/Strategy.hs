module Kore.Step.Strategy
    ( -- * Strategies
      Strategy
    , apply
    , done
    , stuck
    , and
    , all
    , or
    , any
    , many
    , some
      -- * Primitive strategies
    , Prim
    , axiom
    , builtin
      -- * Running strategies
    , runStrategy
    , pickFirst
    , pickLongest
    , pickFirstOrLongest
    ) where

import           Data.Bifunctor
                 ( first )
import           Data.List.NonEmpty
                 ( NonEmpty (..) )
import           Data.Maybe
                 ( fromMaybe )
import           Data.Semigroup
import           Data.Tree
                 ( Tree )
import qualified Data.Tree as Tree
import           Prelude hiding
                 ( all, and, any, or )

import Control.Monad.Counter

{- | An execution strategy.

  @Strategy prim@ represents a strategy for execution by applying rewrite axioms
  of type @prim@.

 -}
data Strategy prim where

    -- The recursive arguments of these constructors are /intentionally/ lazy to
    -- allow strategies to loop.

    Apply :: !prim -> Strategy prim -> Strategy prim

    And :: Strategy prim -> Strategy prim -> Strategy prim

    Done :: Strategy prim

    Stuck :: Strategy prim

    Or :: Strategy prim -> Strategy prim -> Strategy prim

-- | Apply a rewrite axiom.
apply
    :: app
    -- ^ rule
    -> Strategy app
    -- ^ next strategy
    -> Strategy app
apply = Apply

-- | Successfully terminate execution.
done :: Strategy app
done = Done

-- | Unsuccessfully terminate execution.
stuck :: Strategy app
stuck = Stuck

-- | Apply two strategies in parallel.
and :: Strategy app -> Strategy app -> Strategy app
and = And

{- | Apply all of the strategies in parallel.

  @
  all [] === stuck
  @

 -}
all :: [Strategy app] -> Strategy app
all = foldr and stuck

-- | Apply the second strategy if the first fails.
or :: Strategy app -> Strategy app -> Strategy app
or = Or

{- | Apply the given strategies until one succeeds.

  @
  any [] === stuck
  @

 -}
any :: [Strategy app] -> Strategy app
any = foldr or stuck

-- | Apply the strategy zero or more times.
many :: (Strategy app -> Strategy app) -> Strategy app -> Strategy app
many strategy finally = many0
  where
    many0 = or (strategy many0) finally

-- | Apply the strategy one or more times.
some :: (Strategy app -> Strategy app) -> Strategy app -> Strategy app
some strategy finally = strategy (many strategy finally)

{- | A strategy primitive: a rewrite axiom or builtin simplification step.
 -}
data Prim axiom = Builtin | Axiom !axiom

-- | Apply the axiom.
axiom :: axiom -> Prim axiom
axiom = Axiom

-- | Apply builtin simplification.
builtin :: Prim axiom
builtin = Builtin

{- | A simple state machine for running 'Strategy'.

  The machine has a primary and secondary instruction pointer and an
  accumulator. The secondary instruction is intended for exception handling.

 -}
data Machine instr accum =
    Machine
        { instrA :: !instr
        -- ^ primary instruction pointer
        , instrB :: !instr
        -- ^ secondary instruction pointer (for exceptions)
        , accum :: !accum
        -- ^ current accumulator
        , stepCount :: !Natural
        -- ^ current step count
        }

-- | Take a step, i.e. increment the step counter.
step :: Machine instr accum -> Machine instr accum
step state@Machine { stepCount } = state { stepCount = succ stepCount }

{- | An optionally-limited quantity.
 -}
data Limit a
    = Unlimited
    -- ^ No limit
    | Limit !a
    -- ^ Limit @a@ by the given (inclusive) upper bound
    deriving (Eq)

instance Ord a => Ord (Limit a) where
    compare =
        \case
            Unlimited ->
                \case
                    Unlimited -> EQ
                    Limit _ -> GT
            Limit a ->
                \case
                    Unlimited -> LT
                    Limit b -> compare a b

{- | Is the given value within the (inclusive) upper bound?
 -}
withinLimit :: Ord a => Limit a -> a -> Bool
withinLimit =
    \case
        Unlimited -> \_ -> True
        Limit u -> \a -> a <= u

{- | Run a simple state machine.

  The transition rule may allow branching. Returns a tree of all machine states.

 -}
runMachine
    :: Monad m
    => (Machine instr accum -> m [Machine instr accum])
    -- ^ Transition rule
    -> Limit Natural
    -- ^ Step limit
    -> Machine instr accum
    -- ^ Initial state
    -> m (Tree (Machine instr accum))
runMachine transition stepLimit =
    Tree.unfoldTreeM_BF runMachine0
  where
    runMachine0 state@Machine { stepCount } =
        let
            next
                | withinLimit stepLimit stepCount =
                  transition state
                | otherwise =
                  -- Take no more steps if the limit is exceeded.
                  pure []
        in
            (,) state <$> next

{- | Transition rule for running a 'Strategy' 'Machine'.

  The primitive strategy rule is used to execute the 'Apply' strategy. The
  primitive rule is considered successful if it returns any children and
  considered failed if it returns no children.

 -}
strategyTransition
    :: Monad m
    => (prim -> config -> m [config])
    -- ^ Primitive strategy rule
    -> Machine (Strategy prim) config
    -> m [Machine (Strategy prim) config]
strategyTransition applyPrim =
    \state@Machine { instrA, instrB, accum = config } ->
        case instrA of
            Done -> return []
            Stuck -> return []
            And instr1 instr2 ->
                -- Distribute the instructions to child branches.
                return
                    [ state { instrA = instr1 }
                    , state { instrA = instr2 }
                    ]
            Or instr1 instr2 ->
                -- Distribute the instructions to the primary and secondary
                -- instruction pointers.
                return
                    [ state
                        { instrA = instr1
                        -- If instr1 fails, try instr2 and finally instrB.
                        , instrB = or instr2 instrB
                        }
                    ]
            Apply prim instrA' -> do
                let state' = step state { instrA = instrA' }
                -- Apply a primitive strategy.
                configs <- applyPrim prim config
                case configs of
                    [] ->
                        -- If the primitive failed, throw an exception.
                        -- Reset the exception handler so we do not loop.
                        return [ throw state' ]
                    _ -> do
                        -- If the primitive succeeded, reset the exception
                        -- handler and continue with the children.
                        let next accum = state' { accum, instrB = stuck }
                        return (next <$> configs)
  where
    throw state@Machine { instrB } = state { instrA = instrB, instrB = stuck }

{- | Execute a 'Strategy'.

  The primitive strategy rule is used to execute the 'apply' strategy. The
  primitive rule is considered successful if it returns any children and
  considered failed if it returns no children. The given step limit is applied
  to the primitive strategy rule only; i.e. only 'apply' is considered a step,
  the other strategy combinators are "free".

  The resulting tree of configurations is annotated with the strategy stack at
  each node.

  See also: 'pickFirst'

 -}
runStrategy
    :: Monad m
    => (prim -> config -> m [config])
    -- ^ Primitive strategy rule
    -> Strategy prim
    -- ^ Strategy
    -> Limit Natural
    -- ^ Step limit
    -> config
    -- ^ Initial configuration
    -> m (Tree (Strategy prim, config))
runStrategy applyPrim strategy stepLimit config =
    (<$>) annotateConfig <$> runMachine transition stepLimit state
  where
    transition = strategyTransition applyPrim
    state = Machine
        { instrA = strategy
        , instrB = stuck
        , accum = config
        , stepCount = 0
        }
    annotateConfig Machine { instrA, accum } = (instrA, accum)

{- | Pick the first 'Done' result from all the branches of a 'Tree'.

  See also: 'runStrategy'

 -}
pickFirst :: Tree (Strategy prim, config) -> Maybe config
pickFirst =
    (<$>) getFirst . getOption . Tree.foldTree pickFirstAt

{- | Pick the longest-running branch from a 'Tree'.

  See also: 'runStrategy'

 -}
pickLongest :: Tree (Strategy prim, config) -> config
pickLongest =
    getLongest . Tree.foldTree pickLongestAt

{- | Pick the first 'Done' result from a 'Tree', or the longest-running branch.

  The longest-running branch is returned if no branch reaches 'Done'. The tree
  is traversed only once.

  See also: 'pickFirst', 'pickLongest'

 -}
pickFirstOrLongest :: Tree (Strategy prim, config) -> config
pickFirstOrLongest =
    getFirstOrLongest . Tree.foldTree pickFirstOrLongestAt
  where
    getFirstOrLongest (f, l) =
        fromMaybe
            (getLongest l)
            (getFirst <$> getOption f)

{- | Pick the first result at one node of a tree.

  'pickFirst' folds @pickFirstAt@ over an entire tree.

 -}
pickFirstAt
    :: (Strategy prim, config)
    -> [Option (First config)]
    -> Option (First config)
pickFirstAt (instr, config) children =
    let
        this =
            case instr of
                Done -> pure (First config)
                _ -> mempty
    in
        mconcat (this : children)

{- | A 'Semigroup' which returns its longest-running argument.

  See also: 'longest', 'longer'

 -}
newtype Longest a = Longest (Max (Arg Natural a))
    deriving (Semigroup)

getLongest :: Longest a -> a
getLongest (Longest (Max (Arg _ a))) = a

{- | Insert a value into 'Longest' at length 0.
 -}
longest :: a -> Longest a
longest a = Longest (Max (Arg 0 a))

{- | Increase the length of the argument.
 -}
longer :: Longest a -> Longest a
longer (Longest a) = Longest (first succ <$> a)

{- | Pick the longest-running branch at one node of a tree.

  'pickLongest' folds @pickLongestAt@ over an entire tree.

 -}
pickLongestAt :: (instr, config) -> [Longest config] -> Longest config
pickLongestAt (_, config) children =
    sconcat (longest config :| (longer <$> children))

{- | Pick the first and longest results at one node of a tree.

  'pickFirstOrLongest' unfolds @pickFirstOrLongestAt@ over an entire tree.

 -}
pickFirstOrLongestAt
    :: (Strategy prim, config)
    -> [(Option (First config), Longest config)]
    -> (Option (First config), Longest config)
pickFirstOrLongestAt node (unzip -> (firstChildren, longestChildren)) =
    (pickFirstAt node firstChildren, pickLongestAt node longestChildren)
