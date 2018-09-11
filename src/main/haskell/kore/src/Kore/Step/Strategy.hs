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

  The machine has a primary and secondary stack. The secondary stack is intended
  for exception handling. 'runMachine' runs the instructions of the primary
  instruction stack in sequence. 'throw' swaps the stacks.

 -}
data Machine instr accum =
    Machine
        { stackA :: ![instr]
          -- ^ primary instruction stack
        , stackB :: ![instr]
          -- ^ secondary instruction stack (for exceptions)
        , accum :: !accum
          -- ^ current accumulator
        }

-- | Push an instruction to the top of the primary stack.
pushA :: instr -> Machine instr accum -> Machine instr accum
pushA instr state@Machine { stackA } = state { stackA = instr : stackA }

-- | Pop an instruction from the top of the primary stack.
popA :: Machine instr accum -> Maybe (Machine instr accum, instr)
popA state@Machine { stackA } =
    case stackA of
        [] -> Nothing
        instr : stackA' -> Just (state { stackA = stackA' }, instr)

-- | Push an instruction to the top of the secondary stack.
pushB :: instr -> Machine instr accum -> Machine instr accum
pushB instr state@Machine { stackB } = state { stackB = instr : stackB }

-- | Clear the secondary stack.
clearB :: Machine instr accum -> Machine instr accum
clearB state = state { stackB = [] }

-- | Copy the primary stack over the secondary stack.
copyAB :: Machine instr accum -> Machine instr accum
copyAB state@Machine { stackA } = state { stackB = stackA }

{- | Signal an exception.

  The primary and secondary stacks will be swapped.

 -}
throw :: Machine instr accum -> Machine instr accum
throw state@Machine { stackA, stackB } =
    state { stackA = stackB, stackB = stackA }

-- | Return the accumulator.
get :: Machine instr accum -> accum
get Machine { accum } = accum

-- | Set the accumulator.
put :: Machine instr accum -> accum -> Machine instr accum
put state accum = state { accum }

{- | Run a simple state machine.

  The transition rule may allow branching. Returns a tree of all machine states.

 -}
runMachine
    :: Monad m
    => (instr -> Machine instr accum -> m [Machine instr accum])
    -- ^ Transition rule
    -> Machine instr accum
    -- ^ Initial state
    -> m (Tree (Machine instr accum))
runMachine transition =
    Tree.unfoldTreeM_BF runMachine0
  where
    runMachine0 state =
        case popA state of
            Nothing ->
                -- No more instructions, therefore no children.
                return (state, [])
            Just (state', instr) ->
                -- Transition to the next states based on the instruction at
                -- the top of the stack.
                (,) state <$> transition instr state'

{- | Transition rule for running a 'Strategy' 'Machine'.

  The primitive strategy rule is used to execute the 'Apply' strategy. The
  primitive rule is considered successful if it returns any children and
  considered failed if it returns no children.

 -}
strategyTransition
    :: Monad m
    => (prim -> config -> m [config])
    -- ^ Primitive strategy rule
    -> Strategy prim
    -> Machine (Strategy prim) config
    -> m [Machine (Strategy prim) config]
strategyTransition applyPrim =
    \strategy state ->
        case strategy of
            Apply prim strategy' -> do
                let state' = pushA strategy' state
                -- Apply a primitive strategy.
                configs <- applyPrim prim (get state)
                case configs of
                    [] ->
                        -- If the primitive failed, throw an exception.
                        return [ throw state' ]
                    _ ->
                        -- If the primitive succeeded, reset the exception
                        -- handler and continue with the children.
                        return (put (resetB state') <$> configs)
            And strategy1 strategy2 ->
                return [ pushA strategy1 state, pushA strategy2 state ]
            Done -> return []
            Stuck -> return []
            Or strategy1 strategy2 ->
                return [ (pushA strategy1 . pushA strategy . pushB strategy2 . copyAB) state ]
  where
    resetB = pushB stuck . clearB

{- | Execute a 'Strategy'.

  The primitive strategy rule is used to execute the 'Apply' strategy. The
  primitive rule is considered successful if it returns any children and
  considered failed if it returns no children.

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
    -> config
    -- ^ Initial configuration
    -> m (Tree ([Strategy prim], config))
runStrategy applyPrim strategy config =
    (<$>) annotateConfig <$> runMachine (strategyTransition applyPrim) state
  where
    state = Machine
        { stackA = [ strategy ]
        , stackB = [ stuck ]
        , accum = config
        }
    annotateConfig Machine { stackA, accum } = (stackA, accum)

{- | Pick the first 'Done' result from all the branches of a 'Tree'.

  See also: 'runStrategy'

 -}
pickFirst :: Tree ([Strategy prim], config) -> Maybe config
pickFirst =
    (<$>) getFirst . getOption . Tree.foldTree pickFirstAt

{- | Pick the longest-running branch from a 'Tree'.

  See also: 'runStrategy'

 -}
pickLongest :: Tree ([Strategy prim], config) -> config
pickLongest =
    getLongest . Tree.foldTree pickLongestAt

{- | Pick the first 'Done' result from a 'Tree', or the longest-running branch.

  The longest-running branch is returned if no branch reaches 'Done'. The tree
  is traversed only once.

  See also: 'pickFirst', 'pickLongest'

 -}
pickFirstOrLongest :: Tree ([Strategy prim], config) -> config
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
    :: ([Strategy prim], config)
    -> [Option (First config)]
    -> Option (First config)
pickFirstAt (stack, config) children =
    let
        this =
            case stack of
                Done : _ -> pure (First config)
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
pickLongestAt :: (a1, a2) -> [Longest a2] -> Longest a2
pickLongestAt (_, config) children =
    sconcat (longest config :| (longer <$> children))

{- | Pick the first and longest results at one node of a tree.

  'pickFirstOrLongest' unfolds @pickFirstOrLongestAt@ over an entire tree.

 -}
pickFirstOrLongestAt
    :: ([Strategy prim], config)
    -> [(Option (First config), Longest config)]
    -> (Option (First config), Longest config)
pickFirstOrLongestAt node (unzip -> (firstChildren, longestChildren)) =
    (pickFirstAt node firstChildren, pickLongestAt node longestChildren)
