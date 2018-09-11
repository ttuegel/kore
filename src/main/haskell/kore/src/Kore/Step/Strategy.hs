module Kore.Step.Strategy
    ( Strategy
    , apply
    , done
    , stuck
    , seq
    , sequence
    , par
    , parallel
    , many
    , Prim
    , axiom
    , builtin
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
                 ( seq, sequence )

import Control.Monad.Counter

{- | An execution strategy.

  @Strategy app@ represents a strategy for execution by applying rewrite axioms
  of type @app@.

  Notes:

  - The recursive arguments of constructors are /intentionally/ lazy to allow
    strategies to loop.

 -}
data
    Strategy
        prim  -- primitive rewrite rules
  where
    -- | Apply a rewrite axiom.
    Apply
        :: !prim
        -- ^ rule
        -> Strategy prim

    Seq :: Strategy prim -> Strategy prim -> Strategy prim

    Par :: Strategy prim -> Strategy prim -> Strategy prim

    Done :: Strategy prim

    Stuck :: Strategy prim

    Many :: Strategy prim -> Strategy prim

-- | Apply a rewrite axiom.
apply
    :: app
    -- ^ rule
    -> Strategy app
apply = Apply

-- | Successfully terminate execution.
done :: Strategy app
done = Done

-- | Unsuccessfully terminate execution.
stuck :: Strategy app
stuck = Stuck

-- | Apply two strategies in sequence.
seq :: Strategy app -> Strategy app -> Strategy app
seq = Seq

-- | Apply many strategies in sequence.
sequence :: [Strategy app] -> Strategy app
sequence = foldr seq done

-- | Apply two strategies in parallel.
par :: Strategy app -> Strategy app -> Strategy app
par = Par

-- | Apply many strategies in parallel.
parallel :: [Strategy app] -> Strategy app
parallel = foldr par stuck

-- | Apply the strategy zero or more times.
many :: Strategy app -> Strategy app
many = Many

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
            Apply prim -> do
                -- Apply a primitive strategy.
                configs <- applyPrim prim (get state)
                case configs of
                    [] ->
                        -- If the primitive failed, throw an exception.
                        return [ throw state ]
                    _ ->
                        -- If the primitive succeeded, reset the exception
                        -- handler and continue with the children.
                        return (put (resetB state) <$> configs)
            Seq strategy1 strategy2 ->
                return [ (pushA strategy1 . pushA strategy2) state ]
            Par strategy1 strategy2 ->
                return [ pushA strategy1 state, pushA strategy2 state ]
            Done -> return []
            Stuck -> return []
            Many strategy1 ->
                return [ (pushA strategy1 . pushB strategy . copyAB) state ]
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
