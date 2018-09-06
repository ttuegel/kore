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
    , Step (..)
    , runStrategy
    ) where

import           Control.Monad.State.Strict
                 ( StateT, evalStateT )
import qualified Control.Monad.State.Strict as Monad.State
import qualified Control.Monad.Trans as Monad.Trans
import qualified Data.Foldable as Foldable
import           Data.Tree
                 ( Tree )
import qualified Data.Tree as Tree
import           Prelude hiding
                 ( seq, sequence )

import           Control.Monad.Counter

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

{- | The environment for executing a @Strategy app@.

  'runStrategy' unfolds a tree to produce patterns @a@ and a proofs @proof@.

 -}
data Step prim proof a =
    Step
        { stack :: ![Strategy prim]
          -- ^ next strategy to attempty
        , config :: !a
          -- ^ current configuration
        , proof :: !proof
          -- ^ proof of current configuration
        }

pushStrategy :: Monad m => Strategy prim -> StateT (Step prim proof a) m ()
pushStrategy strategy =
    Monad.State.modify' pushStrategy0
  where
    pushStrategy0 state@Step { stack } =
        state { stack = strategy : stack }

{- | Use a strategy to execute the given pattern.

  Returns a tree of execution paths.

 -}
runStrategy
    :: (Foldable f, Monoid proof)
    => (config -> prim -> Either e (Counter (f config, proof)))
    -- ^ Primitive rewrite rule application
    -> Strategy prim
    -- ^ Strategy to apply
    -> config
    -- ^ Initial configuration
    -> Counter (Tree (Step prim proof config))
runStrategy doApply strategy0 config0 =
    Tree.unfoldTreeM_BF runStrategy0 env0
  where
    env0 =
        Step
            { stack = [ strategy0 ]
            , proof = mempty
            , config = config0
            }

    runStrategy0 env1@Step { stack = stack1 } =
        case stack1 of
            [] -> pure (env1, [])
            strategy : stack2 ->
                let env2 = env1 { stack = stack2 }
                in (,) env1 <$> evalStateT (childrenOf strategy) env2

    childrenOf strategy =
        case strategy of
            Seq strategy1 strategy2 ->
                childrenSeq strategy1 strategy2
            Par strategy1 strategy2 ->
                childrenPar strategy1 strategy2
            Apply axiom_ ->
                childrenApply axiom_
            Many strategy1 ->
                childrenMany strategy1
            Done ->
                childrenDone
            Stuck ->
                childrenStuck

    -- | Do nothing and proceed with the next step.
    childrenDone =
        (: []) <$> Monad.State.get

    -- | Do nothing and do not continue.
    childrenStuck =
        pure []

    -- | Push the second strategy onto the stack and continue with the first
    -- strategy.
    childrenSeq strategy1 strategy2 =
        do
            pushStrategy strategy2
            childrenOf strategy1

    -- | Combine the children of both strategies.
    childrenPar strategy1 strategy2 =
        do
            children1 <- childrenOf strategy1
            children2 <- childrenOf strategy2
            pure (children1 ++ children2)

    childrenApply axiom_ =
        do
            env <- Monad.State.get
            let Step { config = config1, proof = proof1 } = env
            case doApply config1 axiom_ of
                Left _ ->
                    -- This branch is stuck because the axiom did not apply.
                    pure []
                Right applied -> do
                    -- Continue execution along this branch.
                    (configs, proof2) <- Monad.Trans.lift applied
                    let proof = mappend proof1 proof2
                        child config = env { proof, config }
                        children = child <$> Foldable.toList configs
                    pure children

    -- | Push @many strategy@ back onto the stack and attempt the strategy.
    -- If the strategy fails, roll the stack back and continue.
    childrenMany strategy =
        do
            env1 <- Monad.State.get
            pushStrategy (many strategy)
            children <- childrenOf strategy
            case children of
                [] -> pure [ env1 ]
                _ : _ -> pure children
