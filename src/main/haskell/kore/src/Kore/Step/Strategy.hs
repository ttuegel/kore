module Kore.Step.Strategy
    ( Strategy
    , apply
    , simplify
    , done
    , stuck
    , seq
    , par
    , many
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
                 ( seq )

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
    -- | Primly a rewrite axiom.
    Apply
        :: !prim
        -- ^ rule
        -> Strategy prim

    -- | Use builtin simplification, including function primlication.
    Simplify :: Strategy prim

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

-- | Use builtin simplification, including function application.
simplify :: Strategy app
simplify = Simplify

-- | Successfully terminate execution.
done :: Strategy app
done = Done

-- | Unsuccessfully terminate execution.
stuck :: Strategy app
stuck = Stuck

-- | Apply two strategies in sequence.
seq :: Strategy app -> Strategy app -> Strategy app
seq = Seq

-- | Apply two strategies in parallel.
par :: Strategy app -> Strategy app -> Strategy app
par = Par

-- | Apply the strategy zero or more times.
many :: Strategy app -> Strategy app
many = Many

{- | The environment for executing a @Strategy app@.

  'runStrategy' unfolds a tree to produce patterns @a@ and a proofs @proof@.

 -}
data Step prim proof a =
    StrategyEnv
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
    pushStrategy0 state@StrategyEnv { stack } =
        state { stack = strategy : stack }

{- | Use a strategy to execute the given pattern.

  Returns a tree of execution paths.

 -}
runStrategy
    :: (Foldable f, Monoid proof)
    => (tools -> functions -> config -> Counter (f config, proof))
    -- ^ Simplifier
    -> (tools -> config -> prim -> Either e (Counter (config, proof)))
    -- ^ Axiom application
    -> Strategy prim
    -- ^ Strategy to apply
    -> tools
    -- ^ Metadata for the execution scope
    -> functions
    -- ^ Function evaluators indexed by identifier
    -> config
    -- ^ Initial configuration
    -> Counter (Tree (Step prim proof config))
runStrategy doSimplify doApply strategy0 tools functions config0 =
    Tree.unfoldTreeM_BF runStrategy0 env0
  where
    env0 =
        StrategyEnv
            { stack = [ strategy0 ]
            , proof = mempty
            , config = config0
            }

    runStrategy0 env1@StrategyEnv { stack = stack1 } =
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
            Apply axiom ->
                childrenApply axiom
            Simplify ->
                childrenSimplify
            Many strategy1 ->
                childrenMany strategy1
            Done ->
                pure []
            Stuck ->
                pure []

    childrenSeq strategy1 strategy2 =
        do
            pushStrategy strategy2
            childrenOf strategy1

    childrenPar strategy1 strategy2 =
        do
            children1 <- childrenOf strategy1
            children2 <- childrenOf strategy2
            pure (children1 ++ children2)

    childrenSimplify =
        do
            env <- Monad.State.get
            let StrategyEnv { config = config1, proof = proof1 } = env
            (configs, proof2) <-
                Monad.Trans.lift $ doSimplify tools functions config1
            let
                proof = mappend proof1 proof2
                child config = env { proof, config }
                children = child <$> Foldable.toList configs
            pure children

    childrenApply axiom =
        do
            env <- Monad.State.get
            let StrategyEnv { config = config1, proof = proof1 } = env
            case doApply tools config1 axiom of
                Left _ ->
                    -- This branch is stuck because the axiom did not apply.
                    pure []
                Right applied -> do
                    -- Continue execution along this branch.
                    (config, proof2) <- Monad.Trans.lift applied
                    let proof = mappend proof1 proof2
                        children = [ env { proof, config } ]
                    pure children

    childrenMany strategy =
        do
            env1 <- Monad.State.get
            pushStrategy (many strategy)
            children <- childrenOf strategy
            case children of
                [] -> pure [ env1 ]
                _ : _ -> pure children
