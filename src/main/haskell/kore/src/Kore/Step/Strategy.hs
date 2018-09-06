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
import           Data.Map
                 ( Map )
import           Data.Semigroup
                 ( Semigroup (..) )
import           Data.Tree
                 ( Tree )
import qualified Data.Tree as Tree
import           Prelude hiding
                 ( seq )

import           Control.Monad.Counter
import           Kore.AST.Common
                 ( Id )
import           Kore.AST.MetaOrObject
                 ( MetaOrObject )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.AxiomPatterns
                 ( AxiomPattern )
import           Kore.Step.BaseStep
                 ( StepProof, simplificationProof, stepWithAxiom )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern )
import           Kore.Step.Function.Data
                 ( CommonApplicationFunctionEvaluator )
import qualified Kore.Step.OrOfExpandedPattern as MultiOr
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

{- | An execution strategy.

  @Strategy app@ represents a strategy for execution by applying rewrite axioms
  of type @app@.

  Notes:

  - The recursive arguments of constructors are /intentionally/ lazy to allow
    strategies to loop.

 -}
data
    Strategy
        app  -- rewrite rules
  where
    -- | Apply a rewrite axiom.
    Apply
        :: !app
        -- ^ rule
        -> Strategy app

    -- | Use builtin simplification, including function application.
    Simplify :: Strategy app

    Seq :: Strategy app -> Strategy app -> Strategy app

    Par :: Strategy app -> Strategy app -> Strategy app

    Done :: Strategy app

    Stuck :: Strategy app

    Many :: Strategy app -> Strategy app

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
data StrategyEnv app proof a =
    StrategyEnv
        { stack :: ![Strategy app]
          -- ^ next strategy to attempty
        , config :: !a
          -- ^ current configuration
        , proof :: !proof
          -- ^ proof of current configuration
        }

pushStrategy :: Monad m => Strategy app -> StateT (StrategyEnv app proof a) m ()
pushStrategy strategy =
    Monad.State.modify' pushStrategy0
  where
    pushStrategy0 state@StrategyEnv { stack } =
        state { stack = strategy : stack }

{- | Use a strategy to execute the given pattern.

  Returns a tree of execution paths.

 -}
runStrategy
    :: MetaOrObject level
    => Strategy (AxiomPattern level)
    -- ^ Strategy to apply
    -> MetadataTools level StepperAttributes
    -- ^ Metadata for the execution scope
    -> Map (Id level) [CommonApplicationFunctionEvaluator level]
    -- ^ Function evaluators indexed by identifier
    -> CommonExpandedPattern level
    -- ^ Initial configuration
    -> Counter (Tree (CommonExpandedPattern level, StepProof level))
runStrategy strategy0 tools functions config0 =
    Tree.unfoldTreeM_BF runStrategy0 env0
  where
    env0 =
        StrategyEnv
            { stack = [ strategy0 ]
            , proof = mempty
            , config = config0
            }

    runStrategy0 env1@StrategyEnv { stack = stack1, proof, config } =
        let node = (config, proof)
        in case stack1 of
          [] -> pure (node, [])
          strategy : stack2 ->
              let env2 = env1 { stack = stack2 }
              in (,) node <$> evalStateT (childrenOf strategy) env2

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
            env@StrategyEnv { config = config1, proof = proof1 }
              <-
                Monad.State.get
            (configs, proof2) <-
                Monad.Trans.lift
                    $ ExpandedPattern.simplify tools functions config1
            let
                proof = proof1 <> simplificationProof proof2
                child config = env { proof, config }
                children = child <$> MultiOr.extractPatterns configs
            pure children

    childrenApply axiom =
        do
            env@StrategyEnv { config = config1, proof = proof1 }
              <-
                Monad.State.get
            case stepWithAxiom tools config1 axiom of
                Left _ ->
                    -- This branch is stuck because the axiom did not apply.
                    pure []
                Right applied -> do
                    -- Continue execution along this branch.
                    (config, proof2) <- Monad.Trans.lift applied
                    let proof = proof1 <> proof2
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
