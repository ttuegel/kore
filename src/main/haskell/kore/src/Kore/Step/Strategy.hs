module Kore.Step.Strategy
    ( Strategy
    , apply
    , simplify
    , done
    , stuck
    , seq
    , par
    , runStrategy
    ) where

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
              in case strategy of
                  Seq strategy1 strategy2 ->
                      (,) node <$> childrenSeq env2 strategy1 strategy2
                  Par strategy1 strategy2 ->
                      (,) node <$> childrenPar env2 strategy1 strategy2
                  Apply axiom ->
                      (,) node <$> childrenApply env2 axiom
                  Simplify ->
                      (,) node <$> childrenSimplify env2
                  Done ->
                      pure (node, [])
                  Stuck ->
                      pure (node, [])

    childrenSeq env@StrategyEnv { stack } strategy1 strategy2 =
        pure [ env { stack = strategy1 : strategy2 : stack } ]

    childrenPar env@StrategyEnv { stack } strategy1 strategy2 =
        pure
            [ env { stack = strategy1 : stack }
            , env { stack = strategy2 : stack }
            ]

    childrenSimplify env@StrategyEnv { config = config1, proof = proof1 } =
        do
            (configs, proof2) <-
                ExpandedPattern.simplify tools functions config1
            let
                proof = proof1 <> simplificationProof proof2
                child config = env { proof, config }
                children = child <$> MultiOr.extractPatterns configs
            pure children

    childrenApply env@StrategyEnv { config = config1, proof = proof1 } axiom =
        case stepWithAxiom tools config1 axiom of
            Left _ ->
                -- This branch is stuck because the axiom did not apply.
                pure []
            Right applied -> do
                -- Continue execution along this branch.
                (config, proof2) <- applied
                let proof = proof1 <> proof2
                    children = [ env { proof, config } ]
                pure children
