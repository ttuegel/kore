module Kore.Step.Strategy
    ( Strategy
    , apply
    , simplify
    , done
    , and
    , runStrategy
    ) where

import qualified Data.Foldable as Foldable
import           Data.Map
                 ( Map )
import           Data.Semigroup
                 ( Semigroup (..) )
import           Data.Tree
                 ( Tree )
import qualified Data.Tree as Tree
import           Prelude hiding
                 ( and )

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

  - Sequencing is implicit: some constructors take a @Strategy@ argument which
    is the next strategy to apply, while other constructors (such as 'Done')
    represent termination.

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
        -- ^ next strategy
        -> Strategy app

    -- | Use builtin simplification, including function application.
    Simplify
        :: Strategy app
        -- ^ next strategy
        -> Strategy app

    -- | Successfully terminate execution.
    Done :: Strategy app

    -- | Attempt both strategies in parallel.
    And
        :: Strategy app
        -> Strategy app
        -> Strategy app

-- | Apply a rewrite axiom.
apply
    :: app
    -- ^ rule
    -> Strategy app
    -- ^ next strategy
    -> Strategy app
apply = Apply

-- | Use builtin simplification, including function application.
simplify
    :: Strategy app
    -- ^ next strategy
    -> Strategy app
simplify = Simplify

-- | Successfully terminate execution.
done :: Strategy app
done = Done

-- | Attempt both strategies in parallel.
and :: Strategy app -> Strategy app -> Strategy app
and = And

{- | The environment for executing a @Strategy app@.

  'runStrategy' unfolds a tree to produce patterns @a@ and a proofs @proof@.

 -}
data StrategyEnv app proof a =
    StrategyEnv
        { strategy :: !(Strategy app)
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
            { strategy = strategy0
            , proof = mempty
            , config = config0
            }

    runStrategy0 StrategyEnv { strategy, proof, config } =
        do
            let node = (config, proof)
            case strategy of
                Done -> runStrategyDone node
                Simplify next -> runStrategySimplify node next
                Apply axiom next -> runStrategyApply node axiom next
                And strategy1 strategy2 -> runStrategyAnd node strategy1 strategy2

    runStrategyDone node =
        pure (node, [])

    runStrategySimplify node@(config1, proof1) strategy =
        do
            (configs, proof2) <-
                ExpandedPattern.simplify tools functions config1
            let
                proof = proof1 <> simplificationProof proof2
                child config = StrategyEnv { strategy, proof, config }
                children = child <$> MultiOr.extractPatterns configs
            pure (node, children)

    runStrategyApply node@(config1, proof1) axiom strategy =
        case stepWithAxiom tools config1 axiom of
            Left _ ->
                -- This branch is stuck because the axiom did not apply.
                pure (node, [])
            Right applied -> do
                -- Continue execution along this branch.
                (config, proof2) <- applied
                let proof = proof1 <> proof2
                    children =
                        [ StrategyEnv { strategy, proof, config } ]
                pure (node, children)

    runStrategyAnd node@(config, proof) strategy1 strategy2 =
        do
            let
                strategies = [ strategy1, strategy2 ]
                child strategy = StrategyEnv { strategy, proof, config }
                children = Foldable.toList (child <$> strategies)
            pure (node, children)
