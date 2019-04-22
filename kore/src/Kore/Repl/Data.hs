{-|
Module      : Kore.Repl.Data
Description : REPL data structures.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
-}

{-# LANGUAGE TemplateHaskell #-}

module Kore.Repl.Data
    ( ReplCommand (..)
    , helpText
    , ExecutionGraph
    , AxiomIndex (..), ClaimIndex (..)
    , ReplState (..)
    , InnerGraph
    , lensAxioms, lensClaims, lensClaim
    , lensGraph, lensNode, lensStepper
    , lensLabels, lensOmit, lensUnifier
    , lensCommands, shouldStore
    , UnifierWithExplanation (..)
    , runUnifierWithExplanation
    ) where

import           Control.Error
                 ( hush )
import qualified Control.Lens.TH.Rules as Lens
import           Control.Monad
                 ( join )
import           Control.Monad.Trans.Accum
                 ( AccumT )
import qualified Control.Monad.Trans.Accum as Monad.Accum
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree
                 ( Gr )
import           Data.Map.Strict
                 ( Map )
import           Data.Monoid
                 ( First (..) )
import           Data.Sequence
                 ( Seq )
import           Data.Text.Prettyprint.Doc
                 ( Doc )
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Kore.AST.Common
                 ( Variable )
import           Kore.AST.MetaOrObject
                 ( Object )
import           Kore.OnePath.Step
                 ( CommonStrategyPattern )
import           Kore.OnePath.Verification
                 ( Axiom (..) )
import           Kore.OnePath.Verification
                 ( Claim )
import           Kore.Step.Pattern
                 ( StepPattern )
import           Kore.Step.Rule
                 ( RewriteRule )
import           Kore.Step.Simplification.Data
                 ( Simplifier )
import qualified Kore.Step.Strategy as Strategy
import           Kore.Unification.Unify
                 ( MonadUnify, Unifier )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
                 ( unparse )

newtype AxiomIndex = AxiomIndex
    { unAxiomIndex :: Int
    } deriving (Eq, Show)

newtype ClaimIndex = ClaimIndex
    { unClaimIndex :: Int
    } deriving (Eq, Show)

-- | List of available commands for the Repl. Note that we are always in a proof
-- state. We pick the first available Claim when we initialize the state.
data ReplCommand
    = ShowUsage
    -- ^ This is the default action in case parsing all others fail.
    | Help
    -- ^ Shows the help message.
    | ShowClaim !Int
    -- ^ Show the nth claim.
    | ShowAxiom !Int
    -- ^ Show the nth axiom.
    | Prove !Int
    -- ^ Drop the current proof state and re-initialize for the nth claim.
    | ShowGraph
    -- ^ Show the current execution graph.
    | ProveSteps !Int
    -- ^ Do n proof steps from current node.
    | ProveStepsF !Int
    -- ^ Do n proof steps (through branchings) from the current node.
    | SelectNode !Int
    -- ^ Select a different node in the graph.
    | ShowConfig !(Maybe Int)
    -- ^ Show the configuration from the current node.
    | OmitCell !(Maybe String)
    -- ^ Adds or removes cell to omit list, or shows current omit list.
    | ShowLeafs
    -- ^ Show leafs which can continue evaluation and leafs which are stuck
    | ShowRule !(Maybe Int)
    -- ^ Show the rule(s) that got us to this configuration.
    | ShowPrecBranch !(Maybe Int)
    -- ^ Show the first preceding branch.
    | ShowChildren !(Maybe Int)
    -- ^ Show direct children of node.
    | Label !(Maybe String)
    -- ^ Show all node labels or jump to a label.
    | LabelAdd !String !(Maybe Int)
    -- ^ Add a label to a node.
    | LabelDel !String
    -- ^ Remove a label.
    | Redirect ReplCommand FilePath
    -- ^ Prints the output of the inner command to the file.
    | Try !(Either AxiomIndex ClaimIndex)
    -- ^ Attempt to apply axiom or claim to current node.
    | Clear !(Maybe Int)
    -- ^ Remove child nodes from graph.
    | SaveSession FilePath
    -- ^ Writes all commands executed in this session to a file on disk.
    | Exit
    -- ^ Exit the repl.
    deriving (Eq, Show)

-- | Please remember to update this text whenever you update the ADT above.
helpText :: String
helpText =
    "Available commands in the Kore REPL: \n\
    \help                    shows this help message\n\
    \claim <n>               shows the nth claim\n\
    \axiom <n>               shows the nth axiom\n\
    \prove <n>               initializes proof mode for the nth \
                             \claim\n\
    \graph                   shows the current proof graph (*)\n\
    \step [n]                attempts to run 'n' proof steps at\
                             \the current node (n=1 by default)\n\
    \stepf [n]               attempts to run 'n' proof steps at\
                             \ the current node, stepping through\
                             \ branchings (n=1 by default)\n\
    \select <n>              select node id 'n' from the graph\n\
    \config [n]              shows the config for node 'n'\
                             \ (defaults to current node)\n\
    \omit [cell]             adds or removes cell to omit list\
                             \ (defaults to showing the omit list)\n\
    \leafs                   shows unevaluated or stuck leafs\n\
    \rule [n]                shows the rule for node 'n'\
                             \ (defaults to current node)\n\
    \prec-branch [n]         shows first preceding branch\
                             \ (defaults to current node)\n\
    \children [n]            shows direct children of node\
                             \ (defaults to current node)\n\
    \label                   shows all node labels\n\
    \label <l>               jump to a label\n\
    \label <+l> [n]          add a new label for a node\
                             \ (defaults to current node)\n\
    \label <-l>              remove a label\n\
    \try <a|c><num>          attempts <a>xiom or <c>laim at index <num>.\n\
    \clear [n]               removes all node children from the proof graph\n\
                             \ (defaults to current node)\n\
    \exit                    exits the repl\
    \\n\
    \Available modifiers:\n\
    \<command> > file        prints the output of 'command' to file\n\
    \\n\
    \(*) If an edge is labeled as Simpl/RD it means that\
    \ either the target node was reached using the SMT solver\
    \ or it was reached through the Remove Destination step."

-- | Determines whether the command needs to be stored or not. Commands that
-- affect the outcome of the proof are stored.
shouldStore :: ReplCommand -> Bool
shouldStore =
    \case
        ShowUsage        -> False
        Help             -> False
        ShowClaim _      -> False
        ShowAxiom _      -> False
        ShowGraph        -> False
        ShowConfig _     -> False
        ShowLeafs        -> False
        ShowRule _       -> False
        ShowPrecBranch _ -> False
        ShowChildren _   -> False
        SaveSession _    -> False
        Exit             -> False
        _                -> True

-- Type synonym for the actual type of the execution graph.
type ExecutionGraph =
    Strategy.ExecutionGraph
        (CommonStrategyPattern Object)
        (RewriteRule Object Variable)

type InnerGraph =
    Gr (CommonStrategyPattern Object) (Seq (RewriteRule Object Variable))

-- | State for the rep.
data ReplState claim level = ReplState
    { axioms   :: [Axiom level]
    -- ^ List of available axioms
    , claims   :: [claim]
    -- ^ List of claims to be proven
    , claim    :: claim
    -- ^ Currently focused claim in the repl
    , graph    :: ExecutionGraph
    -- ^ Execution graph for the current proof; initialized with root = claim
    , node     :: Graph.Node
    -- ^ Currently selected node in the graph; initialized with node = root
    , commands :: Seq String
    -- ^ All commands evaluated by the current repl session
    , omit     :: [String]
    -- ^ The omit list, initially empty
    , stepper
        :: Claim claim
        => claim
        -> [claim]
        -> [Axiom level]
        -> ExecutionGraph
        -> Graph.Node
        -> Simplifier ExecutionGraph
    -- ^ Stepper function, it is a partially applied 'verifyClaimStep'
    , unifier
        :: StepPattern level Variable
        -> StepPattern level Variable
        -> UnifierWithExplanation Variable ()
    -- ^ Unifier function, it is a partially applied 'unificationProcedure'
    --   where we discard the result since we are looking for unification
    --   failures
    , labels  :: Map String Graph.Node
    -- ^ Map from labels to nodes
    }

-- | Unifier that stores the first 'explainBottom'.
-- See 'runUnifierWithExplanation'.
newtype UnifierWithExplanation variable a = UnifierWithExplanation
    { getUnifier :: AccumT (First (Doc ())) (Unifier variable) a
    } deriving (Applicative, Functor, Monad)

instance MonadUnify UnifierWithExplanation where
    throwSubstitutionError =
        UnifierWithExplanation
            . Monad.Trans.lift
            . Monad.Unify.throwSubstitutionError

    throwUnificationError =
        UnifierWithExplanation
            . Monad.Trans.lift
            . Monad.Unify.throwUnificationError

    liftSimplifier =
        UnifierWithExplanation
            . Monad.Trans.lift
            . Monad.Unify.liftSimplifier

    mapVariable f (UnifierWithExplanation u) =
        UnifierWithExplanation
            $ Monad.Accum.mapAccumT (Monad.Unify.mapVariable f) u

    explainBottom info first second =
        UnifierWithExplanation . Monad.Accum.add . First . Just $ Pretty.vsep
            [ info
            , "When unifying:"
            , Pretty.indent 4 $ unparse first
            , "With:"
            , Pretty.indent 4 $ unparse second
            ]

runUnifierWithExplanation
    :: UnifierWithExplanation variable a
    -> Simplifier (Maybe (Doc ()))
runUnifierWithExplanation (UnifierWithExplanation accum)
    = fmap join
        . (fmap . fmap) getFirst
        . (fmap . fmap) snd
        . fmap hush
        . Monad.Unify.runUnifier
        $ Monad.Accum.runAccumT accum mempty

Lens.makeLenses ''ReplState
