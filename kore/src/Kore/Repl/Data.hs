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
    , ReplNode (..)
    , ReplState (..)
    , NodeState (..)
    , GraphProofStatus (..)
    , AliasDefinition (..), ReplAlias (..), AliasArgument(..), AliasError (..)
    , getNodeState
    , InnerGraph
    , lensAxioms, lensClaims, lensClaim
    , lensGraphs, lensNode, lensStepper
    , lensLabels, lensOmit, lensUnifier
    , lensCommands, lensAliases, lensClaimIndex
    , lensLogging
    , shouldStore
    , UnifierWithExplanation (..)
    , runUnifierWithExplanation
    , emptyExecutionGraph
    , getClaimByIndex, getAxiomByIndex, getAxiomOrClaimByIndex
    , switchToProof
    , getTargetNode, getInnerGraph, getExecutionGraph
    , getConfigAt, getRuleFor, getLabels, setLabels
    , StepResult(..)
    , runStepper, runStepper'
    , runUnifier
    , updateInnerGraph, updateExecutionGraph
    , addOrUpdateAlias, findAlias, substituteAlias
    , LogType (..)
    ) where

import           Control.Applicative
                 ( Alternative )
import           Control.Concurrent.MVar
import qualified Control.Lens as Lens hiding
                 ( makeLenses )
import qualified Control.Lens.TH.Rules as Lens
import           Control.Monad
                 ( join )
import           Control.Monad.Error.Class
                 ( MonadError )
import qualified Control.Monad.Error.Class as Monad.Error
import           Control.Monad.IO.Class
                 ( liftIO )
import           Control.Monad.State.Strict
                 ( MonadState, get, modify )
import           Control.Monad.Trans.Accum
                 ( AccumT, runAccumT )
import qualified Control.Monad.Trans.Accum as Monad.Accum
import qualified Control.Monad.Trans.Class as Monad.Trans
import           Data.Bitraversable
                 ( bisequence, bitraverse )
import           Data.Coerce
                 ( coerce )
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree
                 ( Gr )
import           Data.List.NonEmpty
                 ( NonEmpty (..) )
import qualified Data.Map as Map
import           Data.Map.Strict
                 ( Map )
import           Data.Maybe
                 ( fromMaybe, listToMaybe )
import           Data.Monoid
                 ( First (..) )
import           Data.Sequence
                 ( Seq )
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import           Data.Text.Prettyprint.Doc
                 ( Doc )
import qualified Data.Text.Prettyprint.Doc as Pretty
import           GHC.Exts
                 ( toList )
import           Numeric.Natural
import           System.IO
                 ( Handle, IOMode (AppendMode), hClose, openFile )

import           Kore.Internal.Conditional
                 ( Conditional (..) )
import           Kore.Internal.Predicate
                 ( Predicate )
import           Kore.Internal.TermLike
                 ( TermLike )
import           Kore.Logger
                 ( LogMessage, WithLog )
import qualified Kore.Logger.Output as Logger
import           Kore.OnePath.StrategyPattern
import           Kore.OnePath.Verification
                 ( Axiom (..), Claim )
import           Kore.Step.Rule
                 ( RewriteRule (..), RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( MonadSimplify, Simplifier )
import qualified Kore.Step.Strategy as Strategy
import           Kore.Syntax.Variable
                 ( Variable )
import           Kore.Unification.Unify
                 ( MonadUnify, UnifierTT (UnifierTT) )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
                 ( unparse )
import           SMT
                 ( MonadSMT )

newtype AxiomIndex = AxiomIndex
    { unAxiomIndex :: Int
    } deriving (Eq, Show)

newtype ClaimIndex = ClaimIndex
    { unClaimIndex :: Int
    } deriving (Eq, Ord, Show)

newtype ReplNode = ReplNode
    { unReplNode :: Graph.Node
    } deriving (Eq, Show)

data AliasDefinition = AliasDefinition
    { name      :: String
    , arguments :: [String]
    , command   :: String
    } deriving (Eq, Show)

data AliasArgument
  = SimpleArgument String
  | QuotedArgument String
  deriving (Eq, Show)

data ReplAlias = ReplAlias
    { name      :: String
    , arguments :: [AliasArgument]
    } deriving (Eq, Show)

data LogType
    = NoLogging
    | LogToStdOut
    | LogToFile !FilePath
    deriving (Eq, Show)

-- | List of available commands for the Repl. Note that we are always in a proof
-- state. We pick the first available Claim when we initialize the state.
data ReplCommand
    = ShowUsage
    -- ^ This is the default action in case parsing all others fail.
    | Help
    -- ^ Shows the help message.
    | ShowClaim !(Maybe ClaimIndex)
    -- ^ Show the nth claim or the current claim.
    | ShowAxiom !AxiomIndex
    -- ^ Show the nth axiom.
    | Prove !ClaimIndex
    -- ^ Drop the current proof state and re-initialize for the nth claim.
    | ShowGraph !(Maybe FilePath)
    -- ^ Show the current execution graph.
    | ProveSteps !Natural
    -- ^ Do n proof steps from current node.
    | ProveStepsF !Natural
    -- ^ Do n proof steps (through branchings) from the current node.
    | SelectNode !ReplNode
    -- ^ Select a different node in the graph.
    | ShowConfig !(Maybe ReplNode)
    -- ^ Show the configuration from the current node.
    | OmitCell !(Maybe String)
    -- ^ Adds or removes cell to omit list, or shows current omit list.
    | ShowLeafs
    -- ^ Show leafs which can continue evaluation and leafs which are stuck
    | ShowRule !(Maybe ReplNode)
    -- ^ Show the rule(s) that got us to this configuration.
    | ShowPrecBranch !(Maybe ReplNode)
    -- ^ Show the first preceding branch.
    | ShowChildren !(Maybe ReplNode)
    -- ^ Show direct children of node.
    | Label !(Maybe String)
    -- ^ Show all node labels or jump to a label.
    | LabelAdd !String !(Maybe ReplNode)
    -- ^ Add a label to a node.
    | LabelDel !String
    -- ^ Remove a label.
    | Redirect ReplCommand FilePath
    -- ^ Prints the output of the inner command to the file.
    | Try !(Either AxiomIndex ClaimIndex)
    -- ^ Attempt to apply axiom or claim to current node.
    | Clear !(Maybe ReplNode)
    -- ^ Remove child nodes from graph.
    | Pipe ReplCommand !String ![String]
    -- ^ Pipes a repl command into an external script.
    | SaveSession FilePath
    -- ^ Writes all commands executed in this session to a file on disk.
    | AppendTo ReplCommand FilePath
    -- ^ Appends the output of a command to a file.
    | Alias AliasDefinition
    -- ^ Alias a command.
    | TryAlias ReplAlias
    -- ^ Try running an alias.
    | LoadScript FilePath
    -- ^ Load script from file
    | ProofStatus
    -- ^ Show proof status of each claim
    | Log Logger.Severity LogType
    -- ^ Setup the Kore logger.
    | Exit
    -- ^ Exit the repl.
    deriving (Eq, Show)

commandSet :: Set String
commandSet = Set.fromList
    [ "help"
    , "claim"
    , "axiom"
    , "prove"
    , "graph"
    , "step"
    , "stepf"
    , "select"
    , "omit"
    , "leafs"
    , "rule"
    , "prec-branch"
    , "children"
    , "label"
    , "try"
    , "clear"
    , "save-session"
    , "alias"
    , "load"
    , "log"
    , "exit"
    ]

-- | Please remember to update this text whenever you update the ADT above.
helpText :: String
helpText =
    "Available commands in the Kore REPL: \n\
    \help                                  shows this help message\n\
    \claim [n]                             shows the nth claim or if\
                                           \ used without args shows the\
                                           \ currently focused claim\n\
    \axiom <n>                             shows the nth axiom\n\
    \prove <n>                             initializes proof mode for the nth\
                                           \ claim\n\
    \graph [file]                          shows the current proof graph (*)\n\
    \                                      (saves image in .jpeg format if file\
                                           \ argument is given; file extension is\
                                           \ added automatically)\n\
    \step [n]                              attempts to run 'n' proof steps at\
                                           \the current node (n=1 by default)\n\
    \stepf [n]                             attempts to run 'n' proof steps at\
                                           \ the current node, stepping through\
                                           \ branchings (n=1 by default)\n\
    \select <n>                            select node id 'n' from the graph\n\
    \config [n]                            shows the config for node 'n'\
                                           \ (defaults to current node)\n\
    \omit [cell]                           adds or removes cell to omit list\
                                           \ (defaults to showing the omit\
                                           \ list)\n\
    \leafs                                 shows unevaluated or stuck leafs\n\
    \rule [n]                              shows the rule for node 'n'\
                                           \ (defaults to current node)\n\
    \prec-branch [n]                       shows first preceding branch\
                                           \ (defaults to current node)\n\
    \children [n]                          shows direct children of node\
                                           \ (defaults to current node)\n\
    \label                                 shows all node labels\n\
    \label <l>                             jump to a label\n\
    \label <+l> [n]                        add a new label for a node\
                                           \ (defaults to current node)\n\
    \label <-l>                            remove a label\n\
    \try <a|c><num>                        attempts <a>xiom or <c>laim at\
                                           \ index <num>.\n\
    \clear [n]                             removes all node children from the\
                                           \ proof graph\n\
    \                                      (defaults to current node)\n\
    \save-session file                     saves the current session to file\n\
    \alias <name> = <command>              adds as an alias for <command>\n\
    \<alias>                               runs an existing alias\n\
    \load file                             loads the file as a repl script\n\
    \proof-status                          shows status for each claim\n\
    \log <severity> <type>                 configures the logging outout\n\
                                           \<severity> can be debug, info, warning,\
                                           \error, or critical\n\
    \                                      <type> can be NoLogging, LogToStdOut,\
                                           \or LogToFile filename\n\
    \exit                                  exits the repl\
    \\n\
    \Available modifiers:\n\
    \<command> > file                      prints the output of 'command'\
                                           \ to file\n\
    \<command> >> file                     appends the output of 'command'\
                                           \ to file\n\
    \<command> | external script           pipes command to external script\
                                           \ and prints the result in the\
                                           \ repl\n\
    \<command> | external script > file    pipes and then redirects the output\
                                           \ of the piped command to a file\n\
    \<command> | external script >> file   pipes and then appends the output\
                                           \ of the piped command to a file\n\
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
        ShowGraph _      -> False
        ShowConfig _     -> False
        ShowLeafs        -> False
        ShowRule _       -> False
        ShowPrecBranch _ -> False
        ShowChildren _   -> False
        SaveSession _    -> False
        ProofStatus      -> False
        Exit             -> False
        _                -> True

-- Type synonym for the actual type of the execution graph.
type ExecutionGraph =
    Strategy.ExecutionGraph
        CommonStrategyPattern
        (RewriteRule Variable)

type InnerGraph =
    Gr CommonStrategyPattern (Seq (RewriteRule Variable))

-- | State for the rep.
data ReplState claim = ReplState
    { axioms     :: [Axiom]
    -- ^ List of available axioms
    , claims     :: [claim]
    -- ^ List of claims to be proven
    , claim      :: claim
    -- ^ Currently focused claim in the repl
    , claimIndex :: ClaimIndex
    -- ^ Index of the currently focused claim in the repl
    , graphs     :: Map ClaimIndex ExecutionGraph
    -- ^ Execution graph for the current proof; initialized with root = claim
    , node       :: ReplNode
    -- ^ Currently selected node in the graph; initialized with node = root
    , commands   :: Seq String
    -- ^ All commands evaluated by the current repl session
    -- TODO(Vladimir): This should be a Set String instead.
    , omit       :: [String]
    -- ^ The omit list, initially empty
    , stepper
        :: Claim claim
        => claim
        -> [claim]
        -> [Axiom]
        -> ExecutionGraph
        -> ReplNode
        -> Simplifier ExecutionGraph
    -- ^ Stepper function, it is a partially applied 'verifyClaimStep'
    , unifier
        :: TermLike Variable
        -> TermLike Variable
        -> UnifierWithExplanation (Predicate Variable)
    -- ^ Unifier function, it is a partially applied 'unificationProcedure'
    --   where we discard the result since we are looking for unification
    --   failures
    , labels  :: Map ClaimIndex (Map String ReplNode)
    -- ^ Map from labels to nodes
    , aliases :: Map String AliasDefinition
    -- ^ Map of command aliases
    , logging :: (Logger.Severity, LogType)
    , logger  :: MVar (Logger.LogAction IO Logger.LogMessage)
    }

-- | Unifier that stores the first 'explainBottom'.
-- See 'runUnifierWithExplanation'.
newtype UnifierWithExplanation a =
    UnifierWithExplanation
        { getUnifierWithExplanation :: UnifierTT (AccumT (First (Doc ()))) a }
  deriving (Alternative, Applicative, Functor, Monad)

deriving instance MonadSMT UnifierWithExplanation

deriving instance MonadSimplify UnifierWithExplanation

deriving instance WithLog LogMessage UnifierWithExplanation

instance MonadUnify UnifierWithExplanation where
    throwSubstitutionError =
        UnifierWithExplanation . Monad.Unify.throwSubstitutionError
    throwUnificationError =
        UnifierWithExplanation . Monad.Unify.throwUnificationError

    liftSimplifier =
        UnifierWithExplanation . Monad.Unify.liftSimplifier
    liftBranchedSimplifier =
        UnifierWithExplanation . Monad.Unify.liftBranchedSimplifier

    gather =
        UnifierWithExplanation . Monad.Unify.gather . getUnifierWithExplanation
    scatter = UnifierWithExplanation . Monad.Unify.scatter

    explainBottom info first second =
        UnifierWithExplanation
        . UnifierTT
        . Monad.Trans.lift
        . Monad.Accum.add
        . First
        . Just $ Pretty.vsep
            [ info
            , "When unifying:"
            , Pretty.indent 4 $ unparse first
            , "With:"
            , Pretty.indent 4 $ unparse second
            ]

runUnifierWithExplanation
    :: forall a
    .  UnifierWithExplanation a
    -> Simplifier (Either (Doc ()) (NonEmpty a))
runUnifierWithExplanation (UnifierWithExplanation unifier) =
    either (Left . Pretty.pretty) failWithExplanation
    <$> Monad.Unify.runUnifierT
            (\accum -> runAccumT accum mempty)
            unifier
  where
    failWithExplanation (results, explanation) =
        case results of
            [] -> Left $ fromMaybe "No explanation given" (getFirst explanation)
            r : rs -> Right (r :| rs)
  -- where
  --   unificationResults :: Simplifier (Either Failure NonEmpty a)
  --   unificationResults =
  --       _
  --       $   Monad.Unify.runUnifierT
  --               (\accum -> runAccumT accum mempty)
  --               unifier
  --   unificationExplanations :: Simplifier (Maybe (First (Doc ())))
  --   unificationExplanations =
  --       fmap (fmap snd) unificationResults

Lens.makeLenses ''ReplState

-- | Creates a fresh execution graph for the given claim.
emptyExecutionGraph :: Claim claim => claim -> ExecutionGraph
emptyExecutionGraph =
    Strategy.emptyExecutionGraph . extractConfig . RewriteRule . coerce
  where
    extractConfig
        :: RewriteRule Variable
        -> CommonStrategyPattern
    extractConfig (RewriteRule RulePattern { left, requires }) =
        RewritePattern $ Conditional left requires mempty

-- | Get nth claim from the claims list.
getClaimByIndex
    :: MonadState (ReplState claim) m
    => Int
    -- ^ index in the claims list
    -> m (Maybe claim)
getClaimByIndex index = Lens.preuse $ lensClaims . Lens.element index

-- | Get nth axiom from the axioms list.
getAxiomByIndex
    :: MonadState (ReplState claim) m
    => Int
    -- ^ index in the axioms list
    -> m (Maybe Axiom)
getAxiomByIndex index = Lens.preuse $ lensAxioms . Lens.element index

-- | Transforms an axiom or claim index into an axiom or claim if they could be
-- found.
getAxiomOrClaimByIndex
    :: MonadState (ReplState claim) m
    => Either AxiomIndex ClaimIndex
    -> m (Maybe (Either Axiom claim))
getAxiomOrClaimByIndex =
    fmap bisequence
        . bitraverse
            (getAxiomByIndex . coerce)
            (getClaimByIndex . coerce)

-- | Update the currently selected claim to prove.
switchToProof
    :: MonadState (ReplState claim) m
    => Claim claim
    => claim
    -> ClaimIndex
    -> m ()
switchToProof claim cindex =
    modify (\st -> st
        { claim = claim
        , claimIndex = cindex
        , node = ReplNode 0
        })

-- | Get the internal representation of the execution graph.
getInnerGraph
    :: MonadState (ReplState claim) m
    => Claim claim
    => m InnerGraph
getInnerGraph =
    fmap Strategy.graph getExecutionGraph

-- | Get the current execution graph
getExecutionGraph
    :: MonadState (ReplState claim) m
    => Claim claim
    => m ExecutionGraph
getExecutionGraph = do
    ReplState { claimIndex, graphs, claim } <- get
    let mgraph = Map.lookup claimIndex graphs
    return $ maybe (emptyExecutionGraph claim) id mgraph

-- | Update the internal representation of the current execution graph.
updateInnerGraph
    :: MonadState (ReplState claim) m
    => InnerGraph
    -> m ()
updateInnerGraph ig = do
    ReplState { claimIndex, graphs } <- get
    lensGraphs Lens..=
        Map.adjust (updateInnerGraph0 ig) claimIndex graphs
  where
    updateInnerGraph0 :: InnerGraph -> ExecutionGraph -> ExecutionGraph
    updateInnerGraph0 graph Strategy.ExecutionGraph { root } =
        Strategy.ExecutionGraph { root, graph }

-- | Update the current execution graph.
updateExecutionGraph
    :: MonadState (ReplState claim) m
    => ExecutionGraph
    -> m ()
updateExecutionGraph gph = do
    ReplState { claimIndex, graphs } <- get
    lensGraphs Lens..= Map.insert claimIndex gph graphs

-- | Get the node labels for the current claim.
getLabels
    :: MonadState (ReplState claim) m
    => m (Map String ReplNode)
getLabels = do
    ReplState { claimIndex, labels } <- get
    let mlabels = Map.lookup claimIndex labels
    return $ maybe Map.empty id mlabels

-- | Update the node labels for the current claim.
setLabels
    :: MonadState (ReplState claim) m
    => Map String ReplNode
    -> m ()
setLabels lbls = do
    ReplState { claimIndex, labels } <- get
    lensLabels Lens..= Map.insert claimIndex lbls labels


-- | Get selected node (or current node for 'Nothing') and validate that it's
-- part of the execution graph.
getTargetNode
    :: MonadState (ReplState claim) m
    => Claim claim
    => Maybe ReplNode
    -- ^ node index
    -> m (Maybe ReplNode)
getTargetNode maybeNode = do
    currentNode <- Lens.use lensNode
    let node' = unReplNode $ maybe currentNode id maybeNode
    graph <- getInnerGraph
    if node' `elem` Graph.nodes graph
       then pure . Just . ReplNode $ node'
       else pure $ Nothing

-- | Get the configuration at selected node (or current node for 'Nothing').
getConfigAt
    :: MonadState (ReplState claim) m
    => Claim claim
    => Maybe ReplNode
    -> m (Maybe (ReplNode, CommonStrategyPattern))
getConfigAt maybeNode = do
    node' <- getTargetNode maybeNode
    case node' of
        Nothing -> pure $ Nothing
        Just n -> do
            graph' <- getInnerGraph
            pure $ Just (n, getLabel graph' (unReplNode n))
  where
    getLabel gr n = Graph.lab' . Graph.context gr $ n

-- | Get the rule used to reach selected node.
getRuleFor
    :: MonadState (ReplState claim) m
    => Claim claim
    => Maybe ReplNode
    -- ^ node index
    -> m (Maybe (RewriteRule Variable))
getRuleFor maybeNode = do
    targetNode <- getTargetNode maybeNode
    graph' <- getInnerGraph
    pure $ fmap unReplNode targetNode >>= getRewriteRule . Graph.inn graph'
  where
    getRewriteRule
        :: forall a b
        .  [(a, b, Seq (RewriteRule Variable))]
        -> Maybe (RewriteRule Variable)
    getRewriteRule =
        listToMaybe
        . join
        . fmap (toList . third)

    third :: forall a b c. (a, b, c) -> c
    third (_, _, c) = c

-- | Lifting function that takes logging into account.
liftSimplifierWithLogger
    :: forall a t claim
    .  MonadState (ReplState claim) (t Simplifier)
    => Monad.Trans.MonadTrans t
    => MVar (Logger.LogAction IO Logger.LogMessage)
    -> Simplifier a
    -> t Simplifier a
liftSimplifierWithLogger mLogger simplifier = do
   (severity, logType) <- logging <$> get
   (textLogger, maybeHandle) <- logTypeToLogger logType
   let logger = Logger.makeKoreLogger severity textLogger
   _ <- Monad.Trans.lift . liftIO $ swapMVar mLogger logger
   result <- Monad.Trans.lift simplifier
   maybe (pure ()) (Monad.Trans.lift . liftIO . hClose) maybeHandle
   pure result
  where
    logTypeToLogger
        :: LogType
        -> t Simplifier (Logger.LogAction IO Text, Maybe Handle)
    logTypeToLogger =
        \case
            NoLogging   -> pure (mempty, Nothing)
            LogToStdOut -> pure (Logger.logTextStdout, Nothing)
            LogToFile file -> do
                handle <- Monad.Trans.lift . liftIO $ openFile file AppendMode
                pure (Logger.logTextHandle handle, Just handle)

-- | Result after running one or multiple proof steps.
data StepResult
    = NoResult
    -- ^ reached end of proof on current branch
    | SingleResult ReplNode
    -- ^ single follow-up configuration
    | BranchResult [ReplNode]
    -- ^ configuration branched
    deriving (Show)

-- | Run a single step for the data in state
-- (claim, axioms, claims, current node and execution graph).
runStepper
    :: MonadState (ReplState claim) (m Simplifier)
    => Monad.Trans.MonadTrans m
    => Claim claim
    => m Simplifier StepResult
runStepper = do
    ReplState { claims, axioms, node } <- get
    (graph', res) <- runStepper' claims axioms node
    updateExecutionGraph graph'
    case res of
        SingleResult nextNode -> do
            lensNode Lens..= nextNode
            pure res
        _                     -> pure res

-- | Run a single step for the current claim with the selected claims, axioms
-- starting at the selected node.
runStepper'
    :: MonadState (ReplState claim) (m Simplifier)
    => Monad.Trans.MonadTrans m
    => Claim claim
    => [claim]
    -> [Axiom]
    -> ReplNode
    -> m Simplifier (ExecutionGraph, StepResult)
runStepper' claims axioms node = do
    ReplState { claim, stepper } <- get
    mvar <- Lens.use lensLogger
    gph <- getExecutionGraph
    gr@Strategy.ExecutionGraph { graph = innerGraph } <-
        liftSimplifierWithLogger mvar $ stepper claim claims axioms gph node
    pure . (,) gr $ case Graph.suc innerGraph (unReplNode node) of
        []       -> NoResult
        [single] -> case getNodeState innerGraph single of
                        Nothing -> NoResult
                        Just (StuckNode, _) -> NoResult
                        _ -> SingleResult . ReplNode $ single
        nodes    -> BranchResult $ fmap ReplNode nodes

runUnifier
    :: MonadState (ReplState claim) (m Simplifier)
    => Monad.Trans.MonadTrans m
    => TermLike Variable
    -> TermLike Variable
    -> m Simplifier (Either (Doc ()) (NonEmpty (Predicate Variable)))
runUnifier first second = do
    unifier <- Lens.use lensUnifier
    mvar <- Lens.use lensLogger
    liftSimplifierWithLogger mvar
        . runUnifierWithExplanation
        $ unifier first second

getNodeState :: InnerGraph -> Graph.Node -> Maybe (NodeState, Graph.Node)
getNodeState graph node =
        fmap (\nodeState -> (nodeState, node))
        . strategyPattern StrategyPatternTransformer
            { rewriteTransformer = const . Just $ UnevaluatedNode
            , stuckTransformer = const . Just $ StuckNode
            , bottomValue = Nothing
            }
        . Graph.lab'
        . Graph.context graph
        $ node

data NodeState = StuckNode | UnevaluatedNode
    deriving (Eq, Ord, Show)

data AliasError
    = NameAlreadyDefined
    | UnknownCommand

data GraphProofStatus
    = NotStarted
    | Completed
    | InProgress [Graph.Node]
    | StuckProof [Graph.Node]
    | TrustedClaim
    deriving (Eq, Show)

-- | Adds or updates the provided alias.
addOrUpdateAlias
    :: forall m claim
    .  MonadState (ReplState claim) m
    => MonadError AliasError m
    => AliasDefinition
    -> m ()
addOrUpdateAlias alias@AliasDefinition { name, command } = do
    checkNameIsNotUsed
    checkCommandExists
    lensAliases Lens.%= Map.insert name alias
  where
    checkNameIsNotUsed :: m ()
    checkNameIsNotUsed =
        not . Set.member name <$> existingCommands
            >>= falseToError NameAlreadyDefined

    checkCommandExists :: m ()
    checkCommandExists = do
        cmds <- existingCommands
        let
            maybeCommand = listToMaybe $ words command
            maybeExists = Set.member <$> maybeCommand <*> pure cmds
        maybe
            (Monad.Error.throwError UnknownCommand)
            (falseToError UnknownCommand)
            maybeExists

    existingCommands :: m (Set String)
    existingCommands =
        Set.union commandSet
        . Set.fromList
        . Map.keys
        <$> Lens.use lensAliases

    falseToError :: AliasError -> Bool -> m ()
    falseToError e b =
        if b then pure () else Monad.Error.throwError e


findAlias
    :: MonadState (ReplState claim) m
    => String
    -> m (Maybe AliasDefinition)
findAlias name = Map.lookup name <$> Lens.use lensAliases

substituteAlias
    :: AliasDefinition
    -> ReplAlias
    -> String
substituteAlias
    AliasDefinition { arguments, command }
    ReplAlias { arguments = actualArguments } =
    unwords
      . fmap replaceArguments
      . words
      $ command
  where
    values :: Map String AliasArgument
    values
      | length arguments == length actualArguments
        = Map.fromList $ zip arguments actualArguments
      | otherwise = Map.empty

    replaceArguments :: String -> String
    replaceArguments s = maybe s toString $ Map.lookup s values

    toString :: AliasArgument -> String
    toString = \case
        SimpleArgument str -> str
        QuotedArgument str -> "\"" <> str <> "\""
