{-|
Module      : Kore.Repl
Description : Proof REPL
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
-}

module Kore.Repl
    ( runRepl
    ) where

import Prelude.Kore

import Control.Concurrent.MVar
import Control.Exception
    ( AsyncException (UserInterrupt)
    )
import qualified Control.Lens as Lens
import Control.Monad
    ( forever
    , void
    )
import Control.Monad.Catch
    ( MonadCatch
    )
import qualified Control.Monad.Catch as Exception
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Control.Monad.RWS.Strict
    ( RWST
    , execRWST
    )
import Control.Monad.State.Strict
    ( MonadState
    , StateT
    , evalStateT
    )
import qualified Data.Default as Default
import Data.Generics.Product
import Data.Generics.Wrapped
import qualified Data.Graph.Inductive.Graph as Graph
import Data.List
    ( findIndex
    )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Kore.Attribute.RuleIndex
import System.IO
    ( hFlush
    , stdout
    )
import Text.Megaparsec
    ( parseMaybe
    )

import Kore.Internal.TermLike
    ( TermLike
    , mkSortVariable
    , mkTop
    )
import qualified Kore.Log as Log
import Kore.Repl.Data
import Kore.Repl.Interpreter
import Kore.Repl.Parser
import Kore.Repl.State
import Kore.Step.RulePattern
    ( ReachabilityRule (..)
    )
import Kore.Step.Simplification.Data
    ( MonadSimplify
    )
import qualified Kore.Step.Strategy as Strategy
import Kore.Strategies.Goal
import Kore.Strategies.Verification
import Kore.Syntax.Module
    ( ModuleName (..)
    )
import Kore.Syntax.Variable

import Kore.Unification.Procedure
    ( unificationProcedureWorker
    )
import Kore.Unparser
    ( unparseToString
    )

-- | Runs the repl for proof mode. It requires all the tooling and simplifiers
-- that would otherwise be required in the proof and allows for step-by-step
-- execution of proofs. Currently works via stdin/stdout interaction.
runRepl
    :: forall m
    .  MonadSimplify m
    => MonadIO m
    => MonadCatch m
    => [Axiom]
    -- ^ list of axioms to used in the proof
    -> [ReachabilityRule]
    -- ^ list of claims to be proven
    -> MVar (Log.LogAction IO Log.ActualEntry)
    -> ReplScript
    -- ^ optional script
    -> ReplMode
    -- ^ mode to run in
    -> ScriptModeOutput
    -- ^ optional flag for output in run mode
    -> OutputFile
    -- ^ optional output file
    -> ModuleName
    -> m ()
runRepl _ [] _ _ _ _ outputFile _ =
    let printTerm = maybe putStrLn writeFile (unOutputFile outputFile)
    in liftIO . printTerm . unparseToString $ topTerm
  where
    topTerm :: TermLike VariableName
    topTerm = mkTop $ mkSortVariable "R"

runRepl
    axioms'
    claims'
    logger
    replScript
    replMode
    scriptModeOutput
    outputFile
    mainModuleName
    = do
    (newState, _) <-
            (\rwst -> execRWST rwst config state)
            $ evaluateScript replScript scriptModeOutput
    case replMode of
        Interactive -> do
            replGreeting
            flip evalStateT newState
                $ flip runReaderT config
                $ forever repl0
        RunScript ->
            runReplCommand Exit newState

  where

    runReplCommand :: ReplCommand -> ReplState -> m ()
    runReplCommand cmd st =
        void
            $ flip evalStateT st
            $ flip runReaderT config
            $ replInterpreter printIfNotEmpty cmd

    evaluateScript
        :: ReplScript
        -> ScriptModeOutput
        -> RWST (Config m) String ReplState m ()
    evaluateScript script outputFlag =
        maybe (pure ()) (flip parseEvalScript outputFlag) (unReplScript script)

    repl0 :: ReaderT (Config m) (StateT ReplState m) ()
    repl0 = do
        str <- prompt
        let command = fromMaybe ShowUsage $ parseMaybe commandParser str
        when (shouldStore command) $ field @"commands" Lens.%= (Seq.|> str)
        void $ replInterpreter printIfNotEmpty command

    state :: ReplState
    state =
        ReplState
            { axioms         = addIndexesToAxioms axioms'
            , claims         = addIndexesToClaims (length axioms') claims'
            , claim          = firstClaim
            , claimIndex     = firstClaimIndex
            , graphs         = Map.singleton firstClaimIndex firstClaimExecutionGraph
            , node           = ReplNode (Strategy.root firstClaimExecutionGraph)
            , commands       = Seq.empty
            -- TODO(Vladimir): should initialize this to the value obtained from
            -- the frontend via '--omit-labels'.
            , omit           = mempty
            , labels         = Map.empty
            , aliases        = Map.empty
            , koreLogOptions =
                (Default.def @Log.KoreLogOptions)
                    { Log.exeName = Log.ExeName "kore-repl" }
            }

    config :: Config m
    config =
        Config
            { stepper    = stepper0
            , unifier    = unificationProcedureWorker
            , logger
            , outputFile
            , mainModuleName
            }

    firstClaimIndex :: ClaimIndex
    firstClaimIndex =
        ClaimIndex
        . fromMaybe (error "No claims found")
        $ findIndex (not . isTrusted) claims'

    addIndexesToAxioms
        :: [Axiom]
        -> [Axiom]
    addIndexesToAxioms axs =
        fmap addIndex (zip axs [0..])

    addIndexesToClaims
        :: Int
        -> [ReachabilityRule]
        -> [ReachabilityRule]
    addIndexesToClaims len claims'' =
        zipWith addIndexToClaim [len..] claims''
      where
        addIndexToClaim n =
            Lens.over (lensAttribute . field @"identifier") (makeRuleIndex n)

        lensAttribute =
            Lens.lens
                (\case
                    OnePath onePathRule ->
                        Lens.view (_Unwrapped . field @"attributes") onePathRule
                    AllPath allPathRule ->
                        Lens.view (_Unwrapped . field @"attributes") allPathRule
                )
                (\case
                    OnePath onePathRule -> \attrs ->
                        onePathRule
                        & Lens.set (_Unwrapped . field @"attributes") attrs
                        & OnePath
                    AllPath allPathRule -> \attrs ->
                        allPathRule
                        & Lens.set (_Unwrapped . field @"attributes") attrs
                        & AllPath
                )

    addIndex
        :: (Axiom, Int)
        -> Axiom
    addIndex (rw, n) =
        Lens.over (lensAttribute . field @"identifier") (makeRuleIndex n) rw
      where
        lensAttribute = _Unwrapped . _Unwrapped . field @"attributes"

    makeRuleIndex :: Int -> RuleIndex -> RuleIndex
    makeRuleIndex n _ = RuleIndex (Just n)

    firstClaim :: ReachabilityRule
    firstClaim = claims' !! unClaimIndex firstClaimIndex

    firstClaimExecutionGraph :: ExecutionGraph Axiom
    firstClaimExecutionGraph = emptyExecutionGraph firstClaim

    stepper0
        :: ReachabilityRule
        -> [ReachabilityRule]
        -> [Axiom]
        -> ExecutionGraph Axiom
        -> ReplNode
        -> m (ExecutionGraph Axiom)
    stepper0 claim claims axioms graph rnode = do
        let node = unReplNode rnode
        if Graph.outdeg (Strategy.graph graph) node == 0
            then
                catchEverything graph
                $ catchInterruptWithDefault graph
                $ verifyClaimStep claim claims axioms graph node
            else pure graph

    catchInterruptWithDefault :: a -> m a -> m a
    catchInterruptWithDefault a =
        Exception.handle $ \case
            UserInterrupt -> do
                liftIO $ putStrLn "Step evaluation interrupted."
                pure a
            e -> Exception.throwM e

    catchEverything :: a -> m a -> m a
    catchEverything a =
        Exception.handleAll $ \e -> liftIO $ do
            putStrLn "Stepper evaluation errored."
            print e
            pure a

    replGreeting :: m ()
    replGreeting =
        liftIO $
            putStrLn "Welcome to the Kore Repl! Use 'help' to get started.\n"

    prompt :: MonadIO n => MonadState ReplState n => n String
    prompt = do
        node <- Lens.use (field @"node")
        liftIO $ do
            putStr $ "Kore (" <> show (unReplNode node) <> ")> "
            hFlush stdout
            getLine
