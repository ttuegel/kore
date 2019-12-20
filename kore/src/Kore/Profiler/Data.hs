{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}
module Kore.Profiler.Data
    ( MonadProfiler (..)
    , profileEvent
    , profileValue
    , ProfileEvent (..)
    , Configuration (..)
    , Destination (..)
    ) where

import Control.Monad
    ( when
    )
import Control.Monad.Codensity
    ( Codensity
    )
import Control.Monad.IO.Class
    ( MonadIO (liftIO)
    )
import Control.Monad.Reader
    ( ReaderT
    )
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Accum
    ( AccumT
    )
import Control.Monad.Trans.Class
    ( MonadTrans (..)
    )
import Control.Monad.Trans.Except
    ( ExceptT
    )
import Control.Monad.Trans.Identity
    ( IdentityT
    )
import Control.Monad.Trans.Maybe
    ( MaybeT
    )
import Data.Functor.Identity
    ( Identity
    )
import Data.List
    ( intercalate
    )
import qualified Data.List as List
import Debug.Trace
    ( traceM
    )
import Debug.Trace.String
    ( traceEventIO
    )
import System.Clock
    ( TimeSpec
    )

import ListT
    ( ListT
    )

{- Monad that can also handle profiling events.
-}
class Monad profiler => MonadProfiler profiler where
    profileStart :: [String] -> profiler (profiler ())
    default profileStart
        :: (MonadProfiler m, MonadTrans t, profiler ~ t m)
        => [String] -> profiler (profiler ())
    profileStart event = do
        profileStop <- lift (profileStart event)
        return (lift profileStop)

    profile
        :: [String]
        -> profiler a
        -> profiler a
    {-# INLINE profile #-}
    profile strings profiler = do
        profileStop <- profileStart strings
        a <- profiler
        profileStop
        return a

    -- TODO(virgil): Add a command line flag for this.
    profileConfiguration :: profiler Configuration
    profileConfiguration =
        return Configuration
            { identifierFilter = Nothing
            , dumpIdentifier = Nothing
            , destination = GhcEventsAnalyze
            , logBranching = False
            , logStrategy = True
            , logSimplification = False
            , logInitialization = False
            , logEvaluation = True
            , logSmt = True
            }
    {-# INLINE profileConfiguration #-}

profileValue :: MonadProfiler profiler => [String] -> Int -> profiler ()
profileValue tags value = do
    Configuration {destination} <- profileConfiguration
    when (isHumanReadable destination)
        (traceM (intercalate "-" tags ++ " --> " ++ show value))

-- Instance for tests.
instance MonadProfiler Identity where
    {-# INLINE profileStart #-}
    profileStart = \_ -> return (return ())

    {-# INLINE profile #-}
    profile = \_ x -> x

    profileConfiguration =
        return Configuration
            { identifierFilter = Nothing
            , dumpIdentifier = Nothing
            , destination = GhcEventsAnalyze
            , logBranching = False
            , logStrategy = False
            , logSimplification = False
            , logInitialization = False
            , logEvaluation = False
            , logSmt = False
            }
    {-# INLINE profileConfiguration #-}

data Destination =
    GhcEventsAnalyze
    -- ^ Suggestions for the human readable output:
    --
    -- * Pipe through @sed 's/-/ /g' | sed "s/'//g"@ to remove characters
    --   that will confuse the next steps.
    -- * Pipe through @tr '\n' '~'@ to remove newlines.
    -- * Pipe through something like
    --   @sed '^\s*timing.*{~\s*} timing.*e [2-9]s~'@
    --   to remove timings that are too low (i.e. with negative exponents)
    --   and which do not contain other timings inside (sed command not
    --   copy-pasted from actual command-line)
    -- * Pipe repeatedly through the sed command above to remove all low timings
    -- * Put newlines back with @tr '~' '\n'@
    -- * Pipe through astyle to indent
    -- * Use an editor which collapses sections to explore (Visual Studio Code
    --   defines sections based on indentation levels by default and seems
    --   to be fast enough for exploring profiling output files).

data Configuration =
    Configuration
        { identifierFilter :: !(Maybe String)
        -- ^ If present, only emits events for this identifier.
        , dumpIdentifier :: !(Maybe String)
        -- ^ If present, dump extra information for this identifier.
        , destination :: Destination
        , logBranching :: !Bool
        , logStrategy :: !Bool
        , logSimplification :: !Bool
        , logInitialization :: !Bool
        , logEvaluation :: !Bool
        , logSmt :: !Bool
        }

{- A profiler event.

The profiler generates two @ProfileEvent@s for each actual event:
one at the start, with @duration=Nothing@, and one at the end with an @Just@
value for @end@
-}
data ProfileEvent
    = ProfileEvent
        { start :: !TimeSpec
        -- ^ The start CPU time.
        , end :: !(Maybe TimeSpec)
        -- ^ The cpu time at the end of the event.
        -- Nothing if this is a start event.
        , tags :: ![String]
        -- ^ Tags for the event. If @tags=[t1, t2, t3]@ then this event will be
        -- counted as part of @t1@, @t1-t2@ and @t1-t2-t3@.
        }
    deriving (Show, Read)

isHumanReadable :: Destination -> Bool
isHumanReadable _ = False

profileEvent
    :: MonadIO profiler => Configuration -> [String] -> profiler (profiler ())
profileEvent Configuration {destination} =
    case destination of
        GhcEventsAnalyze -> profileGhcEventsAnalyze

{- Times an action in the format required by @ghc-events-analyze@.
-}
profileGhcEventsAnalyze
    :: MonadIO profiler => [String] -> profiler (profiler ())
profileGhcEventsAnalyze event = do
    liftIO $ traceEventIO ("START " ++ List.intercalate "/" event)
    return $ liftIO $ traceEventIO ("STOP " ++ List.intercalate "/" event)

instance MonadProfiler m => MonadProfiler (Codensity m)

instance MonadProfiler m => MonadProfiler (ReaderT thing m )

instance MonadProfiler m => MonadProfiler (Strict.StateT s m)

instance MonadProfiler m => MonadProfiler (MaybeT m)

instance MonadProfiler m => MonadProfiler (IdentityT m)

instance MonadProfiler IO where
    {-# INLINE profileStart #-}
    profileStart event = do
        configuration <- profileConfiguration
        profileEvent configuration event

instance MonadProfiler m => MonadProfiler (ExceptT e m)

instance MonadProfiler m => MonadProfiler (ListT m)

instance (MonadProfiler m, Monoid w) => MonadProfiler (AccumT w m)
