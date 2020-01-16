{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}

module Log
    (
    -- * Entries
      Entry (..)
    , Severity (..)
    , SomeEntry (..)
    , someEntry
    , entryTypeText
    -- * Interface
    , MonadLog (..)
    , WithLog
    -- * Implementation
    , LoggerT (..)
    -- * Log actions
    , LogAction (..)
    , liftLogAction
    , hoistLogAction
    -- * Messages (Deprecated)
    , LogMessage (..)
    , log
    , logDebug
    , logInfo
    , logWarning
    , logError
    , logCritical
    , logWith
    ) where

import Colog
    ( LogAction (..)
    , (<&)
    )
import qualified Control.Lens as Lens
import Control.Lens.Prism
    ( Prism
    )
import Control.Monad.Catch
    ( MonadCatch
    , MonadThrow
    )
import Control.Monad.Except
    ( ExceptT
    )
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class
import Control.Monad.Morph
    ( MFunctor
    )
import qualified Control.Monad.Morph as Morph
import Control.Monad.Trans
    ( MonadTrans
    )
import qualified Control.Monad.Trans as Monad.Trans
import Control.Monad.Trans.Accum
    ( AccumT
    )
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Identity
    ( IdentityT
    )
import Control.Monad.Trans.Maybe
    ( MaybeT
    )
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as Strict
    ( StateT
    )
import Data.Functor.Contravariant
    ( contramap
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
    ( Pretty
    )
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Typeable
    ( Typeable
    )
import qualified Data.Typeable
    ( cast
    )
import qualified GHC.Stack as GHC
import Prelude hiding
    ( log
    )
import qualified Type.Reflection as Reflection

import Control.Monad.Counter
    ( CounterT
    )

-- | Log level used to describe each log message. It is also used to set the
-- minimum level to be outputted.
data Severity
    = Debug
    -- ^ Lowest level used for low-level debugging.
    | Info
    -- ^ Used for various informative messages.
    | Warning
    -- ^ Used for odd/unusual cases which are recoverable.
    | Error
    -- ^ Used for application errors, unexpected behaviors, etc.
    | Critical
    -- ^ Used before shutting down the application.
    deriving (Show, Read, Eq, Ord)

instance Pretty.Pretty Severity where
    pretty = Pretty.pretty . show

-- | This type should not be used directly, but rather should be created and
-- dispatched through the `log` functions.
data LogMessage = LogMessage
    { message   :: Text
    -- ^ message being logged
    , severity  :: !Severity
    -- ^ log level / severity of message
    , callstack :: !GHC.CallStack
    -- ^ call stack of the message, when available
    }

type WithLog msg = MonadLog

-- | 'Monad.Trans.lift' any 'LogAction' into a monad transformer.
liftLogAction
    :: (Monad m, MonadTrans t)
    => LogAction m msg
    -> LogAction (t m) msg
liftLogAction logAction =
    Colog.LogAction (Monad.Trans.lift . Colog.unLogAction logAction)

-- | Use a natural transform on a 'LogAction'.
hoistLogAction
    :: (forall a. m a -> n a)
    -> LogAction m msg
    -> LogAction n msg
hoistLogAction f (LogAction logger) = LogAction $ \msg -> f (logger msg)

-- | Log any message.
logMsg :: (Entry msg, WithLog msg m) => msg -> m ()
logMsg = logM

-- | Logs a message using given 'Severity'.
log
    :: forall m
    . (GHC.HasCallStack, WithLog LogMessage m)
    => Severity
    -- ^ If lower than the minimum severity, the message will not be logged
    -> Text
    -- ^ Message to be logged
    -> m ()
log s t = logMsg $ LogMessage t s GHC.callStack

-- | Logs using 'Debug' log level. See 'log'.
logDebug
    :: forall m
    . (WithLog LogMessage m)
    => Text
    -> m ()
logDebug = log Debug

-- | Logs using 'Info' log level. See 'log'.
logInfo
    :: forall m
    . (WithLog LogMessage m)
    => Text
    -> m ()
logInfo = log Info

-- | Logs using 'Warning' log level. See 'log'.
logWarning
    :: forall m
    . (WithLog LogMessage m)
    => Text
    -> m ()
logWarning = log Warning

-- | Logs using 'Error' log level. See 'log'.
logError
    :: forall m
    . (WithLog LogMessage m)
    => Text
    -> m ()
logError = log Error

-- | Logs using 'Critical' log level. See 'log'.
logCritical
    :: forall m
    . (WithLog LogMessage m)
    => Text
    -> m ()
logCritical = log Critical

-- ---------------------------------------------------------------------
-- * LoggerT

class (Pretty entry, Typeable entry) => Entry entry where
    toEntry :: entry -> SomeEntry
    toEntry = SomeEntry

    fromEntry :: SomeEntry -> Maybe entry
    fromEntry (SomeEntry entry) = Data.Typeable.cast entry

    entrySeverity :: entry -> Severity

instance Entry LogMessage where
    entrySeverity LogMessage { severity } = severity

instance Pretty LogMessage where
    pretty LogMessage { message, callstack } =
        Pretty.hsep
            [ Pretty.pretty message
            , Pretty.brackets (formatCallstack callstack)
            ]
      where
        formatCallstack :: GHC.CallStack -> Pretty.Doc ann
        formatCallstack cs
          | length (GHC.getCallStack cs) <= 1 = mempty
          | otherwise                         = callStackToBuilder cs
        callStackToBuilder :: GHC.CallStack -> Pretty.Doc ann
        callStackToBuilder =
            Pretty.pretty
            . GHC.prettyCallStack
            . GHC.popCallStack

someEntry :: (Entry e1, Entry e2) => Prism SomeEntry SomeEntry e1 e2
someEntry = Lens.prism' toEntry fromEntry

data SomeEntry where
    SomeEntry :: Entry entry => entry -> SomeEntry

instance Entry SomeEntry where
    entrySeverity (SomeEntry entry) = entrySeverity entry

instance Pretty SomeEntry where
    pretty (SomeEntry entry) = Pretty.pretty entry

class Monad m => MonadLog m where
    logM :: Entry entry => entry -> m ()
    default logM
        :: Entry entry
        => (MonadTrans trans, MonadLog log, m ~ trans log)
        => entry
        -> m ()
    logM = Monad.Trans.lift . logM
    {-# INLINE logM #-}

    logScope :: (SomeEntry -> SomeEntry) -> m a -> m a
    default logScope
        :: (MFunctor trans, MonadLog log, m ~ trans log)
        => (SomeEntry -> SomeEntry)
        -> m a
        -> m a
    logScope locally = Morph.hoist (logScope locally)
    {-# INLINE logScope #-}

instance (Monoid acc, MonadLog log) => MonadLog (AccumT acc log) where
    logScope locally = Accum.mapAccumT (logScope locally)
    {-# INLINE logScope #-}

instance MonadLog log => MonadLog (CounterT log)

instance MonadLog log => MonadLog (ExceptT error log)

instance MonadLog log => MonadLog (IdentityT log)

instance MonadLog log => MonadLog (MaybeT log)

instance MonadLog log => MonadLog (Strict.StateT state log)

newtype LoggerT m a =
    LoggerT { getLoggerT :: ReaderT (LogAction m SomeEntry) m a }
    deriving (Functor, Applicative, Monad)
    deriving (MonadIO, MonadThrow, MonadCatch)

instance Monad m => MonadLog (LoggerT m) where
    logM entry =
        LoggerT $ ask >>= Monad.Trans.lift . (<& toEntry entry)
    logScope f = LoggerT . local (contramap f) . getLoggerT

instance MonadTrans LoggerT where
    lift = LoggerT . Monad.Trans.lift
    {-# INLINE lift #-}

logWith
    :: Entry entry
    => LogAction m SomeEntry
    -> entry
    -> m ()
logWith logger entry =
    logger Colog.<& toEntry entry

entryTypeText :: SomeEntry -> Text
entryTypeText (SomeEntry entry) =
    Text.pack . show . Reflection.typeOf $ entry
