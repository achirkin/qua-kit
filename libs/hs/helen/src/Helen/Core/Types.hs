-----------------------------------------------------------------------------
--
-- Module      :  Helen.Core.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Helen.Core.Types
  ( -- * Base server-client relationship
    Helen (..), ClientId (..)
  , Client (..)
    -- * Working with services
  , Service (..), ServiceManager (..), ServicePool (..)
  , TargetedMessage (..), SourcedMessage (..)
    -- * Monad support
  , HelenWorld, HelenRoom, HelenMonad (..)
  , runHelenProgram, forkHelen
    -- * Lenses
  , msgChannel, serviceManager
  , incomingMsgs, idleInstances, busyInstances
  , serviceMap, nextCallId
  ) where

import Data.Unique
import Data.Hashable

import Luci.Messages
import Luci.Connect
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TChan as STM
import           Control.Concurrent (forkIO)
import           Control.Lens
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.State.Lazy
import qualified Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Logger
import           Crypto.Random (MonadRandom (..))

-- | Represent a connected client
newtype ClientId = ClientId Unique
  deriving (Eq,Ord, Hashable)

-- | Information about a connected client
data Client = Client
  { queueMessage :: !(Message -> HelenWorld ())
    -- ^ Queue message directly to a client message channel;
    --   normally, modules should use `sendMessage` message from Helen to send messages.
  , clientAddr   :: !String
    -- ^ Socket address of a client
  }

-- | The program state type
data Helen = Helen
  { _msgChannel         :: !(STM.TChan SourcedMessage)
    -- ^ The very core of Helen, all message processing goes through this channel
  , sendMessage         :: !(TargetedMessage -> HelenWorld ())
    -- ^ Send a message to a given client (by client id)
  , registerClient      :: !(Client -> HelenWorld (ClientId, HelenWorld ()))
    -- ^ Register a send-message callback;
    --   Returns own cliendId and an unregister callback
  , subscribeUnregister :: !(ClientId -> (ClientId -> HelenWorld ()) -> HelenWorld ())
    -- ^ Anyone can subscribe for event "client unregistered".
    -- This will be called when a client with a given id cannot receive message anymore
  , _serviceManager      :: !ServiceManager
    -- ^ Keeps track of all services
  }

-- | The very core of Helen, all message processing goes through this channel
msgChannel :: Functor f => (STM.TChan SourcedMessage -> f (STM.TChan SourcedMessage)) -> Helen -> f Helen
msgChannel k h = fmap (\newC -> h { _msgChannel = newC }) (k $ _msgChannel h)

-- | Keeps track of all services
serviceManager :: Functor f => (ServiceManager -> f ServiceManager) -> Helen -> f Helen
serviceManager k h  = fmap (\newM -> h { _serviceManager = newM }) (k $ _serviceManager h)

-- | A handle representing TCP client connected as a service
data Service = RemoteService !ClientId !ServiceName

-- | Service pool manages available instances of services
data ServicePool = ServicePool
  { _incomingMsgs  :: !(Seq.Seq SourcedMessage)
    -- ^ store pending service tasks
  , _idleInstances :: !(Seq.Seq Service)
    -- ^ round-robin sequence of idle service instances
  , _busyInstances :: !(HashMap.HashMap ClientId (CallId, Service))
    -- ^ map of busy instances, so that it is easy to find instance that finished a task
  }


-- | Keep all services in one place
data ServiceManager = ServiceManager
  { _serviceMap   :: !(HashMap.HashMap ServiceName ServicePool)
    -- ^ store services by name
  , _nextCallId   :: !CallId
    -- ^ keep track of last CallId to assign sequential numbers
  , _currentCalls :: !(HashMap.HashMap CallId ServiceName)
  }

-- | Message with receiver
data TargetedMessage = TargetedMessage !ClientId !Message

-- | Message with sender
data SourcedMessage = SourcedMessage !ClientId !Message

----------------------------------------------------------------------------------------------------
-- * Monad for Helen
----------------------------------------------------------------------------------------------------



-- | Define helen monad as a `MonadState Helen` plus additional transformation from pure monad `HelenRoom`
class MonadState Helen m => HelenMonad m where
  liftHelen :: HelenRoom a -> m a

-- | Variant of Helen state monad without `IO` to do pure evaluation.
newtype HelenRoom t = HelenRoom { unRoom :: StateT Helen Identity t }
  deriving (Functor, Applicative, Monad, MonadBase Identity)

instance MonadState Helen HelenRoom where
  get = HelenRoom get
  put = HelenRoom . put
  state = HelenRoom . state

instance HelenMonad HelenRoom where
  liftHelen = id

-- | IO-full variant of Helen state with multithread-preserved state and logging.
newtype HelenWorld t = HelenWorld { unWorld :: ReaderT (STM.TVar Helen) (LoggingT IO) t}
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO HelenWorld where
  type StM HelenWorld a = a
  liftBaseWith f = HelenWorld $ liftBaseWith $ \q -> f (q . unWorld)
  restoreM = HelenWorld . restoreM

instance MonadRandom HelenWorld where
  getRandomBytes = liftIO . getRandomBytes

instance MonadLogger HelenWorld where
  monadLoggerLog a b c d = HelenWorld . lift $ monadLoggerLog a b c d

instance MonadState Helen HelenWorld where
  get = HelenWorld $ ask >>= liftIO . STM.readTVarIO
  put h = HelenWorld $ ask >>= liftIO . STM.atomically . flip STM.writeTVar h
  state f = HelenWorld $ ask >>= \v -> liftIO . STM.atomically $ do
      (r,h) <- f <$> STM.readTVar v
      STM.writeTVar v h
      return r

instance HelenMonad HelenWorld where
  liftHelen hr = state (runIdentity . runStateT (unRoom hr))


-- | Run a program in IO monad.
runHelenProgram :: Helen -> LogLevel -> HelenWorld r -> IO (r,Helen)
runHelenProgram s ll (HelenWorld p) = do
    hvar <- STM.newTVarIO s
    r <- runStdoutLoggingT . filterLogger (\_ l -> l >= ll) $ runReaderT p hvar
    h <- STM.atomically $ STM.readTVar hvar
    return (r,h)

-- | Fork an execution into a new thread
forkHelen :: HelenWorld () -> HelenWorld ()
forkHelen x = liftBaseWith $ \run -> void . forkIO . void $ run x


----------------------------------------------------------------------------------------------------
-- * Lenses
----------------------------------------------------------------------------------------------------

makeLenses ''ServicePool
makeLenses ''ServiceManager
