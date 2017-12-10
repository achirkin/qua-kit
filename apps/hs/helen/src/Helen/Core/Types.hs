-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Core `helen` types:
--     messages, clients, services, service pools,
--     and monads to make it all run.
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Helen.Core.Types
  ( -- * Base server-client relationship
    Helen (..), ClientId (..), Client (..), sendMessage
    -- * Working with services
  , ServiceInstance (..), ServiceManager (..), ServicePool (..), ServiceInfo (..)
  , TargetedMessage (..), SourcedMessage (..), SessionId (..), siName
    -- * Categorized messages
  , RequestRun (..)
  , BelongsToSession (..)
    -- * Monad support
  , HelenWorld, HelenRoom, HelenMonad (..)
  , runHelenProgram, forkHelen, runHelenRoom
    -- * Lenses
  , msgChannel, serviceManager
  , incomingMsgs, idleInstances, busyInstances
  , serviceMap, nextToken, currentCalls, namedPool
  , serviceInfo
  , isNonBlocking
  ) where

import           Data.Hashable
import           Data.Unique

import           Control.Concurrent           (forkIO)
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TVar  as STM
import           Control.Lens
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Logger
import           Control.Monad.RWS.Lazy       (MonadState(..), RWST(..))
import qualified Control.Monad.RWS.Lazy       as RWS
import qualified Control.Monad.STM            as STM
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Crypto.Random                (MonadRandom (..))
import qualified Data.Aeson                   as JSON
import           Data.ByteString              (ByteString)
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import qualified System.Log.FastLogger        as FastLogger
import           Path
import           Luci.Messages
import           Helen.Core.OptParse.Types

-- | Represent a connected client
newtype ClientId = ClientId Unique
  deriving (Eq,Ord,Hashable)

-- | ClientId plus a token from a corresponding run message
data SessionId = SessionId !ClientId !Token
  deriving (Eq,Ord)

instance Hashable SessionId where
  hashWithSalt s (SessionId i t) = hashWithSalt s i + hashWithSalt s t
  hash (SessionId i t) = hash i + hash t

-- | Information about a connected client
data Client = Client
  { queueMessage :: !(TargetedMessage -> HelenWorld ())
    -- ^ Queue message directly to a client message channel;
    --   normally, modules should use `sendMessage` message from Helen to send messages.
  , clientAddr   :: !String
    -- ^ Socket address of a client
  }

-- | The program state type
data Helen = Helen
  { _msgChannel         :: !(STM.TChan SourcedMessage)
    -- ^ The very core of Helen, all message processing goes through this channel
  , sendDirectMessage   :: !(TargetedMessage -> HelenWorld ())
    -- ^ Send a message to a given client (by client id)
  , registerClient      :: !(Client -> HelenWorld (ClientId, HelenWorld ()))
    -- ^ Register a send-message callback.
    --   Returns own cliendId and an unregister callback.
    --   This is used only in Core module (`helenChannels'`).
  , subscribeUnregister :: !(ClientId -> (ClientId -> HelenWorld ()) -> HelenWorld ())
    -- ^ Anyone can subscribe for event "client unregistered".
    --   This is called when a client with a given id cannot receive messages anymore.
    --   The second argument is an arbitrary action to do given an unregistered `ClientId`.
  , _serviceManager      :: !ServiceManager
    -- ^ Keeps track of all services
  , helenSettings        :: Settings
  }

-- | Put a message into Helen processing channel.
--   Use this function when implementing embedded services:
--   the messages appear in the common queue (channel) for processing.
sendMessage :: Helen -> SourcedMessage -> HelenWorld ()
sendMessage h = liftIO . STM.atomically . STM.writeTChan (_msgChannel h)


-- | The very core of Helen, all message processing goes through this channel
msgChannel :: Functor f => (STM.TChan SourcedMessage -> f (STM.TChan SourcedMessage)) -> Helen -> f Helen
msgChannel k h = fmap (\newC -> h { _msgChannel = newC }) (k $ _msgChannel h)

-- | Keeps track of all services
serviceManager :: Functor f => (ServiceManager -> f ServiceManager) -> Helen -> f Helen
serviceManager k h  = fmap (\newM -> h { _serviceManager = newM }) (k $ _serviceManager h)

-- | A handle representing TCP client connected as a service
data ServiceInstance = ServiceInstance !ClientId !ServiceName
  deriving (Eq)



-- | Helper to get service name
siName :: ServiceInstance -> ServiceName
siName (ServiceInstance _ sn) = sn

-- | Service pool manages available instances of services
data ServicePool = ServicePool
  { _incomingMsgs  :: !(Seq.Seq RequestRun)
    -- ^ store pending service tasks
  , _idleInstances :: !(Seq.Seq ServiceInstance)
    -- ^ round-robin sequence of idle service instances
  , _serviceInfo   :: !ServiceInfo
    -- ^ data coming with "RemoteRegister" message
  }

-- | Can this service handle multiple run requests concurrently?
--   If yes then helen never removes it from the idle instances pool.
--   When the the service invoked, Helen puts the service into the end of the pool instead.
isNonBlocking :: ServicePool -> Bool
isNonBlocking = nonBlocking . _serviceInfo

-- | Keep all services in one place
data ServiceManager = ServiceManager
  { _serviceMap    :: !(HashMap.HashMap ServiceName ServicePool)
    -- ^ store services by name
  , _nextToken     :: !Token
    -- ^ keep track of last Token to assign sequential numbers
  , _currentCalls  :: !(HashMap.HashMap SessionId ServiceName)
    -- ^ lookup service name by session id of a client calling this service
  , _busyInstances :: !(HashMap.HashMap SessionId (ServiceInstance, SessionId))
    -- ^ map of busy instances, so that it is easy to find instance that finished a task
  }



-- | Message with sender and receiver
data TargetedMessage = TargetedMessage !ClientId !ClientId !Message

-- | Message with sender
data SourcedMessage = SourcedMessage !ClientId !Message

-- | MsgRun with assigned callId and clientId
data RequestRun = RequestRun !SessionId !ServiceName !JSON.Object ![ByteString]

class BelongsToSession a where
  -- | get a session id from a message
  sessionId :: a -> SessionId

instance BelongsToSession TargetedMessage where
  sessionId (TargetedMessage _ c msg) = SessionId c $ msgToken msg
instance BelongsToSession SourcedMessage where
  sessionId (SourcedMessage c msg) = SessionId c $ msgToken msg
instance BelongsToSession RequestRun where
  sessionId (RequestRun s _ _ _) = s


----------------------------------------------------------------------------------------------------
-- * Monad for Helen
----------------------------------------------------------------------------------------------------



-- | Define helen monad as a `MonadState Helen` plus additional transformation from pure monad `HelenRoom`
class MonadState Helen m => HelenMonad m where
  liftHelen :: HelenRoom a -> m a

-- | Variant of Helen state monad without `IO` to do pure evaluation.
newtype HelenRoom t = HelenRoom { unRoom :: RWST () (Seq.Seq (Loc,LogSource,LogLevel,LogStr)) Helen Identity t }
  deriving (Functor, Applicative, Monad, MonadBase Identity)


instance MonadLogger HelenRoom where
  monadLoggerLog a b c d = HelenRoom . RWS.tell $ Seq.singleton (a,b,c,toLogStr d)

instance MonadState Helen HelenRoom where
  get = HelenRoom get
  put = HelenRoom . put
  state = HelenRoom . state

instance HelenMonad HelenRoom where
  liftHelen = id

-- | IO-full variant of Helen state with multithread-preserved state.
newtype HelenWorld t = HelenWorld { unWorld :: ReaderT (STM.TVar Helen) (LoggingT IO) t}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadBase IO)

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
  -- | Lift HelenMonad computation into the real world.
  -- Note, when lifting a pure computation into HelenWorld monad,
  -- helen state is locked in STM monad.
  -- Hence, all other threads are blocked if they try to access the state
  --  at the same time.
  -- Therefore, running long-running computations in HelenRoom is highly troublesome!
  liftHelen hr = do
      (r, logs) <- state (f . runIdentity . RWS.runRWST (unRoom hr) ())
      mapM_ putLog logs
      return r
    where
      f (r, h, logs) = ((r,logs), h)
      putLog (a,b,c,d) = monadLoggerLog a b c d


-- | Run a program in IO monad.
runHelenProgram :: Helen -> HelenWorld r -> IO (r,Helen)
runHelenProgram s (HelenWorld p) = do
    hvar <- STM.newTVarIO s
    let ll = settingsLogLevel . helenSettings $ s
    let mlf = settingsLogFile . helenSettings $ s
    let runWithLog = case mlf of
            Nothing -> runStdoutLoggingT
            Just lf -> runFileLoggingT $ toFilePath lf
    r <- runWithLog . filterLogger (\_ l -> l >= ll) $ runReaderT p hvar
    h <- STM.atomically $ STM.readTVar hvar
    return (r,h)

-- | Fork an execution into a new thread
forkHelen :: HelenWorld () -> HelenWorld ()
forkHelen x = liftBaseWith $ \run -> void . forkIO . void $ run x

-- | Run pure evaluation in HelenRoom.
runHelenRoom :: Helen -> LogLevel -> HelenRoom r -> (r, Helen, Seq.Seq Text)
runHelenRoom oldHelen loglvl hr = (r, newHelen, logs')
  where
     (r, newHelen, logs) = runIdentity $ RWS.runRWST (unRoom hr) () oldHelen
     logs' = toText <$> Seq.filter (\(_,_,l,_) -> l >= loglvl) logs
     toText (a,b,c,d) = Text.decodeUtf8 . FastLogger.fromLogStr $ defaultLogStr a b c d

----------------------------------------------------------------------------------------------------
-- * Lenses
----------------------------------------------------------------------------------------------------

makeLenses ''ServicePool
makeLenses ''ServiceManager



-- | A lens to a service pool by name of a service
namedPool :: Functor f
          => ServiceName
          -> (Maybe ServicePool -> f (Maybe ServicePool))
          -> ServiceManager -> f ServiceManager
namedPool sName k sm = fmap wrap (k . HashMap.lookup sName $ _serviceMap sm)
  where
    wrap mp = over serviceMap (HashMap.alter (const mp) sName) sm
