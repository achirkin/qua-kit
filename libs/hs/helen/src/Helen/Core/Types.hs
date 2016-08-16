
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
module Helen.Core.Types
  ( -- * Base server-client relationship
    Helen (..), ClientId (..)
  , Client (..)
    -- * Working with services
  , Service (..), ServiceManager (..)
  ) where

import Data.Unique
import Data.Hashable

import Luci.Messages
import Luci.Connect
import qualified Data.HashMap.Strict as HashMap
import           Data.Functor.Identity (Identity(..))
import qualified Control.Concurrent.STM.TChan as STM
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Class
import           Control.Monad.State.Lazy
import qualified Control.Monad.Trans.State
import           Control.Monad.Logger
import           Crypto.Random (MonadRandom (..))

-- | Represent a connected client
newtype ClientId = ClientId Unique
  deriving (Eq,Ord, Hashable)

-- | Information about a connected client
data Client = Client
  { queueMessage :: !(Message -> LuciProgram Helen ())
    -- ^ Queue message directly to a client message channel;
    --   normally, modules should use `sendMessage` message from Helen to send messages.
  , clientAddr   :: !String
    -- ^ Socket address of a client
  }

-- | The program state type
data Helen = Helen
  { msgChannel          :: !(STM.TChan (ClientId, Message))
    -- ^ The very core of Helen, all message processing goes through this channel
  , sendMessage         :: !(ClientId -> Message -> LuciProgram Helen ())
    -- ^ Send a message to a given client (by client id)
  , registerClient      :: !(Client -> LuciProgram Helen (ClientId, LuciProgram Helen ()))
    -- ^ Register a send-message callback;
    --   Returns own cliendId and an unregister callback
  , subscribeUnregister :: !(ClientId -> (ClientId -> LuciProgram Helen ()) -> LuciProgram Helen ())
    -- ^ Anyone can subscribe for event "client unregistered".
    -- This will be called when a client with a given id cannot receive message anymore
  , serviceManager      :: !ServiceManager
    -- ^ Keeps track of all services
  }


-- | A handle representing TCP client connected as a service
data Service = RemoteService !ClientId !ServiceName


-- | Keep all services in one place
newtype ServiceManager
  = ServiceManager (HashMap.HashMap ServiceName Service)








---- | This data type wraps all necessary type transformers to run Luci service.
----   It has an embeded state `s` that is preserved throughout program execution.
----   The state `s` is a data type defined by a user;
----   for instance, it can be a unit type @()@ if one does not need to have a state at all.
--newtype LuciProgram s t = LuciProgram { unProgram :: StateT s (LoggingT IO) t }
--  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)
--
--instance MonadBaseControl IO (LuciProgram s) where
--  type StM (LuciProgram s) a = (a, s)
--  liftBaseWith f = LuciProgram $ liftBaseWith $ \q -> f (q . unProgram)
--  restoreM = LuciProgram . restoreM
--
--instance MonadRandom (LuciProgram s) where
--  getRandomBytes = liftIO . getRandomBytes
--
--instance MonadLogger (LuciProgram s) where
--  monadLoggerLog a b c d = LuciProgram . lift $ monadLoggerLog a b c d
--
--instance MonadState s (LuciProgram s) where
--  get = LuciProgram get
--  put = LuciProgram . put
--  state = LuciProgram . state



newtype HelenRoom t = HelenRoom { unRoom :: StateT Helen Identity t }
  deriving (Functor, Applicative, Monad, MonadBase Identity)

instance MonadState Helen HelenRoom where
  get = HelenRoom get
  put = HelenRoom . put
  state = HelenRoom . state

newtype HelenWorld t = HelenWorld { unWorld :: StateT Helen (LoggingT IO) t }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO HelenWorld where
  type StM HelenWorld a = (a, Helen)
  liftBaseWith f = HelenWorld $ liftBaseWith $ \q -> f (q . unWorld)
  restoreM = HelenWorld . restoreM

instance MonadRandom HelenWorld where
  getRandomBytes = liftIO . getRandomBytes

instance MonadLogger HelenWorld where
  monadLoggerLog a b c d = HelenWorld . lift $ monadLoggerLog a b c d

instance MonadState Helen HelenWorld where
  get = HelenWorld get
  put = HelenWorld . put
  state = HelenWorld . state
