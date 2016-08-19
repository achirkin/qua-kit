-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Service
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core.Service
  ( defServiceManager
--  , registerService
  ) where


import Data.Text (Text)
--import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence ((|>), ViewL(..))
import qualified Data.Sequence as Seq
--import qualified Control.Monad.STM as STM
--import qualified Control.Concurrent.STM.TMVar as STM
--import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Lens as Lens
import qualified Data.Aeson as JSON
import           Data.ByteString (ByteString)
import Data.Time (DiffTime)

import Helen.Core.Types
--import Luci.Connect
import Luci.Messages



defServiceManager :: ServiceManager
defServiceManager = ServiceManager
  { _serviceMap   = HashMap.empty
  , _nextCallId   = 1
  , _currentCalls = HashMap.empty
  }

defServicePool :: ServicePool
defServicePool = ServicePool
  { _incomingMsgs = Seq.empty
  , _idleInstances = Seq.empty
  , _busyInstances = HashMap.empty
  }


--processRunMsg :: SourcedMessage
--              -> ServiceManager
--              -> ([TargetedMessage], ServiceManager)
--processRunMsg (SourcedMessage requesterId msg@(MsgRun servName _ _)) man
--  | Just pool  <- HashMap.insert servName (_serviceMap man) =
--    ( [TargetedMessage requesterId $ MsgNewCallID callId]
--    , man
--      { _serviceMap = HashMap.insert servName pool' $ _serviceMap man
--      , _nextCallId = callId + 1
--      }
--    )
--  where
--    callId = _nextCallId man
--    pool' | Seq.EmptyL <- Seq.viewl (_idleInstances pool) = Lens.over incomingMsgs pool _


-- | Update service pool according to message, and, maybe prepare a message for someone
poolProcessMsg :: CallId -> SourcedMessage -> ServicePool -> (Maybe TargetedMessage, ServicePool)
-- process run message - comes from client, gets to service
poolProcessMsg callId smsg@(SourcedMessage requesterId msg@(MsgRun _ _ _)) pool
    -- If there are no idle instance, then queue message
  | EmptyL <- Seq.viewl (_idleInstances pool)
      = ( Nothing
        , Lens.over incomingMsgs (|> smsg) pool
        )
    -- If there are idle instances, then ask them to do job
  | ins@(RemoteService i _) :< is <- Seq.viewl (_idleInstances pool)
      = ( Just $ TargetedMessage i msg
        , pool { _idleInstances = is
               , _busyInstances = HashMap.insert i (callId, ins) (_busyInstances pool)
               }
        )
-- cancel
poolProcessMsg _ (SourcedMessage _ (MsgCancel _)) _
  = error "MsgCancel should have not gone so far. Need to think how to handle cancels properly"
poolProcessMsg _ (SourcedMessage _ (MsgNewCallID _)) _
  = error "NewCallId is unexpected inside poolProcessMsg"
-- I need to add client id to busy instancess or keep separate map callId->clientId.
-- Another issue is to how to properly delete message from wherever it is on MsgCancel?
--   Maybe an option is to have extended map callId -> (clientId,serviceName,?state?)
--poolProcessMsg callId (SourcedMessage servClientId msg@(MsgResult _ _ _)) pool
--  | Just (callId, serv) <- HashMap.lookup serviClientId $ _busyInstances pool
--      = ( Just




---- | A handle representing TCP client connected as a service
--data Service = RemoteService !ClientId !ServiceName
--
---- | Service pool manages available instances of services
--data ServicePool = ServicePool
--  { _incomingMsgs  :: !(Seq.Seq (ClientId, Message))
--    -- ^ store pending service tasks
--  , _idleInstances :: !(Seq.Seq Service)
--    -- ^ round-robin sequence of idle service instances
--  , _busyInstances :: !(HashMap.HashMap ClientId (CallId, Service))
--    -- ^ map of busy instances, so that it is easy to find instance that finished a task
--  }
--
--
---- | Keep all services in one place
--data ServiceManager = ServiceManager
--  { _serviceMap :: !(HashMap.HashMap ServiceName ServicePool)
--    -- ^ store services by name
--  , _nextCallId :: !CallId
--    -- ^ keep track of last CallId to assign sequential numbers
--  }

--registerService :: Service
--                -> HelenWorld ()
--registerService rs@(RemoteService clientId serviceName) = do
--    helen <- get
--    -- unregister service on client disconnect
--    subscribeUnregister helen clientId (const . modify $ insideHelen hmUngister)
--    -- register this service now
--    put $ insideHelen hmRegister helen
--  where
--    hmRegister = HashMap.insert serviceName rs
--    hmUngister = HashMap.delete serviceName
--    insideHelen f h@Helen{serviceManager = ServiceManager sm} = h{serviceManager = ServiceManager $ f sm}
--
--
--processMessage :: (ClientId, Message)
--               -> HelenWorld ()
--processMessage (clientId, msg@(MsgRun serviceName _ _)) = do
--  helen <- get
--  -- return message for now
--  sendMessage helen clientId msg
--  return ()

processMessage :: SourcedMessage
               -> HelenRoom [TargetedMessage]
processMessage (SourcedMessage clientId (MsgRun sName pams bs)) = undefined

-- | MsgRun with assigned callId and clientId
data RequestRun = RequestRun !CallId !ClientId !ServiceName !JSON.Object ![ByteString]

-- | MsgCancel with requester clientId
data RequestCancel = RequestCancel !CallId !ClientId

-- | MsgResult with target client
data ResponseResult = ResponseResult !CallId !ClientId !ServiceName !DiffTime !ServiceResult ![ByteString]

-- | MsgProgress with target client
data ResponseProgress = ResponseProgress !CallId !ClientId !ServiceName !DiffTime !Percentage !(Maybe ServiceResult) ![ByteString]

-- | MsgError with target client and callId
data ResponseError = ResponseError !CallId !ClientId !Text

--data Message
--  = MsgRun !ServiceName !JSON.Object ![ByteString]
--    -- ^ run service message, e.g. {'run': 'ServiceList'};
--    -- params: 'run', [(name, value)], optional attachments
--  | MsgCancel !CallId
--    -- ^ cancel service message, e.g. {'cancel': 25};
--    -- params: 'callID'
--  | MsgNewCallID !CallId
--    -- ^ Luci call id, { newCallID: 57 };
--    -- params: 'newCallID'
--  | MsgResult !(Maybe LuciMsgInfo) !ServiceResult ![ByteString]
--    -- ^ result of a service execution
--    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result', optional attachments
--  | MsgProgress !(Maybe LuciMsgInfo) !Percentage !(Maybe ServiceResult) ![ByteString]
--    -- ^ result of a service execution,
--    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, progress: 47, intermediateResult: null};
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'progress', 'intermediateResult', optional attachments
--  | MsgError !Text
--    -- ^ error message, e.g. {'error': 'We are in trouble!'};
--    -- params: 'error'

--data LuciMsgInfo = LuciMsgInfo
--  { lmiCallId      :: !CallId
--    -- ^ Unique callID is given by luci for every user request.
--  , lmiDuration    :: !DiffTime
--    -- ^ Duration of a service execution
--  , lmiServiceName :: !ServiceName
--    -- ^ Name of running service
--  , lmiTaskId      :: !TaskId
--    -- ^ taskID is assigned if a service runs within a workflow
--  } deriving (Eq, Show)




