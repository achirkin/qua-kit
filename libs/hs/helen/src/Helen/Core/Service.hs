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
  ( initServiceManager
--  , registerService
  ) where


import Data.Text (Text)
--import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

--import qualified Control.Monad.STM as STM
--import qualified Control.Concurrent.STM.TMVar as STM
--import qualified Control.Concurrent.STM.TChan as STM


import Helen.Core.Types
import Luci.Connect
import Luci.Messages


data ServiceCall = ServiceCall
  { scClient :: !ClientId
  , scService :: !ClientId

  }


initServiceManager :: ServiceManager
initServiceManager = ServiceManager HashMap.empty

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
--  | MsgProgress !(Maybe LuciMsgInfo) !Percentage !ServiceResult ![ByteString]
--    -- ^ result of a service execution,
--    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, percentage: 0, progress: null};
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'percentage', 'progress', optional attachments
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




