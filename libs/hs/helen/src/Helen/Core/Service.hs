-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Service
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This module is responsible for managing all services:
--   register, unregister, process message.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core.Service
  ( defServiceManager
  , processMessage
  , registerService, unregisterService
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens.Operators
import qualified Control.Lens as Lens
import           Control.Monad.State.Lazy
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Foldable as Foldable (toList,foldl')
import           Data.Maybe (isJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq
--import           Data.Text (Text)

import Helen.Core.Types
import Luci.Messages


-- | Empty service manager
defServiceManager :: ServiceManager
defServiceManager = ServiceManager
  { _serviceMap   = HashMap.empty
  , _nextToken    = 1
  , _currentCalls = HashMap.empty
  , _busyInstances = HashMap.empty
  }

-- | Empty pool
defServicePool :: ServiceInfo -> ServicePool
defServicePool si = ServicePool
  { _incomingMsgs  = Seq.empty
  , _idleInstances = Seq.empty
  , _serviceInfo   = si
  }


-- | Just register a new instance, nobody needs to be informed
registerService :: ServiceInfo
                -> ServiceInstance
                -> HelenRoom ()
registerService info si@(ServiceInstance _ sName) = do
    serviceManager.namedPool sName %= Just . registerInPool
  where
    registerInPool Nothing = Lens.over idleInstances (|> si) (defServicePool info)
    registerInPool (Just p) = Lens.over idleInstances (|> si) p

-- | Unregister all occurrences of an instance
--   and send errors to all clients with pending requests
--   Returns a list of messages to send and
--     1. if the service remains available (provided by other instances)
--     2. how many instances were unregistered
unregisterService :: ServiceInstance
                  -> HelenRoom ((Bool, Int), [TargetedMessage])
unregisterService si@(ServiceInstance sourceCID sName@(ServiceName tname)) = do
    -- first I need to check if there are tasks pending for the service
    (clientCalls, othersHere) <-
      serviceManager.busyInstances %%= inspectBusies
    -- count how many idle instances were deregistered
    ni <- (fromMaybe 0 . fmap (Foldable.foldl' countInSeq 0 . _idleInstances))
          . Lens.view (serviceManager.namedPool sName) <$> get
    clientWaits <-
      if othersHere
      -- if there are instances from other clients, then just remove all idles related to this instances
      then do
        serviceManager.namedPool sName %= deleteIdle
        return []
      else serviceManager.namedPool sName %%= deletePool
    return . (,) (othersHere, length clientCalls + ni) . map sendWarning $ clientCalls ++ clientWaits
  where
    sendWarning (SessionId cId t) = TargetedMessage sourceCID cId . MsgError t $
        "Service " <> tname <> " has been unregistered. Try again later."
    countInSeq n si' = if si' == si then n+1 else n
    deleteIdle Nothing = Nothing
    deleteIdle (Just pool) = Just $ Lens.over idleInstances (Seq.filter (si /=)) pool
    deletePool Nothing = ([], Nothing)
    deletePool (Just pool) = ( map sessionId . Foldable.toList $ _incomingMsgs pool, Nothing)
    inspectBusies hm = let (cs',ss', oh) = HashMap.foldlWithKey' f ([],[],False) hm
                           f (cs,ss,others) ssId (ssi@(ServiceInstance _ n), csId)
                                     = if ssi == si then (csId:cs, ssId:ss, others)
                                                    else (cs,ss, n == sName || others)
                       in ((cs',oh), foldr HashMap.delete hm ss')



----------------------------------------------------------------------------------------------------

-- | The very core of Helen; reactions on all requests are implemented here
processMessage :: SourcedMessage -> HelenRoom [TargetedMessage]

-- run
processMessage (SourcedMessage clientId (MsgRun token sName@(ServiceName tName) pams bs)) = do
  -- early inerrupt if there is no such service
  hasService <- isJust . Lens.view (serviceManager.namedPool sName) <$> get
  if not hasService
  then return [ TargetedMessage clientId clientId
                $ MsgError token $ "Service " <> tName <> " is not available."
              ]
  else do
    -- create run request
    let requestRun = RequestRun (SessionId clientId token) sName pams bs
    -- then add a call to a current call map
    serviceManager.currentCalls %= HashMap.insert (SessionId clientId token) sName
    -- forward processing to a corresponding service pool
    mpoolMsgS <- serviceManager.namedPool sName %%= processRunMessage requestRun
    case mpoolMsgS of
      -- message queued in a pool, do nothing for now
      Nothing -> return []
      -- message ready to be sent to a service instance, add the service instance to busyInstances map
      Just (poolMsgF, Just si) -> do
        poolMsg <- serviceManager.nextToken %%= \i -> (poolMsgF i, i+1)
        serviceManager.busyInstances %= HashMap.insert (sessionId poolMsg) (si, sessionId requestRun)
        return [poolMsg]
      -- this typically means we send error back immediately
      Just (poolMsgF, Nothing) -> do
        poolMsg <- serviceManager.nextToken %%= \i -> (poolMsgF i, i+1)
        return [poolMsg]


-- cancel
processMessage smsg@(SourcedMessage _ (MsgCancel _)) | sesId <- sessionId smsg = do
  -- delete message from currentCalls
  msName <- serviceManager.currentCalls %%= (HashMap.lookup sesId &&& HashMap.delete sesId)
  case msName of
    -- probably, a task has already been finished
    Nothing -> return ()
    -- Try to cancel task if it is still in queue
    Just sName ->
      serviceManager.namedPool sName %= processCancelMessage (RequestCancel sesId)
  return []


-- result
processMessage smsg@(SourcedMessage _ (MsgResult _ _ _)) = processErrorOrResult smsg

-- error
processMessage smsg@(SourcedMessage _ (MsgError _ _)) = processErrorOrResult smsg

-- progress
--  almost the same process as for result
processMessage smsg@(SourcedMessage cId (MsgProgress token p mr bs)) | sesId <- sessionId smsg = do
  -- first of all locate instance
  msInstance <- HashMap.lookup sesId . Lens.view (serviceManager.busyInstances) <$> get
  case msInstance of
    -- if there is no such instance, the message may be sent by a service that was not asked to do anything
    Nothing -> return [ TargetedMessage cId cId (MsgError token "Cannot find you to be working on a task!") ]
    -- here the difference to result message is that neither current calls
    -- nor busyInstances or pools should be altered
    Just (_,SessionId clientId clientToken) -> do
      -- so, we just redirect the message to the client
      return [TargetedMessage cId clientId $ MsgProgress clientToken p mr bs]



----------------------------------------------------------------------------------------------------



-- | The most important part - logic of processing run message
processRunMessage :: RequestRun
                  -> Maybe ServicePool
                  -> (Maybe (Token -> TargetedMessage, Maybe ServiceInstance), Maybe ServicePool)
-- if none instance of a service registered, ServicePool will be Nothing
--   this means Helen sends back an error message
processRunMessage (RequestRun (SessionId clientId token) (ServiceName sName) _ _) Nothing =
  ( Just (\_ -> TargetedMessage clientId clientId $ MsgError token $ "Service " <> sName <> " is not available."
         , Nothing
         )
  , Nothing )
processRunMessage msg@(RequestRun (SessionId clientId _) sName pams bs) (Just pool)
    -- If there are idle instances, then ask one to do the job
  | ins@(ServiceInstance servClientId _) :< is <- Seq.viewl (_idleInstances pool)
      = ( Just ( \t -> TargetedMessage clientId servClientId $ MsgRun t sName pams bs
               , Just ins
               )
        , Just pool { _idleInstances = is }
        )
    -- If there are no idle instance, then queue the message
  | otherwise
      = ( Nothing
        , Just $ Lens.over incomingMsgs (|> msg) pool
        )



-- | The most important part - logic of processing run message
processCancelMessage :: RequestCancel
                     -> Maybe ServicePool
                     -> Maybe ServicePool
processCancelMessage _ Nothing = Nothing
processCancelMessage (RequestCancel sesId) (Just pool)
    = Just $ Lens.over incomingMsgs (Seq.filter dontRemove) pool
  where
    dontRemove (RequestRun sId _ _ _) = sId /= sesId


-- | Process terminal message in a session (either result or error).
--   The idea is to move corresponding instance from busy map to idle queue
--   or give another task to the instance
processErrorOrResult :: SourcedMessage -> HelenRoom [TargetedMessage]
processErrorOrResult smsg@(SourcedMessage serviceId msg) | sesId <- sessionId smsg = do
  -- first of all locate an instance
  msInstance <- serviceManager.busyInstances %%= (HashMap.lookup sesId &&& HashMap.delete sesId)
  case msInstance of
    -- if there is no such instance, the message may be sent by a service that was not asked to do anything
    Nothing -> return [ TargetedMessage serviceId serviceId (MsgError (msgToken msg) "Cannot find you to be working on a task!") ]
    Just (sInstance,reqSId@(SessionId clientId clientToken)) -> do
      -- release instance if there is no tasks in queue.
      mnextcall <- serviceManager.namedPool (siName sInstance) %%= releaseInstance sInstance
      -- filter out result if it was canceled
      msName <- serviceManager.currentCalls %%= (HashMap.lookup reqSId &&& HashMap.delete reqSId)
      let msgRezs = case msName of
             Nothing -> []
             Just _  -> [TargetedMessage serviceId clientId $ setToken clientToken msg]
      case mnextcall of
        -- service stays idle
        Nothing -> return msgRezs
        -- service continues work on a next message
        Just (nextMsgReqF, csId) -> do
          nextMsgReq <- serviceManager.nextToken %%= \i -> (nextMsgReqF i, i+1)
          serviceManager.busyInstances %= HashMap.insert (sessionId nextMsgReq) (sInstance, csId)
          return $ nextMsgReq : msgRezs

-- | Change a message token
setToken :: Token -> Message -> Message
setToken t (MsgRun _ s p b) = MsgRun t s p b
setToken t (MsgCancel _) = MsgCancel t
setToken t (MsgError _ e) = MsgError t e
setToken t (MsgProgress _ p r b) = MsgProgress t p r b
setToken t (MsgResult _ r b) = MsgResult t r b



-- | When a task has been finished, either add instance to idle list or give it another task
releaseInstance :: ServiceInstance
                -> Maybe ServicePool
                -> (Maybe (Token -> TargetedMessage, SessionId), Maybe ServicePool)
releaseInstance _ Nothing = (Nothing, Nothing)
releaseInstance si@(ServiceInstance serviceId _) (Just pool)
  -- if there are messages to be processed in a message queue, then return a new task
  | (RequestRun rsid@(SessionId clientId _) sName pams bs) :< imsgs <- Seq.viewl (_incomingMsgs pool)
    = ( Just (\token -> TargetedMessage clientId serviceId $ MsgRun token sName pams bs, rsid)
      , Just pool {_incomingMsgs = imsgs }
      )
  -- otherwise add instance to idle list
  | otherwise
    = ( Nothing
      , Just $ Lens.over idleInstances (|> si) pool
      )



-- | MsgProgress with service clientId
data ResponseProgress = ResponseProgress !SessionId !Percentage !(Maybe ServiceResult) ![ByteString]

-- | MsgCancel with requester clientId
data RequestCancel = RequestCancel !SessionId

instance BelongsToSession RequestCancel where
  sessionId (RequestCancel s) = s
instance BelongsToSession ResponseProgress where
  sessionId (ResponseProgress s _ _ _) = s

