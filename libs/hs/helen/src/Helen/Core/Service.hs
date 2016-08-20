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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Foldable as Foldable (toList)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq

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
unregisterService :: ServiceInstance
                  -> HelenRoom [TargetedMessage]
unregisterService si@(ServiceInstance _ sName@(ServiceName tname)) = do
    -- first I need to check if there are tasks pending for the service
    (clientCalls, othersHere) <-
      serviceManager.busyInstances %%= inspectBusies
    clientWaits <-
      if othersHere
      -- if there are instances from other clients, then just remove all idles related to this instances
      then do
        serviceManager.namedPool sName %= deleteIdle
        return []
      else serviceManager.namedPool sName %%= deletePool
    return $ map sendWarning $ clientCalls ++ clientWaits
  where
    sendWarning (SessionId cId t) = TargetedMessage cId . MsgError t $
        "Service " <> tname <> " has been unregistered. Try again later."
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
  then return [ TargetedMessage clientId
                $ MsgError token $ "Service " <> tName <> " is not available."
              ]
  else do
    -- create run request and update nextCallId
    requestRun@(RequestRun i _ _ _)
        <- serviceManager.nextToken %%= \i -> (RequestRun (SessionId clientId token) sName pams bs, i+1)
    -- then add a call to a current call map
    serviceManager.currentCalls %= HashMap.insert i sName
    -- forward processing to a corresponding service pool
    mpoolMsgS <- serviceManager.namedPool sName %%= processRunMessage requestRun
    case mpoolMsgS of
      -- message queued in a pool, do nothing for now
      Nothing -> return []
      -- message ready to be sent to a service instance, add the service instance to busyInstances map
      Just (poolMsg, Just si) -> do
        serviceManager.busyInstances %= HashMap.insert (sessionId poolMsg) (si, sessionId requestRun)
        return [poolMsg]
      -- this typically means we send error back immediately
      Just (poolMsg, Nothing) -> do
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
processMessage smsg@(SourcedMessage cId (MsgResult token rez bs)) | sesId <- sessionId smsg = do
  -- first of all locate instance
  msInstance <- serviceManager.busyInstances %%= (HashMap.lookup sesId &&& HashMap.delete sesId)
  case msInstance of
    -- if there is no such instance, the message may be sent by a service that was not asked to do anything
    Nothing -> return [ TargetedMessage cId (MsgError token "Cannot find you to be working on a task!") ]
    Just (sInstance,reqSId) -> do
      -- process result message and maybe another request queued in pool
      (msgRez, mmsgReq) <- serviceManager.namedPool (siName sInstance)
          %%= processResultMessage (sInstance,reqSId) (ResponseResult (SessionId cId token) rez bs)
      -- filter out result if it was canceled
      msName <- serviceManager.currentCalls %%= (HashMap.lookup sesId &&& HashMap.delete sesId)
      let msgRezs = case msName of
             Nothing -> []
             Just _  -> [msgRez]
      case mmsgReq of
        -- service stays idle
        Nothing -> return msgRezs
        -- service continues work on a next message
        Just (msgReq, csId) -> do
          serviceManager.busyInstances %= HashMap.insert (sessionId msgReq) (sInstance, csId)
          return $ msgReq : msgRezs


-- progress
--  almost the same process as for result
processMessage smsg@(SourcedMessage cId (MsgProgress token p mr bs)) | sesId <- sessionId smsg = do
  -- first of all locate instance
  msInstance <- HashMap.lookup sesId . Lens.view (serviceManager.busyInstances) <$> get
  case msInstance of
    -- if there is no such instance, the message may be sent by a service that was not asked to do anything
    Nothing -> return [ TargetedMessage cId (MsgError token "Cannot find you to be working on a task!") ]
    -- here the difference to result message is that neither current calls
    -- nor busyInstances or pools should be altered
    Just (_,SessionId clientId clientToken) -> do
      -- so, we just redirect the message to the client
      return [TargetedMessage clientId $ MsgProgress clientToken p mr bs]


-- error
--  logic of parsing error is exaclty the same as of parsing result
processMessage smsg@(SourcedMessage cId (MsgError token err)) | sesId <- sessionId smsg = do
  -- first of all locate instance
  msInstance <- HashMap.lookup sesId . Lens.view (serviceManager.busyInstances) <$> get
  case msInstance of
    -- if there is no such instance, the message may be sent by a service that was not asked to do anything
    Nothing -> return [ TargetedMessage cId (MsgError token "Cannot find you to be working on a task!") ]
    Just (sInstance,reqSId) -> do
      -- process result message and maybe another request queued in pool
      (msgRez, mmsgReq) <- serviceManager.namedPool (siName sInstance)
          %%= processErrorMessage (sInstance,reqSId) (ResponseError (SessionId cId token) err)
      -- filter out result if it was canceled
      msName <- serviceManager.currentCalls %%= (HashMap.lookup sesId &&& HashMap.delete sesId)
      let msgRezs = case msName of
             Nothing -> []
             Just _  -> [msgRez]
      case mmsgReq of
        -- service stays idle
        Nothing -> return msgRezs
        -- service continues work on a next message
        Just (msgReq, csId) -> do
          serviceManager.busyInstances %= HashMap.insert (sessionId msgReq) (sInstance, csId)
          return $ msgReq : msgRezs



----------------------------------------------------------------------------------------------------



-- | The most important part - logic of processing run message
processRunMessage :: RequestRun
                  -> Maybe ServicePool
                  -> (Maybe (TargetedMessage, Maybe ServiceInstance), Maybe ServicePool)
-- if none instance of a service registered, ServicePool will be Nothing
--   this means Helen sends back an error message
processRunMessage (RequestRun (SessionId clientId token) (ServiceName sName) _ _) Nothing =
  ( Just ( TargetedMessage clientId $ MsgError token $ "Service " <> sName <> " is not available."
         , Nothing
         )
  , Nothing )
processRunMessage msg@(RequestRun (SessionId _ token) sName pams bs) (Just pool)
    -- If there are idle instances, then ask one to do the job
  | ins@(ServiceInstance servClientId _) :< is <- Seq.viewl (_idleInstances pool)
      = ( Just ( TargetedMessage servClientId $ MsgRun token sName pams bs
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
    dontRemove (RequestRun sId _ _ _) = sId == sesId



-- | Redirect result and put a servie back to idle instances sequence.
--   The first message is a result or error, the second is a possible run message to a released service instance
processResultMessage :: (ServiceInstance, SessionId)
                     -> ResponseResult
                     -> Maybe ServicePool
                     -> ((TargetedMessage, Maybe (TargetedMessage, SessionId)), Maybe ServicePool)
-- If there is no such service pool, then result comes from a not registered service
processResultMessage _ (ResponseResult (SessionId clientId token) _ _) Nothing
  = ( ( TargetedMessage clientId (MsgError token "Cannot find you to be registered as a service!")
      , Nothing
      )
    , Nothing
    )
-- normal pipeline
processResultMessage (si, (SessionId cClientId cToken))
                     (ResponseResult (SessionId sClientId _) rez bs) (Just pool)
  -- if there are messages to be processed in a queue, then immediately forward them to a service
  | (RequestRun rsid@(SessionId _ rToken) rsName rreq rbs) :< imsgs <- Seq.viewl (_incomingMsgs pool)
    = ( ( resultMsg
        , Just (TargetedMessage sClientId $ MsgRun rToken rsName rreq rbs, rsid)
        )
        , Just $ pool {_incomingMsgs = imsgs }
      )
  -- otherwise add instance to idle list
  | otherwise
    = ( ( resultMsg
        , Nothing
        )
        , Just $ Lens.over idleInstances (|> si) pool
      )
  where
    resultMsg = TargetedMessage cClientId $ MsgResult cToken rez bs




-- | Exactly the same as processResultMessage
--   TODO: need to merge the code for result and error logic
processErrorMessage :: (ServiceInstance, SessionId)
                     -> ResponseError
                     -> Maybe ServicePool
                     -> ((TargetedMessage, Maybe (TargetedMessage, SessionId)), Maybe ServicePool)
-- If there is no such service pool, then result comes from a not registered service
processErrorMessage _ (ResponseError (SessionId clientId token) _) Nothing
  = ( ( TargetedMessage clientId (MsgError token "Cannot find you to be registered as a service!")
      , Nothing
      )
    , Nothing
    )
-- normal pipeline
processErrorMessage (si, (SessionId cClientId cToken))
                    (ResponseError (SessionId sClientId _) err) (Just pool)
  -- if there are messages to be processed in a queue, then immediately forward them to a service
  | (RequestRun rsid@(SessionId _ rToken) rsName rreq rbs) :< imsgs <- Seq.viewl (_incomingMsgs pool)
    = ( ( resultMsg
        , Just (TargetedMessage sClientId $ MsgRun rToken rsName rreq rbs, rsid)
        )
        , Just $ pool {_incomingMsgs = imsgs }
      )
  -- otherwise add instance to idle list
  | otherwise
    = ( ( resultMsg
        , Nothing
        )
        , Just $ Lens.over idleInstances (|> si) pool
      )
  where
    resultMsg = TargetedMessage cClientId $ MsgError cToken err



