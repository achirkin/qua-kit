-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.LoggingWS
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.LoggingWS
  ( getQVLoggingR
  ) where



import Import

import Yesod.WebSockets
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Control.Monad.Trans.Reader as Reader
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)
import Data.Aeson (decodeStrict')
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HashMap
import Model.Session


loggingApp :: Maybe UserId -> Maybe ScenarioProblemId -> WebSocketsT Handler ()
loggingApp uId scpId = Reader.mapReaderT (`State.evalStateT` Nothing) $ sourceWS $$ mapM_C (\msg -> do
       t <- liftIO getCurrentTime
       case decodeStrict' msg of
         Nothing   -> return ()
         Just (WSLoad msgF) -> do
            i <- lift . lift. runDB . insert $ (msgF uId scpId t :: UserScenarioLoad)
            lift . State.put $ Just i
         Just (WSUpdate msgF) -> do
            mi <- lift State.get
            case mi of
              Nothing -> return ()
              Just i  -> lift . lift . runDB . insert_ $ (msgF i t :: UserScenarioUpdate)
         Just (WSAction msgF) -> do
            mi <- lift State.get
            case mi of
              Nothing -> return ()
              Just i  -> lift . lift . runDB . insert_ $ (msgF i t :: UserScenarioAction)
     )


instance FromJSON (UserScenarioLoadId -> UTCTime -> UserScenarioAction) where
  parseJSON (Object v) = do
    v11:v12:v13:v14
     :v21:v22:v23:v24
     :v31:v32:v33:v34
     :v41:v42:v43:v44:_ <- v .: "transform"
    geomId <- v .: "geomID"
    return $ \ldId t -> UserScenarioAction
       ldId t geomId
       v11 v12 v13 v14
       v21 v22 v23 v24
       v31 v32 v33 v34
       v41 v42 v43 v44
  parseJSON invalid = typeMismatch "UserScenarioAction" invalid

instance FromJSON (Maybe UserId -> Maybe ScenarioProblemId -> UTCTime -> UserScenarioLoad) where
  parseJSON (Object v) = do
    fc <- v .: "load"
    scale <- v .: "scale"
    return $ \uId spid t -> UserScenarioLoad uId spid t fc scale
  parseJSON invalid = typeMismatch "UserScenarioLoad" invalid

instance FromJSON (UserScenarioLoadId -> UTCTime -> UserScenarioUpdate) where
  parseJSON (Object v) = do
    fc <- v .: "update"
    return $ \ldId t -> UserScenarioUpdate ldId t fc
  parseJSON invalid = typeMismatch "UserScenarioUpdate" invalid


data WSInfo
  = WSLoad (Maybe UserId -> Maybe ScenarioProblemId -> UTCTime -> UserScenarioLoad)
  | WSUpdate (UserScenarioLoadId -> UTCTime -> UserScenarioUpdate)
  | WSAction (UserScenarioLoadId -> UTCTime -> UserScenarioAction)


instance FromJSON WSInfo where
  parseJSON (Object v) | HashMap.member "load" v   = WSLoad   <$> parseJSON (Object v)
                       | HashMap.member "update" v = WSUpdate <$> parseJSON (Object v)
                       | otherwise                 = WSAction <$> parseJSON (Object v)
  parseJSON invalid = typeMismatch "WSInfo" invalid


getQVLoggingR :: Handler Html
getQVLoggingR = do
    mUId <- maybeAuthId
    mcustom_exercise_id <- getsSafeSession userSessionCustomExerciseId
    webSockets $ loggingApp mUId mcustom_exercise_id
    notFound
