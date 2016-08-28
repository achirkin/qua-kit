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
--import Data.Conduit.Network as Network
--import qualified Data.Conduit.List as CList
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)
import Data.Aeson (decodeStrict')
import Data.Aeson.Types (typeMismatch)
--import qualified Data.ByteString.Char8 as BSC


loggingApp :: UserId -> ScenarioProblemId -> WebSocketsT Handler ()
loggingApp uId scpId = sourceWS $$ mapM_C (\msg -> do
       t <- liftIO getCurrentTime
       case decodeStrict' msg of
         Nothing   -> return ()
         Just msgF -> lift . runDB . insert_ $ (msgF uId scpId t :: UserScenarioAction)
     )


instance FromJSON (UserId -> ScenarioProblemId -> UTCTime -> UserScenarioAction) where
  parseJSON (Object v) = do
    v11:v12:v13:v14
     :v21:v22:v23:v24
     :v31:v32:v33:v34
     :v41:v42:v43:v44:_ <- v .: "transform"
    geomId <- v .: "geomID"
    return $ \uId spId t -> UserScenarioAction
       uId spId t geomId
       v11 v12 v13 v14
       v21 v22 v23 v24
       v31 v32 v33 v34
       v41 v42 v43 v44
  parseJSON invalid = typeMismatch "UserScenarioAction" invalid


getQVLoggingR :: Handler Html
getQVLoggingR = do
    mUId <- maybeAuthId
    mcustom_exercise_id <- fmap (fmap (toSqlKey . fst) . decimal) <$> lookupSession "custom_exercise_id"
    case (,) <$> mUId <*> mcustom_exercise_id of
      Just (uId, Right eId) -> webSockets $ loggingApp uId eId
      _ -> return ()
    notFound

