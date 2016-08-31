-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.QuaViewSettings
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    ) where


import Import
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)
import qualified Handler.Mooc.Scenario as S


-- | Set up settings for qua-view
--   * qua_view_mode=mode -- edit or view or full; full is default
--   * scenario_id=i -- if set up then load this scenario from database
getQuaViewSettingsR :: Handler TypedContent
getQuaViewSettingsR = do
    qua_view_mode <- fromMaybe "full" <$> lookupSession "qua_view_mode"

    mscenario_id <- getScenarioId
    mscale <- getScenarioScale mscenario_id

    app <- getYesod
    req <- waiRequest
    let appr = getApprootText guessApproot app req
        quaviewHTTP = yesodRender app appr HomeR []
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
        quaViewLoggingWS = "ws" <> drop 4 (yesodRender app appr QVLoggingR [])
        submitHTTP = yesodRender app appr SubmitProposalR []
        mscenarioHTTP = flip (yesodRender app appr . ScenarioR) [] <$> mscenario_id
    return . TypedContent typeJson . toContent . object $
      [ "viewRoute"  .= quaviewHTTP
      , "luciRoute"  .= luciProxyWS
      , "loggingUrl" .= quaViewLoggingWS
      , "submitUrl"  .= submitHTTP
      , "profile"     .= qua_view_mode
      ] ^++^ fmap ("scenarioUrl" .=) mscenarioHTTP
        ^++^ fmap ("objectScale" .=) mscale

(^++^) :: [a] -> Maybe a -> [a]
xs ^++^ Just x = x:xs
xs ^++^ Nothing = xs

getScenarioId :: Handler (Maybe ScenarioId)
getScenarioId = do
    mtscenario_id <- lookupSession "scenario_id"
    case decimal <$> mtscenario_id of
      Just (Right (i, _)) -> return . Just $ toSqlKey i
      _ -> do
        mtscp_id <- lookupSession "custom_exercise_id"
        case decimal <$> mtscp_id of
          Just (Right (i, _)) -> S.getScenarioId $ toSqlKey i
          _ -> return Nothing

getScenarioScale :: Maybe ScenarioId -> Handler (Maybe Double)
getScenarioScale Nothing = return Nothing
getScenarioScale (Just i) = fmap (fmap scenarioScale) . runDB $ get i
