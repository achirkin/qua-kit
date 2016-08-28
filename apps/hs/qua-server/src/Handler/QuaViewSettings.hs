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
--import Data.Aeson
--import qualified Data.Text as Text

getQuaViewSettingsR :: Handler TypedContent
getQuaViewSettingsR = do
    muser <- maybeAuth
    custom_exercise_type <- lookupSession "custom_exercise_type"
    custom_exercise_id   <- lookupSession "custom_exercise_id"
    app <- getYesod
    req <- waiRequest
    let role = muserRole muser
        appr = getApprootText guessApproot app req
        quaviewHTTP = yesodRender app appr HomeR []
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
        quaViewLoggingWS = "ws" <> drop 4 (yesodRender app appr QVLoggingR [])
        submitHTTP = yesodRender app appr SubmitProposalR []
        -- get a link to a current EdX user problem or to an example scenario for other users
        scenarioHTTP = case (,) <$> custom_exercise_type
                                <*> fmap decimal custom_exercise_id of
            Just ("design", Right i) ->
                yesodRender app appr (ScenarioR . toSqlKey . fst $ i) []
            _ ->
                yesodRender app appr (StaticR data_mooctask_geojson) []
    return . TypedContent typeJson . toContent . object $
      [ "viewRoute"  .= quaviewHTTP
      , "luciRoute"  .= luciProxyWS
      , "loggingUrl" .= quaViewLoggingWS
      , "submitUrl"  .= submitHTTP
      , "scenarioUrl" .= scenarioHTTP
      , "objectScale" .= Number 0.001
      , "profile" .= String ( case role of
            UR_NOBODY  -> "full"
            UR_STUDENT -> case custom_exercise_type of
                           Just "design" -> "edit"
                           _ -> "view"
            UR_LOCAL   -> "full"
            UR_ADMIN   -> "full"
            )
      ]
