{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    ) where


import Data.Aeson (encode)
import Import
import qualified Handler.Mooc.Scenario as S
import Types


-- | Set up settings for qua-view
--   * qua_view_mode=mode -- edit or view or full; full is default
--   * scenario_id=i -- if set up then load this scenario from database
getQuaViewSettingsR :: Handler TypedContent
getQuaViewSettingsR = do
    qua_view_mode <- fromMaybe "full" <$> getsSafeSession userSessionQuaViewMode
    scenarioLinkScale <- S.getScenarioLink

    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []
    return $ TypedContent typeJson $ toContent $ encode $ Settings {
        loggingUrl              = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
      , luciUrl                 = Just $ "ws" <> drop 4 (routeUrl LuciR)
      , getSubmissionGeoJsonUrl = routeUrl . fst <$> scenarioLinkScale
      , postSubmissionUrl       = Just $ routeUrl SubmitProposalR
      , reviewSettingsUrl       = Just $ routeUrl QuaViewReviewSettingsR
      , viewMode                = qua_view_mode
      , viewUrl                 = routeUrl HomeR
      }
