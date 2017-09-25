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
    mscId <- getsSafeSession userSessionScenarioId

    scenarioLinkScale <- S.getScenarioLink

    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []
        quaviewHTTP      = routeUrl HomeR
        luciProxyWS      = "ws" <> drop 4 (routeUrl LuciR)
        quaViewLoggingWS = "ws" <> drop 4 (routeUrl QVLoggingR)
        submitHTTP       = routeUrl SubmitProposalR
        mwriteReviewUrl  = mscId >>= \scId -> return $ routeUrl (WriteReviewR scId)
        mscenarioHTTP    = routeUrl . fst <$> scenarioLinkScale
    -- return . TypedContent typeJson . toContent . object $
    --   [ "viewRoute"  .= quaviewHTTP
    --   , "luciRoute"  .= luciProxyWS
    --   , "loggingUrl" .= quaViewLoggingWS
    --   , "submitUrl"  .= submitHTTP
    --   , "profile"     .= qua_view_mode
    --   ] ^++^ fmap ("scenarioUrl" .=) mscenarioHTTP
    --     ^++^ fmap (("objectScale" .=) . snd) scenarioLinkScale
-- (^++^) :: [a] -> Maybe a -> [a]
-- xs ^++^ Just x = x:xs
-- xs ^++^ Nothing = xs
    return $ TypedContent typeJson $ toContent $ encode $ Settings {
        loggingUrl              = Just quaViewLoggingWS
      , luciUrl                 = Just luciProxyWS
      , getSubmissionGeoJsonUrl = mscenarioHTTP
      , postSubmissionUrl       = Just submitHTTP
      , postReviewUrl           = mwriteReviewUrl
      , viewMode                = qua_view_mode
      , viewUrl                 = quaviewHTTP
      }
