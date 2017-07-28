{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    ) where


import Import
import Model.Session
import qualified Handler.Mooc.Scenario as S


-- | Set up settings for qua-view
--   * qua_view_mode=mode -- edit or view or full; full is default
--   * scenario_id=i -- if set up then load this scenario from database
getQuaViewSettingsR :: Handler TypedContent
getQuaViewSettingsR = do
    qua_view_mode <- fromMaybe "full" <$> getsSafeSession userSessionQuaViewMode

    scenarioLinkScale <- S.getScenarioLink

    app <- getYesod
    req <- waiRequest
    let appr = getApprootText guessApproot app req
        quaviewHTTP = yesodRender app appr HomeR []
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
        quaViewLoggingWS = "ws" <> drop 4 (yesodRender app appr QVLoggingR [])
        submitHTTP = yesodRender app appr SubmitProposalR []
        mscenarioHTTP = flip (yesodRender app appr) [] . fst <$> scenarioLinkScale
    return . TypedContent typeJson . toContent . object $
      [ "viewRoute"  .= quaviewHTTP
      , "luciRoute"  .= luciProxyWS
      , "loggingUrl" .= quaViewLoggingWS
      , "submitUrl"  .= submitHTTP
      , "profile"     .= qua_view_mode
      ] ^++^ fmap ("scenarioUrl" .=) mscenarioHTTP
        ^++^ fmap (("objectScale" .=) . snd) scenarioLinkScale

(^++^) :: [a] -> Maybe a -> [a]
xs ^++^ Just x = x:xs
xs ^++^ Nothing = xs
