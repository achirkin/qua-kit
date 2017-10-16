{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    , getQuaViewSettingsFromScIdR
    , getQuaViewSettingsFromExIdR
    ) where

import Import
import Types

getQuaViewSettingsR :: Handler Value
getQuaViewSettingsR = quaViewSettingsR Nothing Nothing

getQuaViewSettingsFromScIdR :: ScenarioId -> Handler Value
getQuaViewSettingsFromScIdR scId = do
  sc <- runDB $ get404 scId
  quaViewSettingsR (Just scId) (Just $ scenarioTaskId sc)

getQuaViewSettingsFromExIdR :: ScenarioProblemId -> Handler Value
getQuaViewSettingsFromExIdR = quaViewSettingsR Nothing . Just

quaViewSettingsR :: Maybe ScenarioId
                 -> Maybe ScenarioProblemId
                 -> Handler Value
quaViewSettingsR mScId mExId = do
  mUsrId <- maybeAuthId
  app <- getYesod
  req <- waiRequest
  let routeUrl route = let appr = getApprootText guessApproot app req
                       in  yesodRender app appr route []
  returnJson $ Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = mUsrId >> Just ("ws" <> drop 4 (routeUrl LuciR))
    , getSubmissionGeometryUrl = mScId >>= return . routeUrl . SubmissionGeometryR
    , postSubmissionUrl        = mExId >>= return . routeUrl . SubmissionsR
    , reviewSettingsUrl        = mScId >>= return . routeUrl . QuaViewReviewSettingsR
    , viewUrl                  = routeUrl $ maybe MyCurrentScenarioR SubmissionR mScId
    }
