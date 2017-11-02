{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsNewR
    , getQuaViewSettingsFromScIdR
    , getQuaViewSettingsFromExIdR
    ) where

import Import
import qualified QuaTypes

getQuaViewSettingsNewR :: Handler Value
getQuaViewSettingsNewR = quaViewSettingsR NewSubmissionR Nothing Nothing

getQuaViewSettingsFromScIdR :: ScenarioId -> Handler Value
getQuaViewSettingsFromScIdR scId = do
  sc <- runDB $ get404 scId
  quaViewSettingsR (SubmissionR scId) (Just scId) (Just $ scenarioTaskId sc)

getQuaViewSettingsFromExIdR :: ScenarioProblemId -> Handler Value
getQuaViewSettingsFromExIdR mExId =
  quaViewSettingsR (NewSubmissionForExerciseR mExId) Nothing $ Just mExId

quaViewSettingsR :: Route App
                 -> Maybe ScenarioId
                 -> Maybe ScenarioProblemId
                 -> Handler Value
quaViewSettingsR curRoute mScId mExId = do
  mUsrId <- maybeAuthId
  app <- getYesod
  req <- waiRequest
  let routeUrl route = let appr = getApprootText guessApproot app req
                       in  yesodRender app appr route []
  returnJson $ QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = mUsrId >> Just ("ws" <> drop 4 (routeUrl LuciR))
    , getSubmissionGeometryUrl = mScId >>= return . routeUrl . SubmissionGeometryR
    , postSubmissionUrl        = mExId >>= return . routeUrl . SubmissionsR
    , reviewSettingsUrl        = mScId >>= return . routeUrl . QuaViewReviewSettingsR
    , viewUrl                  = routeUrl curRoute
    }
