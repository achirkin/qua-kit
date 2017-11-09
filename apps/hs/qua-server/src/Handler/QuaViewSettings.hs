{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsNewR
    , getQuaViewSettingsFromScIdR
    , getQuaViewSettingsFromExIdR
    ) where

import Import
import qualified QuaTypes
import System.FilePath (takeDirectory)
import qualified Data.Text as Text

getQuaViewSettingsNewR :: Handler Value
getQuaViewSettingsNewR = quaViewSettingsR NewSubmissionR Nothing Nothing

getQuaViewSettingsFromScIdR :: ScenarioId -> Handler Value
getQuaViewSettingsFromScIdR scId = do
  sc <- runDB $ get404 scId
  quaViewSettingsR (SubmissionR scId) (Just scId) (Just $ scenarioExerciseId sc)

getQuaViewSettingsFromExIdR :: ExerciseId -> Handler Value
getQuaViewSettingsFromExIdR mExId =
  quaViewSettingsR (NewSubmissionForExerciseR mExId) Nothing $ Just mExId

quaViewSettingsR :: Route App
                 -> Maybe ScenarioId
                 -> Maybe ExerciseId
                 -> Handler Value
quaViewSettingsR curRoute mScId mExId = do
  mUsrId <- maybeAuthId
  app <- getYesod
  req <- waiRequest
  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
  returnJson QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = mUsrId >> Just ("ws" <> drop 4 (routeUrl LuciR))
    , getSubmissionGeometryUrl = routeUrl . SubmissionGeometryR <$> mScId
    , postSubmissionUrl        = routeUrl . SubmissionsR <$> mExId
    , reviewSettingsUrl        = routeUrl . QuaViewReviewSettingsR <$> mScId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    }
