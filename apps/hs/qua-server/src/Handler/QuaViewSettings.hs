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

getQuaViewSettingsFromScIdR :: CurrentScenarioId -> Handler Value
getQuaViewSettingsFromScIdR cScId = do
  cSc <- runDB $ get404 cScId
  quaViewSettingsR (SubmissionR cScId) (Just cScId) (Just $ currentScenarioExerciseId cSc)

getQuaViewSettingsFromExIdR :: ExerciseId -> Handler Value
getQuaViewSettingsFromExIdR mExId =
  quaViewSettingsR (NewSubmissionForExerciseR mExId) Nothing $ Just mExId

quaViewSettingsR :: Route App
                 -> Maybe CurrentScenarioId
                 -> Maybe ExerciseId
                 -> Handler Value
quaViewSettingsR curRoute mcScId mExId = do
  mUsrId <- maybeAuthId
  app <- getYesod
  req <- waiRequest
  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
  returnJson QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = mUsrId >> Just ("ws" <> drop 4 (routeUrl LuciR))
    , getSubmissionGeometryUrl = routeUrl . SubmissionGeometryR <$> mcScId
    , postSubmissionUrl        = routeUrl . SubmissionsR <$> mExId
    , reviewSettingsUrl        = routeUrl . QuaViewReviewSettingsR <$> mcScId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    , permissions              = QuaTypes.permissions mempty
    }
