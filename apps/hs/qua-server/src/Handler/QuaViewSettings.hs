{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewEditorSettingsR
    , getQuaViewExerciseSettingsR
    ) where

import Import
import qualified QuaTypes
import System.FilePath (takeDirectory)
import qualified Data.Text as Text


-- | If a user is not a student, use these generic settings
--   that give full access to qua-view functions
getQuaViewEditorSettingsR :: Handler Value
getQuaViewEditorSettingsR
  = quaViewSettingsR QuaViewEditorR Nothing Nothing
    QuaTypes.Permissions
    { canEditProperties = True
    }

-- | These settings are for students when we know their exercise id,
--   can save exercise submissions, write reviews, etc.
getQuaViewExerciseSettingsR :: ExerciseId -> UserId -> Handler Value
getQuaViewExerciseSettingsR exId uId
  = quaViewSettingsR (SubmissionR exId uId) (Just exId) (Just uId)
    QuaTypes.Permissions
    { canEditProperties = False
    }

quaViewSettingsR :: Route App
                 -> Maybe ExerciseId
                 -> Maybe UserId
                 -> QuaTypes.Permissions
                 -> Handler Value
quaViewSettingsR curRoute mcExId mUsrId perms = do
  app <- getYesod
  req <- waiRequest
  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
  returnJson QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = mUsrId >> Just ("ws" <> drop 4 (routeUrl LuciR))
    , getSubmissionGeometryUrl = fmap routeUrl $ SubmissionGeometryR <$> mcExId <*> mUsrId
    , putSubmissionUrl         = fmap routeUrl $ SubmissionR <$> mcExId <*> mUsrId
    , reviewSettingsUrl        = fmap routeUrl $ QuaViewReviewSettingsR <$> mcExId <*> mUsrId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    , permissions              = perms
    }
