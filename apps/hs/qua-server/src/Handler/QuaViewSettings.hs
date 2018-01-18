{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewEditorSettingsR
    , getQuaViewExerciseSettingsR
    ) where

import Import
import qualified QuaTypes
import System.FilePath (takeDirectory)
import qualified Data.Text as Text
import Database.Persist.Sql (fromSqlKey)

-- | If a user is not a student, use these generic settings
--   that give full access to qua-view functions
getQuaViewEditorSettingsR :: Handler Value
getQuaViewEditorSettingsR
  = quaViewSettingsR QuaViewEditorR Nothing Nothing
    QuaTypes.Permissions
       { canEditProperties      = True
       , canEraseReloadGeometry = True
       , canAddDeleteGeometry   = True
       , canDownloadGeometry    = True
       , canModifyStaticObjects = True
       , showHiddenProperties   = True
       , showShareButton        = False
       }

-- | These settings are for students when we know their exercise id,
--   can save exercise submissions, write reviews, etc.
getQuaViewExerciseSettingsR :: ExerciseId -> UserId -> Handler Value
getQuaViewExerciseSettingsR exId uId
  = quaViewSettingsR (SubmissionR exId uId) (Just exId) (Just uId)
    QuaTypes.Permissions
       { canEditProperties      = False
       , canEraseReloadGeometry = False
       , canAddDeleteGeometry   = False
       , canDownloadGeometry    = False
       , canModifyStaticObjects = False
       , showHiddenProperties   = False
       , showShareButton        = True
       }

quaViewSettingsR :: Route App
                 -> Maybe ExerciseId
                 -> Maybe UserId
                 -> QuaTypes.Permissions
                 -> Handler Value
quaViewSettingsR curRoute mcExId mAuthorId perms = do
  app <- getYesod
  req <- waiRequest
  mUserId <- maybeAuthId
      -- show a submission url iff authorId == userId
  let filteredSubmissionR = case (==) <$> mUserId <*> mAuthorId of
        Just True  -> SubmissionR <$> mcExId <*> mAuthorId
        Nothing    -> Nothing
        Just False -> Nothing

  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
  returnJson QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = ("ws" <> drop 4 (routeUrl LuciR)) <$ mUserId
    , getSubmissionGeometryUrl = fmap routeUrl $ SubmissionGeometryR <$> mcExId <*> mAuthorId
    , putSubmissionUrl         = routeUrl <$> filteredSubmissionR
    , reviewSettingsUrl        = fmap routeUrl $ QuaViewReviewSettingsR <$> mcExId <*> mAuthorId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    , permissions              = perms
    }
