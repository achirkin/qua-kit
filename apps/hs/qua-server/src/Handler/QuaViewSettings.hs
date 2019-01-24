{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE CPP #-}
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
       { canEditProperties      = True
       , canEraseReloadGeometry = True
       , canAddDeleteGeometry   = True
       , canDownloadGeometry    = True
       , canModifyStaticObjects = True
       , showHiddenProperties   = True
       , showShareButton        = False
       , isViewerOnly           = False
       }

-- | These settings are for students when we know their exercise id,
--   can save exercise submissions, write reviews, etc.
getQuaViewExerciseSettingsR :: ExerciseId -> UserId -> Handler Value
getQuaViewExerciseSettingsR exId uId = do
  e <- runDB $ get404 exId
  quaViewSettingsR (SubmissionR exId uId) (Just exId) (Just uId)
    QuaTypes.Permissions
       { canEditProperties      = exerciseCanEditProperties e
       , canEraseReloadGeometry = False
       , canAddDeleteGeometry   = exerciseCanAddDeleteGeom e
       , canDownloadGeometry    = False
       , canModifyStaticObjects = False
       , showHiddenProperties   = False
       , showShareButton        = True
       , isViewerOnly           = False
       }

quaViewSettingsR :: Route App
                 -> Maybe ExerciseId
                 -> Maybe UserId
                 -> QuaTypes.Permissions
                 -> Handler Value
#if EXPO
quaViewSettingsR curRoute mcExId mAuthorId _ = do
  app <- getYesod
  req <- waiRequest
  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
      showDemo = isNothing mcExId || isNothing mAuthorId
  returnJson QuaTypes.Settings
    { loggingUrl               = Nothing
    , luciUrl                  = Nothing
    , getSubmissionGeometryUrl =
        if showDemo
        then Just . routeUrl $ StaticR data_demo_scenario
        else fmap routeUrl $ SubmissionGeometryR <$> mcExId <*> mAuthorId
    , getSubmissionInfoUrl     = fmap routeUrl $ SubmissionInfoR <$> mcExId <*> mAuthorId
    , putSubmissionUrl         = Nothing
    , reviewSettingsUrl        = fmap routeUrl $ QuaViewReviewSettingsR <$> mcExId <*> mAuthorId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    , permissions              = QuaTypes.Permissions
       { canEditProperties      = showDemo
       , canEraseReloadGeometry = showDemo
       , canAddDeleteGeometry   = showDemo
       , canDownloadGeometry    = True
       , canModifyStaticObjects = False
       , showHiddenProperties   = False
       , showShareButton        = False
       , isViewerOnly           = not showDemo
       }
    }
#else
quaViewSettingsR curRoute mcExId mAuthorId perms' = do
  app <- getYesod
  req <- waiRequest
  mUserId <- maybeAuthId
      -- show a submission url iff authorId == userId
  let filteredSubmissionR = case (==) <$> mUserId <*> mAuthorId of
        Just True  -> SubmissionR <$> mcExId <*> mAuthorId
        Nothing    -> Nothing
        Just False -> Nothing
      -- set viewer to not be able to do anything
      perms = case (==) <$> mUserId <*> mAuthorId of
        Just True  -> perms'
        Nothing    -> perms'
        Just False -> perms'
          { QuaTypes.canEditProperties      = False
          , QuaTypes.canEraseReloadGeometry = False
          , QuaTypes.canAddDeleteGeometry   = False
          , QuaTypes.canDownloadGeometry    = False
          , QuaTypes.canModifyStaticObjects = False
          , QuaTypes.isViewerOnly           = True
          }

  let appr = getApprootText guessApproot app req
      routeUrl route = yesodRender app appr route []
  returnJson QuaTypes.Settings {
      loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
    , luciUrl                  = ("ws" <> drop 4 (routeUrl LuciR)) <$ mUserId
    , getSubmissionGeometryUrl =
        if isNothing mUserId && isNothing mcExId && isNothing mAuthorId
        then Just . routeUrl $ StaticR data_demo_scenario
        else fmap routeUrl $ SubmissionGeometryR <$> mcExId <*> mAuthorId
    , getSubmissionInfoUrl     = fmap routeUrl $ SubmissionInfoR <$> mcExId <*> mAuthorId
    , putSubmissionUrl         = routeUrl <$> filteredSubmissionR
    , reviewSettingsUrl        = fmap routeUrl $ QuaViewReviewSettingsR <$> mcExId <*> mAuthorId
    , viewUrl                  = routeUrl curRoute
    , jsRootUrl                = Text.pack . takeDirectory . Text.unpack . routeUrl $ StaticR js_qua_view_js
    , permissions              = perms
    }
#endif
