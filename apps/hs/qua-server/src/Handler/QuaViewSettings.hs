{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    ) where


import Data.Aeson (encode)
import Import
import Types

import Database.Persist.Sql (toSqlKey)

getQuaViewSettingsR :: Maybe ScenarioId -> Handler Value
getQuaViewSettingsR mScId = do
    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []
    returnJson $ Settings {
        loggingUrl               = Just $ "ws" <> drop 4 (routeUrl QVLoggingR)
      , luciUrl                  = Just $ "ws" <> drop 4 (routeUrl LuciR)
      , getSubmissionGeometryUrl = mScId >>= return . routeUrl . SubmissionGeometryR
      , postSubmissionUrl        = Just $ routeUrl SubmissionsR
      , reviewSettingsUrl        = mScId >>= return . routeUrl . QuaViewReviewSettingsR
      , editMode                 = undefined
      , viewUrl                  = routeUrl $ maybe MyCurrentScenarioR SubmissionR mScId
      }
