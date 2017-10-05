module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where


import Data.Aeson (encode)
import Import
import Types

import Handler.Mooc.Comment (fetchReviewsFromDb, currentCriteria)

import Database.Persist.Sql (fromSqlKey)

getQuaViewReviewSettingsR :: ScenarioId -> Handler TypedContent
getQuaViewReviewSettingsR scId = do
    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    msc <- runDB $ get404 scId
    let taskId = scenarioTaskId msc
    criterions <- runDB $ currentCriteria taskId
    reviews    <- runDB $ fetchReviewsFromDb scId
    return $ TypedContent typeJson $ toContent $ encode $ ReviewSettings {
        criterions = flip map criterions $ \(Entity cId c) -> TCriterion {
                          tCriterionId   = fromIntegral $ fromSqlKey cId
                        , tCriterionName = criterionName c
                        , tCriterionIcon = criterionIcon c
                        }
      , reviews    = reviews
      , reviewsUrl = routeUrl (ReviewsR scId)
      }