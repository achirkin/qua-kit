module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where

import Database.Persist.Sql (fromSqlKey)
import Handler.Mooc.Reviews (fetchReviewsFromDb, currentCriteria)
import Import
import Types

getQuaViewReviewSettingsR :: ScenarioId -> Handler Value
getQuaViewReviewSettingsR scId = do
    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    msc <- runDB $ get404 scId
    let taskId = scenarioTaskId msc
    criterions <- runDB $ currentCriteria taskId
    reviews    <- runDB $ fetchReviewsFromDb scId
    returnJson ReviewSettings {
        criterions = flip map criterions $ \(Entity cId c) -> TCriterion {
                          tCriterionId   = fromIntegral $ fromSqlKey cId
                        , tCriterionName = criterionName c
                        , tCriterionIcon = criterionIcon c
                        }
      , reviews    = reviews
      , reviewsUrl = routeUrl (ReviewsR scId)
      }
