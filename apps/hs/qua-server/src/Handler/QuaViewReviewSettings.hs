module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where

import Database.Persist.Sql (fromSqlKey)
import Handler.Mooc.Reviews (fetchReviewsFromDb, currentCriteria)
import Import
import Types

getQuaViewReviewSettingsR :: ScenarioId -> Handler Value
getQuaViewReviewSettingsR scId = do
    sc  <- runDB $ get404 scId
    app <- getYesod
    req <- waiRequest
    mUsrId <- maybeAuthId
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    msc <- runDB $ get404 scId
    let taskId = scenarioTaskId msc
    criterions <- runDB $ currentCriteria taskId
    reviews    <- runDB $ fetchReviewsFromDb scId
    let canReview = maybe False (/= scenarioAuthorId sc) mUsrId
    returnJson ReviewSettings {
        criterions = flip map criterions $ \(Entity cId c) -> TCriterion {
                          tCriterionId   = fromIntegral $ fromSqlKey cId
                        , tCriterionName = criterionName c
                        , tCriterionIcon = criterionIcon c
                        }
      , reviews    = reviews
      , reviewsUrl = if canReview
                     then Just $ routeUrl $ ReviewsR scId
                     else Nothing
      }
