module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where

import Database.Persist.Sql (fromSqlKey)
import Handler.Mooc.Reviews (fetchReviewsFromDb, currentCriteria)
import Import
import qualified QuaTypes.Review as QtR

getQuaViewReviewSettingsR :: CurrentScenarioId -> Handler Value
getQuaViewReviewSettingsR cScId = do
    cSc  <- runDB $ get404 cScId
    app <- getYesod
    req <- waiRequest
    mUsrId <- maybeAuthId
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    criterions <- runDB $ currentCriteria $ currentScenarioExerciseId cSc
    reviews    <- runDB $ fetchReviewsFromDb cScId
    let canReview = maybe False (/= currentScenarioAuthorId cSc) mUsrId
    returnJson QtR.ReviewSettings {
        criterions = flip map criterions $ \(Entity cId c) -> QtR.Criterion {
                          criterionId   = fromIntegral $ fromSqlKey cId
                        , criterionName = criterionName c
                        , criterionIcon = criterionIcon c
                        }
      , reviews    = reviews
      , reviewsUrl = if canReview
                     then Just $ routeUrl $ ReviewsR cScId
                     else Nothing
      }
