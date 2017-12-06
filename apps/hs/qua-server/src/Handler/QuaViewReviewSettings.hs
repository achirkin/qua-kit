module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where

import Database.Persist.Sql (fromSqlKey)
import Handler.Mooc.Reviews (fetchReviewsFromDb, currentCriteria)
import Import
import qualified QuaTypes.Review as QtR

getQuaViewReviewSettingsR :: ExerciseId -> UserId -> Handler Value
getQuaViewReviewSettingsR exId authorId = do
    app <- getYesod
    req <- waiRequest
    mUsrId <- maybeAuthId
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    criterions <- runDB $ currentCriteria exId
    reviews    <- runDB $ fetchReviewsFromDb exId authorId
    let canReview = maybe False (/= authorId) mUsrId
    returnJson QtR.ReviewSettings {
        criterions = flip map criterions $ \(Entity cId c) -> QtR.Criterion {
                          criterionId   = fromIntegral $ fromSqlKey cId
                        , criterionName = criterionName c
                        , criterionIcon = criterionIcon c
                        }
      , reviews    = reviews
      , reviewsUrl = if canReview
                     then Just . routeUrl $ ReviewsR exId authorId
                     else Nothing
      }
