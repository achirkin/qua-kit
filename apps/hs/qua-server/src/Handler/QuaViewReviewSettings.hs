module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where

import Database.Persist.Sql (fromSqlKey)
import Handler.Mooc.ExpertReview (fetchExpertReviewsFromDb)
import Handler.Mooc.Reviews (fetchReviewsFromDb, currentCriteria)
import Import
import qualified QuaTypes.Review as QtR

getQuaViewReviewSettingsR :: ExerciseId -> UserId -> Handler Value
getQuaViewReviewSettingsR exId authorId = do
    app <- getYesod
    req <- waiRequest
    mUsrEnt <- maybeAuth
    let mUsrId = entityKey <$> mUsrEnt
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    criterions  <- runDB $ currentCriteria exId
    userReviews <- runDB $ fetchReviewsFromDb exId authorId
    mcsc <- runDB $ getBy $ SubmissionOf authorId exId
    expertReviews <- case mcsc of
                       Just (Entity cScId _) -> fetchExpertReviewsFromDb cScId
                       Nothing               -> return []
    let reviews = reverse $ sortOn QtR.reviewTimestamp $ userReviews ++ expertReviews
    let canReview = maybe False (/= authorId) mUsrId
    let isExpert = muserRole mUsrEnt == UR_EXPERT
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
      , expertReviewsUrl = if isExpert
                           then Just . routeUrl $ ExpertReviewsR exId authorId
                           else Nothing
      }
