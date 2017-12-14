{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ExpertReview
  ( postWriteExpertReviewR
  , fetchExpertReviewsFromDb
  ) where


import Control.Monad.Trans.Except
import Import
import Import.Util
import Application.Grading
import Application.Edx
import qualified QuaTypes.Review as QtR

postWriteExpertReviewR :: ExerciseId -> UserId -> Handler Value
postWriteExpertReviewR exId authorId = runJSONExceptT $ do
    userId  <- maybeE "You must login to review." maybeAuthId
    grade   <- maybeE "You must specify a grade." $
                 liftM (>>= readMay) $ lookupPostParam "grade"
    comment <- lift $ fromMaybe "" <$> lookupPostParam "comment"
    t       <- liftIO getCurrentTime
    Entity _ cSc     <- lift $ runDB $ getBy404 $ SubmissionOf authorId exId
    ExceptT $ runDB $ runExceptT $ do
      when (userId == authorId) $
        throwE "You cannot review yourself!"
      when (length comment < 80) $
        throwE "Comment needs to be at least 80 characters."
      _ <- lift $ upsert (ExpertReview userId (currentScenarioHistoryScenarioId cSc) comment grade t)
                   [ ExpertReviewGrade     =. grade
                   , ExpertReviewComment   =. comment
                   , ExpertReviewTimestamp =. t ]

      -- expert grade overrules votes and thus overwrites existing grade
      lift $ updateCurrentScenarioGrade $ currentScenarioHistoryScenarioId cSc
      -- queue new grade for sending to edX
      lift $ queueDesignGrade $ currentScenarioHistoryScenarioId cSc

    return $ object [ "grade"     .= grade
                    , "comment"   .= comment
                    , "timestamp" .= t
                    ]


fetchExpertReviewsFromDb :: CurrentScenarioId -> Handler [QtR.Review]
fetchExpertReviewsFromDb cScId = do
  reviewsAndUsers <- fetchReviews cScId
  let toQtReview (r, mUsr) = QtR.Review {
        reviewUserName    = maybe "-" userName mUsr
      , reviewRating      = QtR.ExpertRating $ expertReviewGrade r
      , reviewComment     = expertReviewComment r
      , reviewTimestamp   = expertReviewTimestamp r
      }
  return $ map toQtReview reviewsAndUsers

fetchReviews :: CurrentScenarioId -> Handler [(ExpertReview, Maybe User)]
fetchReviews cScId = do
  cSc <- runDB $ get404 cScId
  let scId = currentScenarioHistoryScenarioId cSc
  reviews <- runDB $ selectList [ExpertReviewScenarioId ==. scId] []
  reviewsAndUsers <- forM reviews $ \(Entity _ r) -> do
    mReviewer <- runDB $ get $ expertReviewReviewerId r
    return (r, mReviewer)
  return reviewsAndUsers
