{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Reviews
  ( postReviewsR
  , getReviewsR
  , fetchReviewsFromDb
  , currentCriteria
  ) where

import Import
import Import.Util
import Control.Monad.Trans.Except
import Database.Persist.Sql (fromSqlKey, toSqlKey, rawSql, Single(..))
import qualified Data.Text as Text
import qualified QuaTypes.Review as QtR

postReviewsR :: ExerciseId -> UserId -> Handler Value
postReviewsR exId authorId = runJSONExceptT $ do
    Entity _ cSc <- lift . runDB . getBy404 $ SubmissionOf authorId exId
    let scenarioId = currentScenarioHistoryScenarioId cSc
    (Entity userId user) <- maybeE "You must login to review." maybeAuth
    reviewPost <- requireJsonBody

    t <- liftIO getCurrentTime
    let fromThumb QtR.ThumbUp   = True
        fromThumb QtR.ThumbDown = False
        fromThumb _ = error "invalid thumb state"
    let criterionId = toSqlKey $ fromIntegral $ QtR.reviewPostCriterionId reviewPost
    ExceptT $ runDB $ runExceptT $ do
      scenario <- maybeE "Cannot find a correcsponding scenario." $ get scenarioId
      when (userId == scenarioAuthorId scenario) $
        throwE "You cannot review yourself!"
      oreview <- lift $ getBy (ReviewOf userId scenarioId criterionId)
      when (isJust oreview) $
        throwE "You have already voted on this design and criterion combination."
      let review = Review userId
                          scenarioId
                          criterionId
                          (fromThumb $ QtR.reviewPostThumb reviewPost)
                          (QtR.reviewPostComment reviewPost)
                          t
      reviewId <- lift $ insert review
      return $ reviewToQtReview (userName user) True (Entity reviewId review)

getReviewsR :: ExerciseId -> UserId -> Handler Value
getReviewsR exId authorId = runDB $ do
  reviews <- fetchReviewsFromDb exId authorId
  returnJson reviews

fetchReviewsFromDb :: ExerciseId -> UserId -> ReaderT SqlBackend Handler [QtR.Review]
fetchReviewsFromDb exId authorId = do
  Entity _ cSc <- getBy404 $ SubmissionOf authorId exId
  mUsrId <- lift maybeAuthId
  let isMine reviewerId = case mUsrId of
        Just usrId -> reviewerId == usrId
        Nothing    -> False
  let scId = currentScenarioHistoryScenarioId cSc
  let query = Text.unlines
          ["SELECT \"user\".id, \"user\".name, ??"
          ,"FROM review"
          ,"INNER JOIN \"user\""
          ,"        ON \"user\".id = review.reviewer_id"
          ,"WHERE review.scenario_id IN ("
          ,"    SELECT scenario.id FROM scenario"
          ,"    INNER JOIN (SELECT scenario.author_id, scenario.exercise_id FROM scenario WHERE scenario.id = ?) t"
          ,"            ON scenario.exercise_id = t.exercise_id AND scenario.author_id = t.author_id)"
          ,"ORDER BY review.timestamp DESC;"
          ]
  rows <- rawSql query [toPersistValue scId]
  return $ map (\(Single userId, Single userName, review) ->
    reviewToQtReview userName (isMine userId) review) rows

reviewToQtReview :: Text -> Bool -> Entity Review -> QtR.Review
reviewToQtReview userName isMine (Entity _ r) = QtR.Review {
      reviewUserName    = userName
    , reviewIsMine      = isMine
    , reviewRating      = QtR.UserRating
                            (fromIntegral $ fromSqlKey $ reviewCriterionId r)
                            (toThumb $ reviewPositive r)
    , reviewComment     = reviewComment r
    , reviewTimestamp   = reviewTimestamp r
    }
  where
    toThumb True  = QtR.ThumbUp
    toThumb False = QtR.ThumbDown

currentCriteria :: ExerciseId -> ReaderT SqlBackend Handler [Entity Criterion]
currentCriteria exId = rawSql query [toPersistValue exId]
  where
    query = unlines
        ["SELECT ?? FROM criterion,exercise_criterion"
        ,"         WHERE exercise_criterion.exercise_id = ?"
        ,"           AND exercise_criterion.criterion_id = criterion.id;"
        ]
