{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
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

postReviewsR :: ScenarioId -> Handler Value
postReviewsR scenarioId = runJSONExceptT $ do
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
        throwE "You have already voted on this exact design according w.r.t. this criterion."
      let review = Review userId
                          scenarioId
                          criterionId
                          (fromThumb $ QtR.reviewPostThumb reviewPost)
                          (QtR.reviewPostComment reviewPost)
                          t
      reviewId <- lift $ insert review
      return $ reviewToTReview (userName user) (Entity reviewId review)

getReviewsR :: ScenarioId -> Handler Value
getReviewsR scId = do
    runDB $ do
      _ <- get404 scId
      reviews <- fetchReviewsFromDb scId
      returnJson reviews

fetchReviewsFromDb :: ScenarioId -> ReaderT SqlBackend Handler [QtR.Review]
fetchReviewsFromDb scId = do
  let query = Text.unlines
          ["SELECT \"user\".name, ??"
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
  return $ map (\(Single userName, review) -> reviewToTReview userName review) rows

reviewToTReview :: Text -> Entity Review -> QtR.Review
reviewToTReview userName (Entity reviewId r) = QtR.Review {
      reviewId          = fromIntegral $ fromSqlKey reviewId
    , reviewUserName    = userName
    , reviewCriterionId = fromIntegral $ fromSqlKey $ reviewCriterionId r
    , reviewThumb       = toThumb $ reviewPositive r
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
