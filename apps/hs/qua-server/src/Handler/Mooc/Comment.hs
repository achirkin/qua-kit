{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Comment
  ( postReviewsR
  , getReviewsR
  , fetchReviewsFromDb
  , viewComments -- deprecated
  , currentCriteria
  ) where

import Import
import Import.Util
import Control.Monad.Trans.Except
import Database.Persist.Sql (fromSqlKey, toSqlKey, rawSql, Single(..))
import qualified Data.Text as Text
import Types

-- | we won't need this any more since reflex rewrite
viewComments :: ScenarioId -> Handler Widget
viewComments = undefined

postReviewsR :: ScenarioId -> Handler Value
postReviewsR scenarioId = runJSONExceptT $ do
    (Entity userId user) <- maybeE "You must login to review." maybeAuth
    reviewPost <- requireJsonBody

    t <- liftIO getCurrentTime
    let fromThumb ThumbUp   = True
        fromThumb ThumbDown = False
        fromThumb _ = error "invalid thumb state"
    let criterionId = toSqlKey $ fromIntegral $ Types.criterionId reviewPost
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
                          (fromThumb $ thumb reviewPost)
                          (reviewPostComment reviewPost)
                          t
      reviewId <- lift $ insert review
      return $ reviewToTReview (userName user) (Entity reviewId review)

getReviewsR :: ScenarioId -> Handler Value
getReviewsR scId = do
    runDB $ do
      _ <- get404 scId
      reviews <- fetchReviewsFromDb scId
      returnJson reviews

fetchReviewsFromDb :: ScenarioId -> ReaderT SqlBackend Handler [TReview]
fetchReviewsFromDb scId = do
  let query = Text.unlines
          ["SELECT \"user\".name, ??"
          ,"FROM review"
          ,"INNER JOIN \"user\""
          ,"        ON \"user\".id = review.reviewer_id"
          ,"WHERE review.scenario_id IN ("
          ,"    SELECT scenario.id FROM scenario"
          ,"    INNER JOIN (SELECT scenario.author_id, scenario.task_id FROM scenario WHERE scenario.id = ?) t"
          ,"            ON scenario.task_id = t.task_id AND scenario.author_id = t.author_id)"
          ,"ORDER BY review.timestamp DESC;"
          ]
  rows <- rawSql query [toPersistValue scId]
  return $ map (\(Single userName, review) -> reviewToTReview userName review) rows

reviewToTReview :: Text -> Entity Review -> TReview
reviewToTReview userName (Entity reviewId r) = TReview {
      tReviewId          = fromIntegral $ fromSqlKey reviewId
    , tReviewUserName    = userName
    , tReviewCriterionId = fromIntegral $ fromSqlKey $ reviewCriterionId r
    , tReviewThumb       = toThumb $ reviewPositive r
    , tReviewComment     = reviewComment r
    , tReviewTimestamp   = reviewTimestamp r
    }
  where
    toThumb True  = ThumbUp
    toThumb False = ThumbDown

currentCriteria :: ScenarioProblemId -> ReaderT SqlBackend Handler [Entity Criterion]
currentCriteria scp_id = rawSql query [toPersistValue scp_id]
  where
    query = unlines
        ["SELECT ?? FROM criterion,problem_criterion"
        ,"         WHERE problem_criterion.problem_id = ?"
        ,"           AND problem_criterion.criterion_id = criterion.id;"
        ]
