-----------------------------------------------------------------------------
--
-- Module      :  Handler.Mooc.Comment
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Comment
  ( postWriteReviewR
  , viewComments
  ) where


import Import
import Database.Persist.Sql (fromSqlKey, rawSql, Single(..))
import qualified Data.Text as Text

-- | Get post params:
--   * criterion - id
--   * val - criterion good or bad (1 or 0)
--   * comment - textual comment
postWriteReviewR :: ScenarioId -> Handler Value
postWriteReviewR scenarioId = do
    userId <- requireAuthId
    mcriterionId <- parseSqlKey <$> requirePostParam "criterion" "You must specify one criterion you review."
    criterionVal <- ("1" ==) <$> requirePostParam "val" "You must specify one criterion you review."
    comment <- fromMaybe "" <$> lookupPostParam "comment"
    criterionId <- case mcriterionId of
        Nothing -> invalidArgsI ["You must specify one criterion you review."::Text]
        Just i -> return i
    t <- liftIO getCurrentTime
    runDB $ do
      scenario <- get404 scenarioId
      when (userId == scenarioAuthorId scenario) $ invalidArgsI ["You cannot review yourself!"::Text]
      getBy (ReviewOf userId scenarioId criterionId) >>= \mr ->
        when (isJust mr)
             (invalidArgsI ["You have already voted this exact design according to this criterion"::Text])
      insert_ $ Review userId scenarioId criterionId criterionVal comment t
    return $ object [ "criterion" .= fromSqlKey criterionId
                    , "val" .= Number (if criterionVal then 1 else 0)
                    , "comment" .= comment
                    , "timestamp" .= t]

viewComments :: ScenarioId -> Handler Widget
viewComments scId = runDB $ do
    criteria <- selectList [] []
    wrapIt criteria . foldl' (
      \w (Single icon, Single name, Entity _ r) -> w <> commentW icon name r
     ) mempty <$> getReviews scId
  where
    wrapIt criteria w = do
      toWidgetHead [cassius|
        p.small-p > span.icon24
          height: 24px
          width: 24px
          font-size: 16px
          padding: 4px
        td.small-td > p.small-p
          margin: 2px
          padding: 0px
          line-height: 24px
        tr > td.small-td
          margin: 2px
          padding: 2px
        tr.small-tr.small-tr-please
          padding: 2px
          margin: 2px
        .comment-table
          margin: 5px 5px 5px 20px
          padding: 0px
      |]
      [whamlet|
        <div.comment-table>
          ^{writeCommentFormW scId criteria}
          <table.table>
            ^{w}
      |]


type Icon = Text
type UserName = Text

writeCommentFormW :: ScenarioId -> [Entity Criterion] -> Widget
writeCommentFormW scId criteria = [whamlet|
    <div.card>
      <div.card-main>
        <div.card-inner>
        <div.card-action>
          $forall (Entity cId criterion) <- criteria
            <span criterion="#{fromSqlKey cId}" style="opacity: 0.7;">
              #{preEscapedToMarkup $ criterionIcon criterion}
          <span.icon>thumb_up
          <span.icon>thumb_down
  |]

commentW :: Icon -> UserName -> Review -> Widget
commentW ico uname Review{..} = [whamlet|
    <tr.small-tr.small-tr-please>
      <td.small-td>
        <p.small-p>
          <span.icon.icon24.text-brand-accent>
            $if reviewPositive
              thumb_up
            $else
              thumb_down
        <p.small-p style="height:24px">
          #{preEscapedToMarkup ico}
      <td.small-td>
          <p.small-p.text-brand-accent>
            #{formatTime defaultTimeLocale "%Y.%m.%d - %H:%M" reviewTimestamp} - #{uname}
          <p.small-p>
            #{reviewComment}
  |]


getReviews :: ScenarioId -> ReaderT SqlBackend Handler [(Single Icon, Single UserName, Entity Review)]
getReviews scId = rawSql query [toPersistValue scId]
  where
    query = Text.unlines
          ["SELECT \"criterion\".icon, \"user\".name, ??"
          ,"FROM review"
          ,"INNER JOIN \"user\""
          ,"        ON \"user\".id = review.reviewer_id"
          ,"INNER JOIN \"criterion\""
          ,"        ON \"criterion\".id = review.criterion_id"
          ,"WHERE review.scenario_id = ?"
          ,"ORDER BY review.timestamp DESC;"
          ]


--Criterion
--    name         Text
--    description  Text
--    image        ByteString
--    icon         Text
--    CriterionDef name

--Review
--    reviewerId  UserId
--    scenarioId  ScenarioId
--    criterionId CriterionId
--    positive    Bool
--    comment     Text
--    ReviewOf reviewerId scenarioId criterionId
