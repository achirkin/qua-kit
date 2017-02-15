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
import Control.Monad.Trans.Except
import Database.Persist.Sql (fromSqlKey, rawSql, Single(..))
import qualified Data.Text as Text

-- | Get post params:
--   * criterion - id
--   * val - criterion good or bad (1 or 0)
--   * comment - textual comment
postWriteReviewR :: ScenarioId -> Handler Value
postWriteReviewR scenarioId = runJSONExceptT $ do
    userId <- maybeE "You must login to review." maybeAuthId
    criterionId <- maybeE "You must specify one criterion you review." $
      (>>= parseSqlKey) <$> lookupPostParam "criterion"
    criterionVal <- fmap ("1" ==) $
      maybeE "You must specify one criterion you review." $
      lookupPostParam "val"
    comment <- lift $ fromMaybe "" <$> lookupPostParam "comment"

    t <- liftIO getCurrentTime
    ExceptT $ runDB $ runExceptT $ do
      scenario <- maybeE "Cannot find a correcsponding scenario." $ get scenarioId
      when (userId == scenarioAuthorId scenario) $
        throwE "You cannot review yourself!"
      oreview <- lift $ getBy (ReviewOf userId scenarioId criterionId)
      when (isJust oreview) $
        throwE "You have already voted on this exact design according w.r.t. this criterion."
      lift $ insert_ $ Review userId scenarioId criterionId criterionVal comment t
    return $ object [ "criterion" .= fromSqlKey criterionId
                    , "val" .= Number (if criterionVal then 1 else 0)
                    , "comment" .= comment
                    , "timestamp" .= t]


maybeE :: (Monad m) => Text -> m (Maybe a) -> ExceptT Text m a
maybeE errtxt m = ExceptT $ m >>= \mv -> case mv of
  Nothing -> return $ Left errtxt
  Just v  -> return $ Right v

runJSONExceptT :: ExceptT Text Handler Value -> Handler Value
runJSONExceptT m = f <$> runExceptT m
  where
    f (Left err) = object ["error" .= err]
    f (Right v ) = v

viewComments :: ScenarioId -> Handler Widget
viewComments scId = do
    muserId <- maybeAuthId
    runDB $ do
      mscenario <- get scId
      case mscenario of
        Nothing -> return mempty
        Just sc -> do
          authorName <- get (scenarioAuthorId sc) >>= \mu -> case mu of
            Nothing -> return "anonymous"
            Just u  -> return $ userName u
          let scUpdateTime = scenarioLastUpdate sc
              canComment = case muserId of
                Nothing -> False
                Just uId -> uId /= scenarioAuthorId sc
          criteria <- selectList [] []
          wrapIt authorName
                 scUpdateTime
                 (scenarioDescription sc) canComment criteria . foldl' (
            \w (Single icon, Single name, Entity _ r) -> w <> commentW icon name r
           ) mempty <$> getReviews scId
  where
    wrapIt authorName scUpdateTime desc canComment criteria w = do
      toWidgetHead [cassius|
        div.card-comment.card
          padding: 0
        div.card-comment.card-main
          padding: 0
          margin: 0
        div.card-comment.card-inner
          padding: 2px
          margin: 0
          min-height: 40px
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
        #commentContainer
          overflow-x: visible
          overflow-y: auto
          margin: -1px
          padding: 1px
      |]
      toWidgetHead [julius|
        $(document).ready(function() {
          var nh = $(window).height() - $('#commentContainer').offset().top,
              oh = $('#commentContainer').height();
          if (nh < oh) {
            $('#commentContainer').css("margin-right", "-8px");
            $('#commentContainer').height(nh);
          }
        });
      |]
      [whamlet|
        <div.comment-table>
          <div.card-comment.card>
            <div.card-comment.card-main>
              <div.card-comment.card-inner>
                <p style="margin: 6px; color: #b71c1c; float: left;">
                  #{authorName}
                <p style="margin: 6px; color: #b71c1c; float: right;">
                  #{show $ utctDay $ scUpdateTime}
                <p style="margin: 8px;">&nbsp;
                <p style="white-space: pre-line; margin: 2px;">
                  #{desc}
          $if canComment
            ^{writeCommentFormW scId criteria}
          <div #commentContainer>
            <table.table style="margin-top: 0px;">
              ^{w}
      |]


type Icon = Text
type UserName = Text

writeCommentFormW :: ScenarioId -> [Entity Criterion] -> Widget
writeCommentFormW scId criteria = do
  toWidgetHead [cassius|
    div.card-comment.card-action
      padding: 4px
      margin: 0
      min-height: 28px
    span.card-comment.card-criterion
      height: 24px
      width: 24px
      margin: 0px 4px 0px 4px
      padding: 0px
      float: left
      opacity: 0.3
    span.card-comment.card-criterion:hover
      opacity: 1.0
      cursor: pointer
    span.card-comment.icon
      height: 24px
      width: 24px
      font-size: 16px
      padding: 4px
      margin: 0px 2px 0px 2px
      float: left
      opacity: 0.3
    span.card-comment
      height: 24px
      width: 24px
      font-size: 16px
      padding: 4px
      margin: 0px 2px 0px 2px
      float: left
    span.card-comment:hover
      opacity: 1.0
    span.card-comment.icon:hover
      opacity: 1.0
      cursor: pointer
    a.card-comment.btn
      height: 24px
      padding: 4px
      font-size: 16px
      float: right
      display: none
    div.card-comment.form-group
      margin: 16px 4px 4px 4px
    #voteinfo
      margin: 2px
      padding: 2px
  |]
  toWidgetHead [julius|
    function submitComment(){
        $.post(
          { url: '@{WriteReviewR scId}'
          , data: $('#commentForm').serialize()
          , success: function(result){
                resetAllComment();
                $("body").snackbar({
                  content: (result.error ? result.error : "Review sent. Refresh the page to see it.")
                });
            }
          });
    }
    var voteSet = false, critSet = false;
    function resetAllComment() {
      voteSet = false;
      critSet = false;
      $('#criterionIdI').val(undefined);
      $('#criterionValI').val(undefined);
      $('#commentI').val('');
      $('#voteCName').text('');
      $('#voteIndication').text('');
      $('span.card-comment.card-criterion').css('opacity','0.3');
      $('#downvoteSpan').css('opacity','0.3');
      $('#upvoteSpan').css('opacity','0.3');
      checkVote();
    }
    function voteCriterion(criterionId, criterionName) {
      $('#criterionIdI').val(criterionId);
      $('#voteCName').text(criterionName);
      $('span.card-comment.card-criterion').css('opacity','0.3');
      $('#cspan' + criterionId).css('opacity','1.0');
      critSet = true;
      checkVote();
    }
    function setUpvote() {
      $('#criterionValI').val(1);
      $('#voteIndication').text('Upvote ');
      $('#downvoteSpan').css('opacity','0.3');
      $('#upvoteSpan').css('opacity','1.0');
      voteSet = true;
      checkVote();
    }
    function setDownvote() {
      $('#criterionValI').val(0);
      $('#voteIndication').text('Downvote ');
      $('#downvoteSpan').css('opacity','1.0');
      $('#upvoteSpan').css('opacity','0.3');
      voteSet = true;
      checkVote();
    }
    function checkVote() {
      if(voteSet && critSet) {
        $('#voteSubmit').show();
      } else {
        $('#voteSubmit').hide();
      }
    }
  |]
  toWidgetBody [hamlet|
    <div.card-comment.card>
      <div.card-comment.card-main>
        <div.card-comment.card-inner>
          <form #commentForm method="post">
            <input type="hidden" #criterionIdI name="criterion">
            <input type="hidden" #criterionValI name="val">
            <div.card-comment.form-group.form-group-label>
              <label.floating-label for="commentI">Write a review
              <textarea.form-control.textarea-autosize form="commentForm" id="commentI" rows="1" name="comment">
            <p.card-comment #voteinfo>
              <span #voteIndication>
              <span #voteCName>
        <div.card-comment.card-action>
          $forall (Entity cId criterion) <- criteria
            <span.card-comment.card-criterion onclick="voteCriterion(#{fromSqlKey cId},'#{criterionName criterion}')" id="cspan#{fromSqlKey cId}">
              #{preEscapedToMarkup $ criterionIcon criterion}
          <span.card-comment>&nbsp;
          <span.card-comment.icon #upvoteSpan onclick="setUpvote()">thumb_up
          <span.card-comment.icon #downvoteSpan onclick="setDownvote()">thumb_down
          <a.card-comment.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect #voteSubmit onclick="submitComment()">
            <span.icon>check
            Send
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
          ,"WHERE review.scenario_id IN ("
          ,"    SELECT scenario.id FROM scenario"
          ,"    INNER JOIN (SELECT scenario.author_id, scenario.task_id FROM scenario WHERE scenario.id = ?) t"
          ,"            ON scenario.task_id = t.task_id AND scenario.author_id = t.author_id)"
          ,"ORDER BY review.timestamp DESC;"
          ]
