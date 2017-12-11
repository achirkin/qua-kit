{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ExpertReview
  ( postWriteExpertReviewR
  , writeExpertReview
  , fetchExpertReviewsFromDb
  , viewExpertReviews
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

viewExpertReviews :: CurrentScenarioId -> Handler Widget
viewExpertReviews cScId = do
  reviewsAndUsers <- fetchReviews cScId
  let avgGrade = (fromIntegral $ sum grades) /
                 (fromIntegral $ length reviewsAndUsers) :: Double
        where
          grades = map (expertReviewGrade . fst) reviewsAndUsers
  return $ do
    let stars review =
          let grade = expertReviewGrade review
          in  mconcat $
            (replicate grade [whamlet|<span.icon.icon-lg>star</span>|]) ++
            replicate (5 - grade) [whamlet|<span.icon.icon-lg>star_border</span>|]
    [whamlet|
      $if not $ null reviewsAndUsers
        <div.comment-table>
          #{length reviewsAndUsers} grade(s) with an average of #{ avgGrade } stars

          $forall (review, mReviewer) <- reviewsAndUsers
            <div.card-comment.card>
              <div.card-comment.card-main>
                <div.card-comment.card-inner>
                  <div>
                    ^{ stars review }
                    #{ formatTime defaultTimeLocale "%Y.%m.%d - %H:%M " $ expertReviewTimestamp review }
                    $maybe reviewer <- mReviewer
                      - #{userName reviewer}
                  #{expertReviewComment review}
    |]

writeExpertReview :: UserId -> CurrentScenarioId -> Handler Widget
writeExpertReview userId cScId = do
  cSc <- runDB $ get404 cScId
  let scId = currentScenarioHistoryScenarioId cSc
      exId = currentScenarioExerciseId cSc
      authorId = currentScenarioAuthorId cSc
  reviewExists <- liftM isJust $ runDB $ getBy $ ExpertReviewOf userId scId
  return $ do
    let starNrs = [1..5]::[Int]
    [whamlet|
      <div.comment-table #writeExpertReviewTable>
        <div.card-comment.card>
          <div.card-comment.card-main>
            <div.card-comment.card-inner>
              $if reviewExists
                <div class="alert alert-danger">
                  Replace the grade you previously submitted for this version:
              <div.card-comment.form-group.form-group-label>
                <label.floating-label for="commentER">Comments...
                <textarea.form-control.textarea-autosize #commentER rows="1" name="comment">
              <div .expertReviewStars>
                $forall i <- starNrs
                  <span.icon.icon-lg.reviewStar data-starid="#{ i }">star_border</span>
              <a.btn.btn-flat #submitExpertReview>Grade</a>
    |]
    toWidgetHead [cassius|
      .expertReviewStars .icon
        cursor: pointer;
    |]
    toWidgetHead [julius|
      $(function(){
        var grade
          , commentField = $('#commentER')[0]
          ;
        $('.reviewStar').click(function(e){
          var starId = e.target.getAttribute('data-starid');
          grade = starId;
          $('.reviewStar').each(function(){
            if (parseInt( this.getAttribute('data-starid') ) <= starId) {
              this.innerHTML = 'star';
            } else {
              this.innerHTML = 'star_border';
            }
          });
        });
        $('#submitExpertReview').click(function(e){
          if (grade && commentField.value.length > 80) {
            $.post(
              { url: '@{WriteExpertReviewR exId authorId}'
              , data: { grade:   grade
                      , comment: commentField.value
                      }
              , success: function(result){
                    $('#writeExpertReviewTable').remove();
                    $("body").snackbar({
                      content: (result.error
                                ? result.error
                                : "Review sent. Refresh the page to see it.")
                    });
                }
              });
          } else {
            alert('Please rate this design by clicking a star and add at least 80 characters of text before submitting.');
          }
        });
      });
    |]

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
