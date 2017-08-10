{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Mooc.CompareProposals
  ( getCompareProposalsR
  , getCompareByCriterionR
  , postVoteForProposalR
  ) where


import Import hiding (on, (==.), groupBy, Value)
import Text.Blaze
import Database.Persist.Sql (rawSql, Single(..))
import Data.FileEmbed (embedStringFile)
import Application.Edx
import Application.Grading

import Database.Esqueleto
import qualified Database.Persist as P

postVoteForProposalR :: CriterionId -> ScenarioId -> ScenarioId -> Handler Html
postVoteForProposalR cId better worse = do
    userId <- requireAuthId
    mResId <- getsSafeSession userSessionEdxResourceId

    mexplanation <- lookupPostParam "explanation"
    vId <- runDB $ do
      t <- liftIO getCurrentTime
      mGradingId <- case mResId of
          Nothing -> pure Nothing
          Just eResId -> fmap (fmap entityKey) . getBy $ EdxGradeKeys eResId userId
      insert $ Vote userId cId better worse mexplanation t mGradingId
    runDB $ do
      -- update ratings
      updateRatingsOnVoting vId
      -- grade edX designs
      queueDesignGrade better
      queueDesignGrade worse

    mcustom_exercise_count <- getsSafeSession userSessionCustomExerciseCount
    case mcustom_exercise_count of
      Nothing -> do
        runDB $ queueVoteGrade vId
        redirect CompareProposalsR
      Just custom_exercise_count -> do
         compare_counter <- fromMaybe 0 <$> getsSafeSession userSessionCompareCounter
         case custom_exercise_count `compare` (compare_counter+1) of
            -- continue exercise
            GT -> do
              setSafeSession userSessionCompareCounter $ compare_counter + 1
              getCompareByCriterionR userId cId
            -- something strange happend, go to standard pipeline
            LT -> redirect CompareProposalsR
            -- Finish exercise!
            EQ -> do
              setMessage "You are done with this exercise, thank you! You can continue exploring the site or go back to edX."
              deleteSafeSession userSessionCustomExerciseCount
              deleteSafeSession userSessionCompareCounter

              -- send the base grade back to edX
              case mResId of
                Nothing -> return ()
                Just eResId -> do
                  ye <- getYesod
                  sendEdxGrade (appSettings ye) userId eResId 0.6 (Just "Automatic grade on design submission.")
              runDB $ queueVoteGrade vId
              redirect MoocHomeR


getCompareProposalsR :: Handler Html
getCompareProposalsR = do
  setUltDest MoocHomeR
  userId <- requireAuthId
  scpId <- getCurrentScenarioProblem
  getLeastPopularCriterion scpId userId >>= \mcID -> case mcID of
     Nothing  -> notFound
     Just cid -> getCompareByCriterionR userId cid



getCompareByCriterionR :: UserId -> CriterionId -> Handler Html
getCompareByCriterionR uId cId = do
  custom_exercise_count   <- fromMaybe 0 <$> getsSafeSession userSessionCustomExerciseCount
  compare_counter   <- fromMaybe 0 <$> getsSafeSession userSessionCompareCounter
  scpId <- getCurrentScenarioProblem
  let showPopup = custom_exercise_count > 0 && compare_counter == 0
  when showPopup $ void getMessages
  (criterion,msubs) <- runDB $ do
      cr <- get404 cId
      ms <- getLeastPopularSubmissions scpId uId cId
      return (cr,ms)
  case msubs of
    Nothing -> do
      setMessage "I am sorry, seems like you have voted too much already."
      fullLayout Nothing "Error" mempty
    Just (Entity k1 s1, Entity k2 s2) -> do
      _ <- getMessages
      fullLayout (Just . preEscapedToMarkup $ criterionIcon criterion)
                 ("Compare designs according to a "
                  <> toLower (criterionName criterion)
                  <> " criterion"
                  <> ( if compare_counter <= custom_exercise_count && custom_exercise_count > 0
                       then " (" <> pack (show $ compare_counter + 1) <> "/" <> pack (show $ custom_exercise_count) <> ")"
                       else ""
                     )
                  ) $ do
        setTitle "Compare design proposals"
        toWidgetHead
          [julius|
            function selectLeft() {
              $('#submitvoteform').attr('action','@{VoteForProposalR cId k1 k2}');
              $('#leftChoiceButton').css('opacity','1');
              $('#leftChoiceButtonA').html('<span class="icon">thumb_up</span> Selected');
              $('#leftChoiceButtonA').addClass('btn-red');
              $('#rightChoiceButton').css('opacity','0.6');
              $('#rightChoiceButtonA').html('<span class="icon">thumb_down</span> Selected');
              $('#rightChoiceButtonA').removeClass('btn-red');
              $('#votebutton').click(function(){$('#submitvoteform').submit();});
            }
            function selectRight() {
              $('#submitvoteform').attr('action','@{VoteForProposalR cId k2 k1}');
              $('#rightChoiceButton').css('opacity','1');
              $('#rightChoiceButtonA').html('<span class="icon">thumb_up</span> Selected');
              $('#rightChoiceButtonA').addClass('btn-red');
              $('#leftChoiceButton').css('opacity','0.6');
              $('#leftChoiceButtonA').html('<span class="icon">thumb_down</span> Selected');
              $('#leftChoiceButtonA').removeClass('btn-red');
              $('#votebutton').click(function(){$('#submitvoteform').submit();});
            }
          |]
        toWidgetBody
          [hamlet|
            $if showPopup
              ^{infoModal criterion}

            <div class="row">

              <div.col-lg-6.col-md-6.col-sm-10.col-xs-12>
                  <div class="card">
                    <aside.card-side.card-side-img.pull-left>
                      <img src="@{ProposalPreviewR k1}" width="100%">
                    <div class="card-main">
                      <div.card-inner>
                        <p style="white-space: pre-line; overflow-y: hidden; color: #555;">
                          #{prepareDescription s1}
                      <div.card-action>
                        <div.card-action-btn.pull-left>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect href="@{ViewProposalR k1}" target="_blank">
                            <span.icon>visibility
                            View
                        <div.card-action-btn.pull-left #leftChoiceButton style="opacity: 0.6;">
                          <a.btn.btn-flat.waves-attach.waves-effect #leftChoiceButtonA onclick="selectLeft()">
                            <span.icon>thumb_up
                            Select

              <div.col-lg-6.col-md-6.col-sm-10.col-xs-12>
                  <div class="card">
                    <aside.card-side.card-side-img.pull-left>
                      <img src="@{ProposalPreviewR k2}" width="100%">
                    <div class="card-main">
                      <div.card-inner>
                        <p style="white-space: pre-line; overflow-y: hidden; color: #555;">
                          #{prepareDescription s2}
                      <div.card-action>
                        <div.card-action-btn.pull-left>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect href="@{ViewProposalR k2}" target="_blank">
                            <span.icon>visibility
                            View
                        <div.card-action-btn.pull-left #rightChoiceButton style="opacity: 0.6;">
                          <a.btn.btn-flat.waves-attach.waves-effect #rightChoiceButtonA onclick="selectRight()">
                            <span.icon>thumb_up
                            Select

              <div.col-lg-9.col-md-10.col-sm-10.col-xs-12>
                  <div class="card">
                    <div class="card-main">
                      <div.card-inner>
                        <p class="card-heading">
                          Confirm your choice and vote
                        <form #submitvoteform method="post">
                          <div.form-group.form-group-label>
                            <label.floating-label for="explanation">
                               Explanation (optional)
                            <textarea.form-control.textarea-autosize id="explanation" name="explanation" rows="1">
                      <div.card-action>
                        <div.card-action-btn.pull-right>
                          <a.btn.btn-flat.btn-red.waves-attach.waves-effect #votebutton>
                            Vote!
          |]
    where
      infoModal criterion =
        [hamlet|
          <div.modal.modal-va-middle.fade #popuphelp aria-hidden="true" role="dialog" tabindex="-1">
              <div class="modal-dialog">
                <div class="card">
                  <aside class="card-side card-side-img pull-left card-side-moocimg">
                    <img src="@{CriteriaImgR cId}">
                  <div class="card-main">
                    <div.card-header.text-brand-accent>
                      <div class="card-inner">
                        <h3>
                          Compare designs exercise
                        Read the criterion description and then compare a series of design pairs.
                    <div.card-inner>
                      <h4.h4.margin-bottom-no.margin-top-no>
                        #{criterionName criterion}
                      #{preEscapedText $ criterionDescription criterion}
                    <div.card-action>
                      <div.card-action-btn.pull-right>
                        <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal">
                          Ok, let's go!
          <script type="text/javascript">
            \$( document ).ready(function() { $('#popuphelp').modal('show');});
        |]


prepareDescription :: Scenario -> Text
prepareDescription sc = if n > 3
                        then t
                        else unlines (ts ++ replicate (3 - n) "")
  where
    t = scenarioDescription sc
    ts = lines t
    n = length ts


-- | Select a criterion that was used least among others
getLeastPopularCriterion :: ScenarioProblemId -> UserId -> Handler (Maybe CriterionId)
getLeastPopularCriterion scpId uId =
    fmap getValue $ runDB $ select $ from $ \(problemCriterion `LeftOuterJoin` rating) -> do
      on (just (problemCriterion ^. ProblemCriterionProblemId) ==. rating ?. RatingProblemId)
      where_ (rating ?. RatingProblemId ==. just (val scpId))
      groupBy (rating ?. RatingAuthorId, problemCriterion ^. ProblemCriterionCriterionId)
      let ratingEvidence = coalesceDefault
            [ rating ?. RatingCurrentEvidenceW
            ]
            (val 0)
      let sum_' = sum_ ratingEvidence :: SqlExpr (Value (Maybe Double))
      orderBy [asc sum_']
      limit 1
      pure $ problemCriterion ^. ProblemCriterionCriterionId
  where
    getValue :: [(Value CriterionId)] -> Maybe CriterionId
    getValue ((Value n):_) = Just n
    getValue _ = Nothing

getLeastPopularSubmissions :: ScenarioProblemId -> UserId -> CriterionId -> ReaderT SqlBackend Handler (Maybe (Entity Scenario, Entity Scenario))
getLeastPopularSubmissions scpId uId cId = do
    r <- rawSql query ( [uid, uid, uid, toPersistValue scpId, uid, cid])
    case r of
      ((Single i1,Single i2):_) -> do
        ms1 <- get i1
        ms2 <- get i2
        return $ (,) <$> (Entity i1 <$> ms1) <*> (Entity i2 <$> ms2)
      _ -> return Nothing
  where
    uid = toPersistValue uId
    cid = toPersistValue cId
    query = $(embedStringFile "sql/get-least-popular-submissions.sql")

-- getLastExercise :: UserId -> ReaderT SqlBackend Handler (Maybe (EdxResourceId,Text,Text))
-- getLastExercise uId = getVal <$> rawSql query [toPersistValue uId]
--   where
--     getVal ((Single edre, Single o, Single i):_) = Just (toSqlKey edre,o,i)
--     getVal [] = Nothing
--     query = Text.unlines
--           ["SELECT edx_resource,edx_outcome_url,edx_result_id"
--           ,"FROM vote"
--           ,"WHERE edx_resource IS NOT NULL"
--           ,"  AND edx_outcome_url IS NOT NULL"
--           ,"  AND edx_result_id IS NOT NULL"
--           ,"  AND voter_id = ?"
--           ,"ORDER BY timestamp DESC"
--           ,"LIMIT 1;"
--           ]
