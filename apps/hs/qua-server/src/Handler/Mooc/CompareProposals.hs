{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Mooc.CompareProposals
  ( getCompareProposalsR
  , getCompareByCriterionR
  , postVoteForProposalR
  ) where


import Import
import Text.Blaze
import Database.Persist.Sql (rawSql, Single(..))
import qualified Data.Text as Text
import Application.Edx
import Application.Grading

postVoteForProposalR :: ExerciseId -> CriterionId
                     -> UserId -> UserId -> Handler Html
postVoteForProposalR exId cId betterUId worseUId = do
    Entity _ betterSc <- runDB . getBy404 $ SubmissionOf betterUId exId
    Entity _ worseSc  <- runDB . getBy404 $ SubmissionOf worseUId  exId
    let better = currentScenarioHistoryScenarioId betterSc
        worse  = currentScenarioHistoryScenarioId worseSc
    userId <- requireAuthId
    mResId <- getsSafeSession userSessionEdxResourceId

    mexplanation <- lookupPostParam "explanation"
    let mexplanation' = case Text.strip <$> mexplanation of
                          Just "" -> Nothing
                          Just ex -> Just ex
                          Nothing -> Nothing
    vId <- runDB $ do
      t <- liftIO getCurrentTime
      mGradingId <- case mResId of
          Nothing -> pure Nothing
          Just eResId -> fmap (fmap entityKey) . getBy $ EdxGradeKeys eResId userId
      insert $ Vote userId cId better worse mexplanation' t mGradingId
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
  exId <- getCurrentExercise
  runDB (getLeastPopularCriterion exId userId) >>= \mcID -> case mcID of
     Nothing  -> notFound
     Just cid -> getCompareByCriterionR userId cid



getCompareByCriterionR :: UserId -> CriterionId -> Handler Html
getCompareByCriterionR uId cId = do
  custom_exercise_count   <- fromMaybe 0 <$> getsSafeSession userSessionCustomExerciseCount
  compare_counter   <- fromMaybe 0 <$> getsSafeSession userSessionCompareCounter
  exId <- getCurrentExercise
  let showPopup = custom_exercise_count > 0 && compare_counter == 0
  when showPopup $ void getMessages
  (criterion,msubs) <- runDB $ do
      cr <- get404 cId
      ms <- getLeastPopularSubmissions exId uId cId
      return (cr,ms)
  case msubs of
    Nothing -> do
      setMessage "I am sorry, seems like you have voted too much already."
      fullLayout Nothing "Error" mempty
    Just (Entity _ s1, Entity _ s2) -> do
      let u1 = currentScenarioAuthorId s1
          u2 = currentScenarioAuthorId s2
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
              $('#submitvoteform').attr('action','@{VoteForProposalR exId cId u1 u2}');
              $('#leftChoiceButton').css('opacity','1');
              $('#leftChoiceButtonA').html('<span class="icon">thumb_up</span> Selected');
              $('#leftChoiceButtonA').addClass('btn-red');
              $('#rightChoiceButton').css('opacity','0.6');
              $('#rightChoiceButtonA').html('<span class="icon">thumb_down</span> Selected');
              $('#rightChoiceButtonA').removeClass('btn-red');
              $('#votebutton').click(function(){$('#submitvoteform').submit();});
            }
            function selectRight() {
              $('#submitvoteform').attr('action','@{VoteForProposalR exId cId u2 u1}');
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
                      <img src="@{ProposalPreviewR exId u1}" width="100%">
                    <div class="card-main">
                      <div.card-inner>
                        <p style="white-space: pre-line; overflow-y: hidden; color: #555;">
                          #{prepareDescription s1}
                      <div.card-action>
                        <div.card-action-btn.pull-left>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                              href="@{SubmissionR exId u1}" target="_blank">
                            <span.icon>visibility
                            View
                        <div.card-action-btn.pull-left #leftChoiceButton style="opacity: 0.6;">
                          <a.btn.btn-flat.waves-attach.waves-effect #leftChoiceButtonA onclick="selectLeft()">
                            <span.icon>thumb_up
                            Select

              <div.col-lg-6.col-md-6.col-sm-10.col-xs-12>
                  <div class="card">
                    <aside.card-side.card-side-img.pull-left>
                      <img src="@{ProposalPreviewR exId u2}" width="100%">
                    <div class="card-main">
                      <div.card-inner>
                        <p style="white-space: pre-line; overflow-y: hidden; color: #555;">
                          #{prepareDescription s2}
                      <div.card-action>
                        <div.card-action-btn.pull-left>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                              href="@{SubmissionR exId u2}" target="_blank">
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


prepareDescription :: CurrentScenario -> Text
prepareDescription cSc = if n > 3
                        then t
                        else unlines (ts ++ replicate (3 - n) "")
  where
    t = currentScenarioDescription cSc
    ts = lines t
    n = length ts


-- | Select a criterion that was used least among others
getLeastPopularCriterion :: ExerciseId -> UserId -> ReaderT SqlBackend Handler (Maybe CriterionId)
getLeastPopularCriterion exId uId = getValue <$> rawSql query [toPersistValue exId, toPersistValue uId]
  where
    getValue :: [(Single CriterionId)] -> Maybe CriterionId
    getValue ((Single n):_) = Just n
    getValue _ = Nothing
    query = Text.unlines
          ["SELECT criterion.id as id"
          ,"FROM criterion"
          ,"INNER JOIN exercise_criterion ON criterion.id = exercise_criterion.criterion_id AND exercise_criterion.exercise_id = ?"
          ,"LEFT OUTER JOIN"
          ,"    (SELECT vote.criterion_id as cid,COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0) as m, COALESCE(count(*),0) as n"
          ,"       FROM vote GROUP BY vote.criterion_id"
          ,"     UNION ALL"
          ,"     SELECT  review.criterion_id as cid,0 as m, COALESCE(count(*),0) as n FROM review GROUP BY  review.criterion_id) r"
          ,"    ON criterion.id = r.cid"
          ,"GROUP BY criterion.id"
          ,"ORDER BY SUM(r.m) ASC, SUM(r.n) ASC"
          ,"LIMIT 1;"
          ]

getLeastPopularSubmissions :: ExerciseId
                           -> UserId
                           -> CriterionId
                           -> ReaderT SqlBackend Handler
                                (Maybe (Entity CurrentScenario, Entity CurrentScenario))
getLeastPopularSubmissions exId uId cId = do
    r <- rawSql query ( [uid, uid, uid, toPersistValue exId, uid, cid])
    case r of
      ((Single i1,Single i2):_) -> do
        ms1 <- get i1
        ms2 <- get i2
        return $ (,) <$> (Entity i1 <$> ms1) <*> (Entity i2 <$> ms2)
      _ -> return Nothing
  where
    uid = toPersistValue uId
    cid = toPersistValue cId
    query = Text.unlines
          ["-- first query gets all submissions ordered by how many times they were voted"
          ,"WITH sc AS ("
          ,"SELECT current_scenario.*, t.nn, t.mm"
          ,"FROM current_scenario"
          ,"INNER JOIN ( SELECT current_scenario.author_id as author_id, current_scenario.exercise_id as exercise_id, MAX(current_scenario.last_update) as last_update, SUM (COALESCE(v.m, 0)) as mm, SUM (COALESCE(v.n, 0))  as nn"
          ,"             FROM current_scenario"
          ,"             LEFT OUTER JOIN"
          ,"                 ( SELECT  vote.better_id as sid, COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0) as m, count(*) as n FROM vote GROUP BY  vote.better_id"
          ,"                   UNION ALL"
          ,"                   SELECT  vote.worse_id as sid, COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0)  as m, count(*) as n FROM vote GROUP BY  vote.worse_id"
          ,"                 ) v"
          ,"                ON current_scenario.history_scenario_id = v.sid"
          ,"             WHERE current_scenario.author_id != ? AND current_scenario.exercise_id = ?"
          ,"             GROUP BY current_scenario.author_id, current_scenario.exercise_id"
          ,"             ORDER BY mm ASC, nn ASC"
          ,"           ) t"
          ,"        ON t.author_id = current_scenario.author_id AND t.exercise_id = current_scenario.exercise_id AND t.last_update = current_scenario.last_update"
          ,")"
          ,"-- get all pairs of designs"
          ,"SELECT s1.id, s2.id"
          ,"FROM sc s1 CROSS JOIN sc s2"
          ,"-- select only those pairs, wich never occured for a user"
          ,"LEFT OUTER JOIN (SELECT vote.better_id, vote.worse_id FROM vote WHERE vote.voter_id = ? AND vote.criterion_id = ?) vv"
          ,"        ON (vv.better_id = s1.id AND vv.worse_id = s2.id) OR (vv.better_id = s2.id AND vv.worse_id = s1.id)"
          ,"-- remove duplicates and submissions from different exercises"
          ,"WHERE vv.better_id IS NULL AND s1.id < s2.id AND s1.exercise_id = s2.exercise_id"
          ,"-- order by popularity, but randomize a little"
          ,"ORDER BY round(random()*4) ASC, (s1.mm + s2.mm) ASC, (s1.nn + s2.nn) ASC"
          ,"LIMIT 1;"
          ]


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
