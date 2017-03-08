-----------------------------------------------------------------------------
--
-- Module      :  Handler.Mooc.CompareProposals
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
{-# Language CPP #-}
module Handler.Mooc.CompareProposals
  ( getCompareProposalsR
  , getCompareByCriterionR
  , postVoteForProposalR
  ) where


import Import
import Text.Blaze
import Database.Persist.Sql (rawSql, Single(..), toSqlKey)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Web.LTI


postVoteForProposalR :: CriterionId -> ScenarioId -> ScenarioId -> Handler Html
postVoteForProposalR cId better worse = do
  userId <- requireAuthId
  useSVars <- (\mt -> if mt == Just "compare" then id else const Nothing) <$> lookupSession "custom_exercise_type"
  resource_link_id        <- useSVars <$> lookupSession "resource_link_id"
  lis_outcome_service_url' <- useSVars <$> lookupSession "lis_outcome_service_url"
  lis_result_sourcedid'    <- useSVars <$> lookupSession "lis_result_sourcedid"
  mresId' <- case resource_link_id of
       Nothing -> return Nothing
       Just rlid -> runDB $ fmap entityKey <$> getBy (EdxResLinkId rlid)
  (mresId, lis_outcome_service_url, lis_result_sourcedid) <- case (,,) <$> mresId' <*> lis_outcome_service_url' <*> lis_result_sourcedid' of
      Just _  -> return (mresId', lis_outcome_service_url', lis_result_sourcedid')
      Nothing -> runDB (getLastExercise userId) >>= \mr -> case mr of
          Nothing -> return (mresId', lis_outcome_service_url', lis_result_sourcedid')
          Just (a,b,c) -> return (Just a, Just b, Just c)
  mexplanation <- lookupPostParam "explanation"
  runDB $ do
    t <- liftIO getCurrentTime
    _ <- insert $ Vote userId cId better worse mexplanation mresId
                       lis_outcome_service_url lis_result_sourcedid t
    return ()

  mcustom_exercise_count   <- (>>= parseInt) <$> lookupSession "custom_exercise_count"
  case mcustom_exercise_count of
    Nothing -> redirect CompareProposalsR
    Just custom_exercise_count -> do
       compare_counter <- fromMaybe 0 . (>>= parseInt) <$> lookupSession "compare_counter"
       case custom_exercise_count `compare` (compare_counter+1) of
          -- continue exercise
          GT -> do
            setSession "compare_counter" (pack . show $ compare_counter + 1)
            getCompareByCriterionR userId cId
          -- something strange happend, go to standard pipeline
          LT -> redirect CompareProposalsR
          -- Finish exercise!
          EQ -> do
            setMessage "You are done with this exercise, thank you! You can continue exploring the site or go back to edX."
            deleteSession "custom_exercise_count"
            deleteSession "compare_counter"

            -- send the base grade back to edX
            case (,) <$> lis_result_sourcedid <*> lis_outcome_service_url of
              Nothing -> return ()
              Just (sourcedId, outcomeUrl) -> do
                ye <- getYesod
                req <- replaceResultRequest (appLTICredentials $ appSettings ye) (Text.unpack outcomeUrl) sourcedId 0.6 Nothing
                _ <- httpNoBody req
                return ()
            redirect MoocHomeR


--updateSession :: Text -> (Maybe Text -> Maybe Text) -> Handler ()
--updateSession s f = lookupSession s >>= \m -> case f m of
--    Nothing -> return ()
--    Just v  -> setSession s v
--
--
--incrementTextValue :: Maybe Text -> Maybe Text
--incrementTextValue t = (pack . show . (+1)) <$> (t >>= parseInt)

parseInt :: Text -> Maybe Int
parseInt t = case Text.decimal t of
  Right (i,_) -> Just i
  _ -> Nothing

getCompareProposalsR :: Handler Html
getCompareProposalsR = do
  setUltDest MoocHomeR
  userId <- requireAuthId
  scpId <- getCurrentScenarioProblem
  runDB (getLeastPopularCriterion scpId userId) >>= \mcID -> case mcID of
     Nothing  -> notFound
     Just cid -> getCompareByCriterionR userId cid



getCompareByCriterionR :: UserId -> CriterionId -> Handler Html
getCompareByCriterionR uId cId = do
  custom_exercise_count   <- fromMaybe 0 . (>>= parseInt) <$> lookupSession "custom_exercise_count"
  compare_counter   <- fromMaybe 0 . (>>= parseInt) <$> lookupSession "compare_counter"
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
getLeastPopularCriterion :: ScenarioProblemId -> UserId -> ReaderT SqlBackend Handler (Maybe CriterionId)
getLeastPopularCriterion scpId uId = getValue <$> rawSql query [toPersistValue scpId, toPersistValue uId]
  where
    getValue :: [(Single CriterionId)] -> Maybe CriterionId
    getValue ((Single n):_) = Just n
    getValue _ = Nothing
    query = Text.unlines
          ["SELECT criterion.id as id"
          ,"FROM criterion"
          ,"INNER JOIN problem_criterion ON criterion.id = problem_criterion.criterion_id AND problem_criterion.problem_id = ?"
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
    query = Text.unlines
          ["-- first query gets all submissions ordered by how many times they were voted"
          ,"WITH sc AS ("
          ,"SELECT scenario.*, t.nn, t.mm"
          ,"FROM scenario"
          ,"INNER JOIN ( SELECT scenario.author_id as author_id, scenario.task_id as task_id, MAX(scenario.last_update) as last_update, SUM (COALESCE(v.m, 0)) as mm, SUM (COALESCE(v.n, 0))  as nn"
          ,"             FROM scenario"
          ,"             LEFT OUTER JOIN"
          ,"                 ( SELECT  vote.better_id as sid, COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0) as m, count(*) as n FROM vote GROUP BY  vote.better_id"
          ,"                   UNION ALL"
          ,"                   SELECT  vote.worse_id as sid, COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0)  as m, count(*) as n FROM vote GROUP BY  vote.worse_id"
          ,"                 ) v"
          ,"                          ON scenario.id = v.sid"
          ,"             WHERE scenario.author_id != ? AND scenario.task_id = ?"
          ,"             GROUP BY scenario.author_id, scenario.task_id"
          ,"             ORDER BY mm ASC, nn ASC"
          ,"           ) t"
          ,"        ON t.author_id = scenario.author_id AND t.task_id = scenario.task_id AND t.last_update = scenario.last_update"
          ,")"
          ,"-- get all pairs of designs"
          ,"SELECT s1.id, s2.id"
          ,"FROM sc s1 CROSS JOIN sc s2"
          ,"-- select only those pairs, wich never occured for a user"
          ,"LEFT OUTER JOIN (SELECT vote.better_id, vote.worse_id FROM vote WHERE vote.voter_id = ? AND vote.criterion_id = ?) vv"
          ,"        ON (vv.better_id = s1.id AND vv.worse_id = s2.id) OR (vv.better_id = s2.id AND vv.worse_id = s1.id)"
          ,"-- remove duplicates and submissions from different exercises"
          ,"WHERE vv.better_id IS NULL AND s1.id < s2.id AND s1.task_id = s2.task_id"
          ,"-- order by popularity, but randomize a little"
          ,"ORDER BY round(random()*4) ASC, (s1.mm + s2.mm) ASC, (s1.nn + s2.nn) ASC"
          ,"LIMIT 1;"
          ]


getLastExercise :: UserId -> ReaderT SqlBackend Handler (Maybe (EdxResourceId,Text,Text))
getLastExercise uId = getVal <$> rawSql query [toPersistValue uId]
  where
    getVal ((Single edre, Single o, Single i):_) = Just (toSqlKey edre,o,i)
    getVal [] = Nothing
    query = Text.unlines
          ["SELECT edx_resource,edx_outcome_url,edx_result_id"
          ,"FROM vote"
          ,"WHERE edx_resource IS NOT NULL"
          ,"  AND edx_outcome_url IS NOT NULL"
          ,"  AND edx_result_id IS NOT NULL"
          ,"  AND voter_id = ?"
          ,"ORDER BY timestamp DESC"
          ,"LIMIT 1;"
          ]
