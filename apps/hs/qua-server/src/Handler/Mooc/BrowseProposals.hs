-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.BrowseProposals
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.BrowseProposals
  ( getBrowseProposalsR
  ) where

import Import
import Import.BootstrapUtil
import qualified Data.Text as Text
import qualified Text.Blaze as Blaze
import Database.Persist.Sql (rawSql, Single (..))


pageSize :: Int
pageSize = 80

getBrowseProposalsR :: Int -> Handler Html
getBrowseProposalsR page = do
    role <- muserRole <$> maybeAuth
    let isExpert = role == UR_EXPERT
    ((res, widget), _) <- runFormGet proposalsForm
    let params = case res of
                   (FormSuccess ps) -> ps
                   _ -> noProposalParams
    usersscenarios <- runDB $ getLastSubmissions page params
    pages <- negate . (`div` pageSize) . negate <$> runDB (countUniqueSubmissions params)
    let is = [1..pages]
    fullLayout Nothing "Qua-kit student designs" $ do
      setTitle "Qua-kit student designs"
      toWidgetHead $
        -- move card-action down. Note, height 210 is 200 for image + 10 for overlapping margin of element above
        [julius|
          $(document).ready(function() {
            $('div.card-comment.card-action').each(function(){
                $(this).css('margin-top', Math.max(210 - $(this).position().top - $(this).height(), 0) + 'px');
              })
          });
        |]
      toWidgetHead $
        [cassius|
          span.card-comment.card-criterion
            width: 24px
            padding: 0px
            margin: 4px
            display: inline-block
            color: #ff6f00
            text-align: center
          .form-inline
            .form-group
              display: inline-block
              margin-left: 15px
            .btn
              margin-left: 15px
          .pageSelector
            display:inline
            background: none !important
            color: inherit
            border: none
            margin: 2px
            padding: 0 !important
            font: inherit
            cursor: pointer
            color: #ff6f00
            &:hover
              color: #b71c1c
        |]
      [whamlet|
        <form .form-inline>
          <div class="ui-card-wrap">
            <div class=row>
              ^{widget}
            <div class=row>
              $forall ((scId, scpId, uId), lu, desc, uname, (mexpertgrade, hasToBeGraded), crits) <- usersscenarios
                <div class="col-lg-4 col-md-6 col-sm-9 col-xs-9 story_cards">
                  <div.card>
                    <aside.card-side.card-side-img.pull-left.card-side-moocimg>
                      <img src="@{ProposalPreviewR scId}" width="200px" height="200px" style="margin-left: -25px;">
                    <div.card-main>
                      <div.card-inner style="margin: 10px 12px;">
                        <p style="margin: 6px 0px; color: #b71c1c;">
                          #{uname}
                            <br>
                          #{show $ utctDay $ lu}
                        <p style="margin: 6px 0px; white-space: pre-line; overflow-y: hidden; color: #555;">
                         #{shortComment desc}
                      <div.card-comment.card-action>
                        $forall (svg, cname, rating) <- crits
                         $if rating > 0
                          <span.card-comment.card-criterion style="opacity: #{cOpacity rating}" title="#{cname}">
                            #{svg}
                            <p style="display: inline; margin: 0; padding:0; color: rgb(#{156 + rating}, 111, 0)">
                              #{rating}
                         $else
                          <span.card-comment.card-criterion style="opacity: 0.3" title="Not enough votes to show rating">
                            #{svg}
                            \ - #
                        $maybe grade <- mexpertgrade
                          <span.card-comment.card-criterion  title="Expert Grade">
                            <span class="icon icon-lg" style="width:24px; height:24px;">star</span>
                              <p style="display: inline; margin: 0; padding:0; color: #b71c1c;)">
                                #{grade}
                        <div.card-action-btn.pull-right>
                          $if isExpert && hasToBeGraded
                            <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                                style="background: red; color: white !important;"
                                href="@{SubmissionViewerR scpId uId}" target="_blank">
                              <span.icon>visibility
                              Review
                          $else
                            <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                                href="@{SubmissionViewerR scpId uId}" target="_blank">
                              <span.icon>visibility
                              View

          <!-- footer with page numbers -->
          $if pages == 0
            <p>No submissions found with the selected criteria.
          $else
           <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9">
              <div class="card margin-bottom-no">
                <div class="card-main">
                  <div class="card-inner">
                   $forall i <- is
                    $if i == page
                      <p style="margin:2px;padding:0;display:inline">#{i}
                    $else
                      <input
                        type=submit
                        class=pageSelector
                        value=#{i}
                        formaction=@{BrowseProposalsR i}>
      |]
 where
   cOpacity i = 0.5 + fromIntegral i / 198 :: Double



shortLength :: Int
shortLength = 140

maxLines :: Int
maxLines = 3

data ProposalParams = ProposalParams {
      onlyNeedsReview :: Maybe ()
    , onlyByAuthorId  :: Maybe UserId
    -- , onlyByExerciseId  :: Maybe ScenarioProblemId
    }

noProposalParams :: ProposalParams
noProposalParams = ProposalParams {
      onlyNeedsReview = Nothing
    , onlyByAuthorId  = Nothing
    }

proposalsForm :: Html -> MForm Handler (FormResult ProposalParams, Widget)
proposalsForm extra = do
  maybeMe <- lift maybeAuthId
  (onlyNeedsReviewRes, onlyNeedsReviewView) <- mreq (bootstrapSelectFieldList [
                      ("All"::Text,    Nothing)
                    , ("Needs review", Just ())
                    ]) "" Nothing
  (onlyByAuthorIdRes, onlyByAuthorView) <- mreq (bootstrapSelectFieldList [
                      ("All"::Text, Nothing)
                    , ("Only mine", maybeMe)
                    ]) "" Nothing
  let proposalParams = ProposalParams <$> onlyNeedsReviewRes
                                      <*> onlyByAuthorIdRes
  let widget = do
        [whamlet|
          #{extra}
          ^{fvInput onlyNeedsReviewView}
          ^{fvInput onlyByAuthorView}
          <input type=submit value=Filter class="btn btn-default">
        |]
  return (proposalParams, widget)


shortComment :: Text -> Text
shortComment t = dropInitSpace . remNewLines $
  if Text.length t < shortLength
    then t
    else remLong t <> "..."
  where remLong = Text.dropEnd 1
                . Text.dropWhileEnd (\c -> c /= ' ' && c /= '\n' && c /= '\t')
                . Text.take shortLength
        remNewLines = Text.dropWhileEnd (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
                    . Text.unlines
                    . take maxLines
                    . filter (not . Text.null)
                    . map (Text.dropWhile (\c -> c == ' ' || c == '\r' || c == '\t'))
                    . Text.lines
        dropInitSpace = Text.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

generateJoins :: Text
generateJoins = Text.unlines [
   " INNER JOIN \"user\" ON \"user\".id = scenario.author_id"
  ," INNER JOIN ( SELECT scenario.author_id, scenario.task_id, MAX(scenario.last_update) as x, AVG(COALESCE(rating.value, 0)) as score"
  ,"              FROM scenario"
  ,"              LEFT OUTER JOIN rating"
  ,"                           ON rating.author_id = scenario.author_id"
  ,"              GROUP BY scenario.author_id, scenario.task_id"
  ,"              ORDER BY scenario.task_id DESC, score DESC, x DESC"
  ,"            ) t"
  ,"         ON t.task_id = scenario.task_id AND t.author_id = scenario.author_id AND t.x = scenario.last_update"
  ," INNER JOIN problem_criterion ON scenario.task_id = problem_criterion.problem_id"
  ," INNER JOIN criterion ON criterion.id = problem_criterion.criterion_id"
  ," LEFT OUTER JOIN rating"
  ,"         ON scenario.task_id = rating.problem_id AND scenario.author_id = rating.author_id AND criterion.id = rating.criterion_id"
  ," LEFT OUTER JOIN ( SELECT scenario_id, AVG(grade) as expertgrade, COUNT(grade) as nrofexpertgrades"
  ,"              FROM expert_review"
  ,"              GROUP BY scenario_id "
  ,"            ) g"
  ,"         ON scenario.id = g.scenario_id "
  ]

generateWhereClause :: ProposalParams -> ([PersistValue], Text)
generateWhereClause ps =
      if null wheres
      then ([], "")
      else (map fst wheres, "WHERE " ++ intercalate " AND " (map snd wheres))
      where
        wheres = catMaybes [
                   needsReview
                 , byAuthorId
                 ]
        needsReview = onlyNeedsReview ps >>= \_ -> Just (toPersistValue (0::Int), "(rating.value IS NULL AND COALESCE(g.nrofexpertgrades, 0) = ?)")
        byAuthorId  = onlyByAuthorId  ps >>= \a -> Just (toPersistValue a, "scenario.author_id = ?")

-- | get user name, scenario, and ratings
getLastSubmissions :: Int -> ProposalParams -> ReaderT SqlBackend Handler
  [((ScenarioId, ScenarioProblemId, UserId), UTCTime, Text, Text, (Maybe Double, Bool), [(Blaze.Markup, Text, Int)])]
getLastSubmissions page params = getVals <$> rawSql query preparedParams
  where
    preparedParams = whereParams ++ map toPersistValue [pageSize, (max 0 $ page-1)*pageSize]
    (whereParams, whereClause) = generateWhereClause params
    getVal' scId' xxs@(((Single scId, Single _, Single _), Single _, Single _, Single _, Single icon, Single cname, Single rating, (Single _expertgrade, Single _hasToBeGraded)):xs)
        | scId == scId' = first ((Blaze.preEscapedToMarkup (icon :: Text), cname, min 99 $ round (100*rating::Double)) :) $ getVal' scId xs
        | otherwise = ([],xxs)
    getVal' _ [] = ([],[])
    getVals xxs@(( (Single scId, Single scpId, Single uid)
                 , Single lu, Single desc, Single uname, Single _icon, Single _name, Single _rating, (Single expertgrade, Single hasToBeGraded)):_)
                                      = let (g,rest) = getVal' scId xxs
                                        in ((scId, scpId, uid), lu, desc, uname, (expertgrade, hasToBeGraded), g) : getVals rest
    getVals [] = []
    query = Text.unlines
          ["SELECT scenario.id, scenario.task_id, scenario.author_id"
          ,"     , scenario.last_update, scenario.description,\"user\".name"
          ,"     , criterion.icon, criterion.name, COALESCE(rating.value, 0)"
          ,"     , g.expertgrade, (rating.value IS NULL AND COALESCE(g.nrofexpertgrades, 0) = 0)"
          ,"FROM scenario"
          , generateJoins
          , whereClause
          ,"ORDER BY scenario.task_id DESC, t.score DESC, scenario.id DESC, criterion.id ASC"
          ,"LIMIT ? OFFSET ?"
          ,";"
          ]

countUniqueSubmissions :: ProposalParams -> ReaderT SqlBackend Handler Int
countUniqueSubmissions params = getVal <$> rawSql query whereParams
  where
    (whereParams, whereClause) = generateWhereClause params
    getVal (Single c:_)  = c
    getVal [] = 0
    query = Text.unlines
          [ "SELECT count(DISTINCT scenario.author_id)"
          , "FROM scenario"
          , generateJoins
          , whereClause
          , ";"
          ]
