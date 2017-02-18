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
import qualified Data.Text as Text
import qualified Text.Blaze as Blaze
import Database.Persist.Sql (rawSql, Single (..))


pageSize :: Int
pageSize = 80

getBrowseProposalsR :: Int -> Handler Html
getBrowseProposalsR page = do
    usersscenarios <- runDB $ getLastSubmissions page
    pages <- negate . (`div` pageSize) . negate <$> runDB countUniqueSubmissions
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
        |]
      toWidgetBody $
        [hamlet|
          <div class="ui-card-wrap">
            <div class="row">
              $forall ((scId, scpId, uId), lu, desc, uname, crits) <- usersscenarios
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
                        <div.card-action-btn.pull-right>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect href="@{SubmissionViewerR scpId uId}" target="_blank">
                            <span.icon>visibility
                            View

          <!-- footer with page numbers -->
          $if pages > 1
           <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9">
              <div class="card margin-bottom-no">
                <div class="card-main">
                  <div class="card-inner">
                   $forall i <- is
                    $if i == page
                      <p style="margin:2px;padding:0;display:inline">#{i}
                    $else
                      <a style="margin:2px;padding:0;display:inline" href="@{BrowseProposalsR i}">#{i}
        |]
 where
   cOpacity i = 0.5 + fromIntegral i / 198 :: Double



shortLength :: Int
shortLength = 140

maxLines :: Int
maxLines = 3

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

-- | get user name, scenario, and ratings
getLastSubmissions :: Int -> ReaderT SqlBackend Handler
  [((ScenarioId, ScenarioProblemId, UserId), UTCTime, Text, Text, [(Blaze.Markup, Text, Int)])]
getLastSubmissions page = getVals <$> rawSql query [toPersistValue pageSize, toPersistValue $ (max 0 (page-1))*pageSize]
  where
    getVal' scId' xxs@(((Single scId, Single _, Single _), Single _, Single _, Single _, Single icon, Single cname, Single rating):xs)
        | scId == scId' = first ((Blaze.preEscapedToMarkup (icon :: Text), cname, min 99 $ round (100*rating::Double)) :) $ getVal' scId xs
        | otherwise = ([],xxs)
    getVal' _ [] = ([],[])
    getVals xxs@(( (Single scId, Single scpId, Single uid)
                 , Single lu, Single desc, Single uname, Single _icon, Single _name, Single _rating):_)
                                      = let (g,rest) = getVal' scId xxs
                                        in ((scId, scpId, uid), lu, desc, uname,g) : getVals rest
    getVals [] = []
    query = Text.unlines
          ["SELECT scenario.id, scenario.task_id, scenario.author_id"
          ,"     , scenario.last_update, scenario.description,\"user\".name"
          ,"     , criterion.icon, criterion.name,COALESCE(rating.value, 0)"
          ,"FROM scenario"
          ,"INNER JOIN \"user\" ON \"user\".id = scenario.author_id"
          ,"INNER JOIN ( SELECT scenario.author_id, scenario.task_id, MAX(scenario.last_update) as x, AVG(COALESCE(rating.value, 0)) as score"
          ,"             FROM scenario"
          ,"             LEFT OUTER JOIN rating"
          ,"                          ON rating.author_id = scenario.author_id"
          ,"             GROUP BY scenario.author_id, scenario.task_id"
          ,"             ORDER BY scenario.task_id DESC, score DESC, x DESC"
          ,"             LIMIT ? OFFSET ?) t"
          ,"        ON t.task_id = scenario.task_id AND t.author_id = scenario.author_id AND t.x = scenario.last_update"
          ,"INNER JOIN problem_criterion ON scenario.task_id = problem_criterion.problem_id"
          ,"INNER JOIN criterion ON criterion.id = problem_criterion.criterion_id"
          ,""
          ,"LEFT OUTER JOIN rating"
          ,"        ON scenario.task_id = rating.problem_id AND scenario.author_id = rating.author_id AND criterion.id = rating.criterion_id"
          ,""
          ,"ORDER BY scenario.task_id DESC, t.score DESC, scenario.id DESC, criterion.id ASC;"
          ]

countUniqueSubmissions :: ReaderT SqlBackend Handler Int
countUniqueSubmissions= getVal <$> rawSql query []
  where
    getVal (Single c:_)  = c
    getVal [] = 0
    query = Text.unlines
          ["SELECT count(DISTINCT scenario.author_id) FROM scenario;"
          ]
