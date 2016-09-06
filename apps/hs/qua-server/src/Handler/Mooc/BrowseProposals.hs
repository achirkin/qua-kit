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

getBrowseProposalsR :: Handler Html
getBrowseProposalsR = do
    usersscenarios <- runDB getLastSubmissions
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
              $forall (uname, Entity scId scenario, crits) <- usersscenarios
                <div class="col-lg-4 col-md-6 col-sm-9 col-xs-9 story_cards">
                  <div.card>
                    <aside.card-side.card-side-img.pull-left.card-side-moocimg>
                      <img src="@{ProposalPreviewR scId}" width="200px" height="200px" style="margin-left: -25px;">
                    <div.card-main>
                      <div.card-inner style="margin: 10px 12px;">
                        <p style="margin: 6px 0px; color: #b71c1c;">
                          #{uname}
                            <br>
                          #{show $ utctDay $ scenarioLastUpdate scenario}
                        <p style="margin: 6px 0px; white-space: pre-line; overflow-y: hidden; color: #555;">
                         #{shortComment $ scenarioDescription scenario}
                      <div.card-comment.card-action>
                        $forall (svg, rating) <- crits
                         $if rating > 0
                          <span.card-comment.card-criterion style="opacity: #{cOpacity rating}">
                            #{svg}
                            <p style="display: inline; margin: 0; padding:0; color: rgb(#{156 + rating}, 111, 0)">
                              #{rating}
                         $else
                          <span.card-comment.card-criterion style="opacity: 0.3" title="Not enough votes to show rating">
                            #{svg}
                            \ - #
                        <div.card-action-btn.pull-right>
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect href="@{ViewProposalR scId}" target="_blank">
                            <span.icon>visibility
                            View
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
getLastSubmissions :: ReaderT SqlBackend Handler [(Text, Entity Scenario, [(Blaze.Markup, Int)])]
getLastSubmissions = getVals <$> rawSql query []
  where
    getVal' scId xxs@((a, _, Single c, Single d):xs) | scId == entityKey a = first ((Blaze.preEscapedToMarkup (c :: Text),min 99 $ round (100*d::Double)) :) $ getVal' scId xs
                                                     | otherwise = ([],xxs)
    getVal' _ [] = ([],[])
    getVals xxs@((s,Single t,_,_):_) = let (g,rest) = getVal' (entityKey s) xxs
                                        in (t,s,g) : getVals rest
    getVals [] = []
    query = Text.unlines
          ["SELECT ??,\"user\".name,criterion.icon,COALESCE(rating.value, 0)"
          ,"FROM scenario"
          ,"INNER JOIN \"user\" ON \"user\".id = scenario.author_id"
          ,"INNER JOIN ( SELECT scenario.author_id, scenario.task_id, MAX(scenario.last_update) as x"
          ,"             FROM scenario"
          ,"             GROUP BY scenario.author_id, scenario.task_id ) t"
          ,"        ON t.task_id = scenario.task_id AND t.author_id = scenario.author_id AND t.x = scenario.last_update"
          ,"CROSS JOIN criterion"
          ,""
          ,"LEFT OUTER JOIN rating"
          ,"        ON scenario.task_id = rating.problem_id AND scenario.author_id = rating.author_id AND criterion.id = rating.criterion_id"
          ,""
          ,"ORDER BY scenario.id ASC, criterion.id ASC;"
          ]
