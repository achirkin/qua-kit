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
--import qualified Data.Conduit.List as Conduit
import Database.Persist.Sql (rawSql)

getBrowseProposalsR :: Handler Html
getBrowseProposalsR = do
    usersscenarios <- runDB getLastSubmissions
    fullLayout Nothing "Qua-kit student designs" $ do
      setTitle "Qua-kit student designs"
      toWidgetHead $
        [cassius|
          /* transparent button on top of card, so we can click on it */
          .hoverbutton
            padding: 0 16px 24px 16px
            top: 0px
            left: 0px
            position: absolute
            width: 100%
            height: 100%
            a
              width: 100%
              height: 100%
        |]
      toWidgetBody $
        [hamlet|
          <div class="ui-card-wrap">
            <div class="row">
              $forall (Entity _ u, Entity scId scenario) <- usersscenarios
                <div class="col-lg-4 col-md-6 col-sm-9 col-xs-9 story_cards">
                  <div class="card">
                    <aside class="card-side card-side-img pull-left card-side-moocimg">
                      <img src="@{ProposalPreviewR scId}" width="200px" height="200px" style="margin-left: -25px;">
                    <div class="card-main">
                      <div class="card-inner" style="margin: 10px 12px;">
                        <p style="margin: 6px 0px;">
                          #{userName u}
                            <br>
                          #{show $ utctDay $ scenarioLastUpdate scenario}
                        <p style="margin: 6px 0px; white-space: pre-line; overflow-y: hidden; color: #555;">
                         #{shortComment $ scenarioDescription scenario}
                  <div class="hoverbutton">
                    <a class="btn btn-flat waves-attach call-modal" href="@{ViewProposalR scId}" target="_blank">
        |]



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


getLastSubmissions :: ReaderT SqlBackend Handler [(Entity User, Entity Scenario)]
getLastSubmissions = rawSql query []
  where
    query = Text.unlines
          ["SELECT ??, ??"
          ,"FROM scenario"
          ,"INNER JOIN ( SELECT scenario.author_id, scenario.task_id, MAX(scenario.last_update) as x"
          ,"             FROM scenario"
          ,"             GROUP BY scenario.author_id, scenario.task_id ) t"
          ,"        ON t.task_id = scenario.task_id AND t.author_id = scenario.author_id AND t.x = scenario.last_update"
          ,"INNER JOIN \"user\" ON \"user\".id = scenario.author_id;"
          ]
