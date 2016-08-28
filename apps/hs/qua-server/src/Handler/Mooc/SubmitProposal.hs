-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.SubmitProposal
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.SubmitProposal
  ( postSubmitProposalR
  ) where

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)


postSubmitProposalR :: Handler Html
postSubmitProposalR = do
  muser <- maybeAuth
  mgeometry <- lookupPostParam "geometry"
  mpreview  <- lookupPostParam "preview"
  description  <- lookupPostParam "description"
  mcustom_exercise_type <- lookupSession "custom_exercise_type"
  mcustom_exercise_id   <- lookupSession "custom_exercise_id"
  case (,,,,) <$> muser
              <*> mpreview
              <*> mgeometry
              <*> mcustom_exercise_type
              <*> fmap decimal mcustom_exercise_id of
    Nothing -> invalidArgs
      [  "The session was not set up properly, the submission is not saved. "
      <> "You must login via edX exercise page to submit your proposals."]
    Just (_,_,_,_, Left _) -> invalidArgs
      [  "Cannot parse custom_exercise_id. Please, consult the developer."]
    Just ( Entity uId user
         , preview
         , geometry
         , "design"
         , Right i) -> do
      mprevScenario <- runDB $ selectSource
          [ ScenarioAuthorId ==. uId
          , ScenarioTaskId   ==. (toSqlKey $ fst i)
          ]
          [ Desc ScenarioLastUpdate
          ] $$ await
      case mprevScenario of
        Nothing -> notFound
        Just (Entity _ prevScenario) -> do
          t <- liftIO getCurrentTime
          _ <- runDB . insert $ prevScenario
             { scenarioImage = encodeUtf8 preview
             , scenarioDescription = fromMaybe "" description
             , scenarioGeometry = encodeUtf8 geometry
             , scenarioLastUpdate = t
             }
          setMessage . toHtml $ "Thank you, " <> userName user <> ", your design proposal has been saved."
          redirectUltDest MoocHomeR
    Just (_,_,_,_, _) -> invalidArgs
      [  "You can submit proposals only in \"design\" mode, corresponding to design exercises."]

