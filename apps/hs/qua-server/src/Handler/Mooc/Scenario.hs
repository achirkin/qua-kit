-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.Scenario
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.Scenario
  ( getScenarioR
  , getScenarioId
  ) where

import Import
import Database.Persist.Sql (fromSqlKey)
import Control.Monad.Trans.Maybe

getScenarioId :: ScenarioProblemId -> Handler (Maybe ScenarioId)
getScenarioId scpId = runMaybeT $ do
    userId <- MaybeT maybeAuthId
    mScenarioId <- fmap (fmap entityKey) . lift . runDB $ selectSource
                      [ ScenarioAuthorId ==. userId
                      , ScenarioTaskId   ==. scpId
                      ]
                      [ Desc ScenarioLastUpdate
                      ] $$ await
    case mScenarioId of
      Just scId -> return scId
      Nothing -> do
        resource_link_id        <- MaybeT $ lookupSession "resource_link_id"
        lis_outcome_service_url <- lift $ lookupSession "lis_outcome_service_url"
        lis_result_sourcedid    <- lift $ lookupSession "lis_result_sourcedid"
        (scproblem, edxres) <- MaybeT . runDB $ (\mx my -> (,) <$> mx <*> my)
                                             <$> get scpId
                                             <*> getBy (EdxResLinkId resource_link_id)
        t <- liftIO getCurrentTime
        scId <- lift . runDB . insert $ Scenario userId scpId
             (scenarioProblemImage scproblem)
             (scenarioProblemGeometry scproblem)
             (scenarioProblemDescription scproblem)
             (scenarioProblemScale scproblem)
             (entityKey edxres)
             lis_outcome_service_url
             lis_result_sourcedid
             t
        lift $ setSession "scenario_id" (pack . show $ fromSqlKey scId)
        return scId

getScenarioR :: ScenarioId -> Handler Text
getScenarioR scId = maybe "" (decodeUtf8 . scenarioGeometry) <$> runDB (get scId)
