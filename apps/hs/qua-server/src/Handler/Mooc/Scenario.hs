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
  , getScenarioProblemR
  , getScenarioId
  , getScenarioLink
  ) where

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)
import Control.Monad.Trans.Maybe

getScenarioId :: ScenarioProblemId -> Handler (Maybe ScenarioId)
getScenarioId scpId = runMaybeT $ do
    userId <- MaybeT maybeAuthId
    fmap entityKey . MaybeT . runDB $ selectSource
                      [ ScenarioAuthorId ==. userId
                      , ScenarioTaskId   ==. scpId
                      ]
                      [ Desc ScenarioLastUpdate
                      ] $$ await



getScenarioLink :: Handler (Maybe (Route App, Double))
getScenarioLink = runMaybeT $ do
  msc_id <- lift $ lookupSession "scenario_id" >>= \x -> case decimal <$> x of
                        Just (Right (i,_)) -> return . Just $ toSqlKey i
                        _ -> return Nothing
  case msc_id of
    Just sc_id -> do
      scale <- fmap scenarioScale . MaybeT . runDB $ get sc_id
      return (ScenarioR sc_id, scale)
    Nothing -> do
      userId <- MaybeT maybeAuthId
      mscp_id <- lift $ lookupSession "custom_exercise_id" >>= \x -> case decimal <$> x of
                            Just (Right (i,_)) -> return . Just $ toSqlKey i
                            _ -> return Nothing
      mScenarioId <- fmap (fmap entityKey) . lift . runDB $ selectSource
                        (case mscp_id of
                           Just i  -> [ ScenarioAuthorId ==. userId
                                      , ScenarioTaskId   ==. i
                                      ]
                           Nothing -> [ScenarioAuthorId ==. userId]
                        )
                        [ Desc ScenarioLastUpdate
                        ] $$ await
      case mScenarioId of
        Just scId -> do
          scale <- fmap scenarioScale . MaybeT . runDB $ get scId
          return (ScenarioR scId, scale)
        Nothing -> do
          scp_id <- MaybeT $ return mscp_id
          scale <- fmap scenarioProblemScale . MaybeT . runDB $ get scp_id
          return (ScenarioProblemR scp_id, scale)



getScenarioR :: ScenarioId -> Handler Text
getScenarioR scId = maybe "{}" (decodeUtf8 . scenarioGeometry) <$> runDB (get scId)


getScenarioProblemR :: ScenarioProblemId -> Handler Text
getScenarioProblemR scpId = maybe "{}" (decodeUtf8 . scenarioProblemGeometry) <$> runDB (get scpId)
