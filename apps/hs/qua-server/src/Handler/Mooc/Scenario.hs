{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Scenario
  ( getScenarioR
  , getScenarioProblemR
  , getScenarioId
  , getScenarioLink
  ) where

import Import
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
  msc_id <- lift $ getsSafeSession userSessionScenarioId
  case msc_id of
    Just sc_id -> do
      scale <- getScScale sc_id
      return (ScenarioR sc_id, scale)
    Nothing -> do
      userId <- MaybeT maybeAuthId
      mscp_id <- lift $ getsSafeSession userSessionCustomExerciseId
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
          scale <- getScScale scId
          return (ScenarioR scId, scale)
        Nothing -> do
          scp_id <- MaybeT $ return mscp_id
          scale <- fmap scenarioProblemScale . MaybeT . runDB $ get scp_id
          return (ScenarioProblemR scp_id, scale)
  where
    getScScale scId = do
      sc <- MaybeT . runDB $ get scId
      scp <- MaybeT . runDB $ get (scenarioTaskId sc)
      return $ scenarioProblemScale scp



getScenarioR :: ScenarioId -> Handler Text
getScenarioR scId = maybe "{}" (decodeUtf8 . scenarioGeometry) <$> runDB (get scId)


getScenarioProblemR :: ScenarioProblemId -> Handler Text
getScenarioProblemR scpId = maybe "{}" (decodeUtf8 . scenarioProblemGeometry) <$> runDB (get scpId)
