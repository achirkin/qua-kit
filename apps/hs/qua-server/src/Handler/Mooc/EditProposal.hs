{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.EditProposal
  ( getEditProposalR
  ) where

import Import
import qualified Handler.Mooc.Scenario as S

getEditProposalR :: Handler Html
getEditProposalR = do
  setUltDest MoocHomeR
  role <- muserRole <$> maybeAuth
  setSafeSession userSessionQuaViewMode $
    case role of
      UR_STUDENT -> "edit"
      _          -> "full"
  let safeOrDestroy (Just scId) = setSafeSession    userSessionScenarioId scId
      safeOrDestroy Nothing     = deleteSafeSession userSessionScenarioId
  mtscp_id <- getsSafeSession userSessionCustomExerciseId
  mscId <- case mtscp_id of
             Just i  -> S.getScenarioId i
             Nothing -> tryNewest
  safeOrDestroy mscId
  redirect HomeR

-- this is a temporary hack:
-- if the exerciseId is not in a cookie and cannot be retrieved from the db
-- we try to guess it
tryNewest :: Handler (Maybe ScenarioId)
tryNewest = do
  muserId <- maybeAuthId
  case muserId of
    Just userId -> do
      scs <- runDB $ selectList [CurrentScenarioAuthorId ==. userId]
                       [Desc CurrentScenarioLastUpdate, LimitTo 1]
      case scs of
        (Entity _ sc):_ -> do
          setSafeSession userSessionCustomExerciseId $ currentScenarioTaskId sc
          return $ Just $ currentScenarioHistoryScenarioId sc
        [] -> do
          -- We cannot error out here since the code flow passes tough here
          -- if we are a new user opening the viewer the very first time.
          -- Also, there are some students in the db that don't have any submissions
          -- so we move out of edit mode (since there's no submission to edit),
          -- back into full mode.
          setSafeSession userSessionQuaViewMode "full"
          return Nothing
    Nothing -> return Nothing
