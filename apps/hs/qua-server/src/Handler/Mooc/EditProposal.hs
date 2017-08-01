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
  case role of
    UR_STUDENT -> setSafeSession userSessionQuaViewMode "edit"
    _          -> setSafeSession userSessionQuaViewMode "full"
  mtscp_id <- getsSafeSession userSessionCustomExerciseId
  case mtscp_id of
    Just i -> do
      mscId <- S.getScenarioId i
      case mscId of
        Nothing -> deleteSafeSession userSessionScenarioId
        Just scId -> setSafeSession userSessionScenarioId scId
    _ -> deleteSafeSession userSessionScenarioId
  redirect HomeR
