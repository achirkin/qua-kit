-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.EditProposal
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.EditProposal
  ( getEditProposalR
  ) where

import Import
import Database.Persist.Sql (fromSqlKey,toSqlKey)
import Data.Text.Read (decimal)
import qualified Handler.Mooc.Scenario as S
import Model.Session

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
