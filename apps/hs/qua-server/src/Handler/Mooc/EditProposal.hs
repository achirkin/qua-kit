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

getEditProposalR :: Handler Html
getEditProposalR = do
  setUltDest MoocHomeR
  role <- muserRole <$> maybeAuth
  case role of
    UR_STUDENT -> setSession "qua_view_mode" "edit"
    _          -> setSession "qua_view_mode" "full"
  mtscp_id <- lookupSession "custom_exercise_id"
  case decimal <$> mtscp_id of
    Just (Right (i, _)) -> do
      mscId <- S.getScenarioId $ toSqlKey i
      case mscId of
        Nothing -> return ()
        Just scId -> setSession "scenario_id" (pack . show $ fromSqlKey scId)
    _ -> return ()
  redirect HomeR
