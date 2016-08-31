-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.ViewProposal
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.ViewProposal
  ( getViewProposalR
  ) where


import Import
import Database.Persist.Sql (fromSqlKey)

getViewProposalR :: ScenarioId -> Handler Html
getViewProposalR scId = do
  setSession "qua_view_mode" "view"
  setSession "scenario_id" (pack . show $ fromSqlKey scId)
  redirect HomeR
