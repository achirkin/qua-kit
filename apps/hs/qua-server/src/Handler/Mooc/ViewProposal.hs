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
import Model.Session

getViewProposalR :: ScenarioId -> Handler Html
getViewProposalR scId = do
  setSafeSession userSessionQuaViewMode "view"
  setSafeSession userSessionScenarioId scId
  redirect HomeR
