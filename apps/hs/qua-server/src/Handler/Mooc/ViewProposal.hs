{-# OPTIONS_HADDOCK hide, prune #-}
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
