{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ProposalPreview
  ( getProposalPreviewR
  ) where

import Import

getProposalPreviewR :: CurrentScenarioId -> Handler TypedContent
getProposalPreviewR cScId = do
    cSc <- runDB $ get404 cScId
    sc  <- runDB $ get404 $ currentScenarioHistoryScenarioId cSc
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ scenarioImage sc)
