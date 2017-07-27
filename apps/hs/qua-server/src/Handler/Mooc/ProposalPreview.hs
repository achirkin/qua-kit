{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ProposalPreview
  ( getProposalPreviewR
  ) where

import Import

getProposalPreviewR :: ScenarioId -> Handler TypedContent
getProposalPreviewR ident = do
    scenario <- runDB $ get404 ident
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ scenarioImage scenario)
