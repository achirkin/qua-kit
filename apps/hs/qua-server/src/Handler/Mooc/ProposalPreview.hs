{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ProposalPreview
  ( getProposalPreviewR
  ) where

import Import

getProposalPreviewR :: ExerciseId -> UserId -> Handler TypedContent
getProposalPreviewR exId uId = do
    sc <- runDB $ do
      Entity _ cSc <- getBy404 $ SubmissionOf uId exId
      get404 $ currentScenarioHistoryScenarioId cSc
    neverExpires
    setEtag $ tshow $ scenarioLastUpdate sc
    addHeader "Content-Disposition" "inline"
    sendResponse ("image/png" :: ByteString, toContent $ scenarioImage sc)
