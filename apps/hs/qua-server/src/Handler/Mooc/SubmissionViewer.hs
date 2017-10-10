{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.SubmissionViewer
  ( getSubmissionViewerR
  ) where

import Import

import qualified Data.Conduit.List as CL

import Handler.Home.LoadingSplash
import Handler.Home.PopupHelp
import Handler.Home.UIButtons
import Handler.Home.PanelServices
import Handler.Home.PanelGeometry
import Handler.Home.PanelInfo
import Handler.Home.PopupEdxGuide
import Handler.Home.LuciConnect
import Handler.Mooc.ExpertReview

getSubmissionViewerR ::  ScenarioProblemId -> UserId -> Handler Html
getSubmissionViewerR scpId authorId = do

  scIds <- runDB $ selectKeys [ScenarioTaskId ==. scpId, ScenarioAuthorId ==. authorId]
                              [Desc ScenarioLastUpdate, LimitTo 1] $$ CL.head

  case scIds of
    Just scId -> redirect $ SubmissionR scId
    Nothing -> do
      setMessage "Design submission your are looking for does not exist."
      redirect MoocHomeR
