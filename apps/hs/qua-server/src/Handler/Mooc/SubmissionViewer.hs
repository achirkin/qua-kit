{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.SubmissionViewer
  ( getSubmissionViewerR
  , postSubmissionViewerR
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
import Handler.Mooc.Comment
import Handler.Mooc.ExpertReview

getSubmissionViewerR ::  ScenarioProblemId -> UserId -> Handler Html
getSubmissionViewerR = renderQuaView

postSubmissionViewerR :: ScenarioProblemId -> UserId -> Handler Html
postSubmissionViewerR = renderQuaView


renderQuaView :: ScenarioProblemId -> UserId -> Handler Html
renderQuaView scpId authorId = do

  muser <- maybeAuth
  scIds <- runDB $ selectKeys [ScenarioTaskId ==. scpId, ScenarioAuthorId ==. authorId]
                              [Desc ScenarioLastUpdate, LimitTo 1] $$ CL.head
  authorName <- runDB (get authorId) >>= \mu -> case mu of
    Nothing -> return "anonymous"
    Just u  -> return $ userName u

  scId <- case scIds of
    Just x -> return x
    Nothing -> do
      setMessage "Design submission your are looking for does not exist."
      redirect MoocHomeR

  setSafeSession userSessionScenarioId scId

  let goEdit = case muser of
        (Just (Entity userId _)) -> authorId == userId
        _ -> False
  when goEdit $ redirect EditProposalR

  let urole = UR_STUDENT
      qua_view_mode = "view" :: Text
      showFull = False
      showHelp = False
  setSafeSession userSessionQuaViewMode qua_view_mode
  commentsW <- viewComments scId

  writeExpertReviewW <- case muser of
    (Just (Entity userId user))
      | userRole user == UR_EXPERT -> writeExpertReview userId scId
      | otherwise -> return mempty
    _ -> return mempty

  viewExpertReviewsW <- viewExpertReviews scId

  -- connecting form + conteiners for optional content
  (lcConnectedClass, lcDisconnectedClass, luciConnectForm) <- luciConnectPane
  (popupScenarioList, luciScenariosPane) <- luciScenarios
  (uiButtonsGUI, uiButtonsSubmitPopup) <- uiButtons $ Just (scpId, authorId)
  minimalLayout $ do

    -- add qua-view dependencies and OpenGraph info about submission.
    toWidgetHead
      [hamlet|
        <meta property="og:url"         content="@{SubmissionViewerR scpId authorId}" />
        <meta property="og:type"        content="website" />
        <meta property="og:title"       content="Qua-kit: #{authorName}'s design" />
        <meta property="og:description" content="#{authorName} made a design proposal on qua-kit website. Check it out!" />
        <meta property="og:image"       content="@{ProposalPreviewR scId}" />
        <script src="@{StaticR js_numeric_min_js}" type="text/javascript">
        <script src="@{StaticR js_qua_view_js}"    type="text/javascript">
      |]

    -- write a function to retrieve settings from qua-server to qua-view
    toWidgetHead
      [julius|
        window['getQuaViewSettings'] = function getQuaViewSettings(f){
          var qReq = new XMLHttpRequest();
          qReq.onload = function (e) {
            if (qReq.readyState === 4) {
              if (qReq.status === 200) {
                f(JSON.parse(qReq.responseText));
              } else {f({});}
            }
          };
          qReq.onerror = function (e) {f({});};
          qReq.open("GET", "@{QuaViewSettingsR}", true);
          qReq.send();
        };
      |]

    setTitle "qua-kit"

    -- render all html
    $(widgetFile "qua-view")
