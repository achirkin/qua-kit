{-|
This module contains:
1. HTML Handlers for displaying or redirecting to a qua-view
2. JSON Handlers that form the REST API for Submissions (aka Scenarios)
   see e.g. http://www.restapitutorial.com/lessons/httpmethods.html
-}

module Handler.Submissions
    ( getNewSubmissionR
    , getNewSubmissionForExerciseR
    , getRedirectToCurrentScenarioR
    , getSubmissionR
    , getSubmissionGeometryR
    , postSubmissionsR
    , putSubmissionR
    ) where

import Application.Edx (sendEdxGrade)
import Application.Grading (updateRatingOnSubmission)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Base64 as BSB (decodeLenient)
import Handler.Mooc.User (maybeFetchExerciseId)
import Import
import Import.Util
import qualified QuaTypes.Submission as QtS

-- | Serve GUI for free-form editor
getNewSubmissionR :: Handler Html
getNewSubmissionR = minimalLayout $ do
  toWidgetHead [hamlet|
    <meta property="qua-view:settingsUrl" content="@{QuaViewSettingsNewR}" /> |]
  quaW

-- | Serve GUI to edit or create the submission for a particular exercise
getNewSubmissionForExerciseR :: ScenarioProblemId -> Handler Html
getNewSubmissionForExerciseR exId = minimalLayout $ do
  toWidgetHead [hamlet|
    <meta property="qua-view:settingsUrl" content="@{QuaViewSettingsFromExIdR exId}" /> |]
  quaW

-- | Serve GUI to edit a particular submission
getSubmissionR :: ScenarioId -> Handler Html
getSubmissionR scId = minimalLayout $ do
  toWidgetHead [hamlet|
    <meta property="qua-view:settingsUrl" content="@{QuaViewSettingsFromScIdR scId}" /> |]
  quaW

-- | Redirect to the user's most recent submission,
--   or to new submission if no current submission exists
getRedirectToCurrentScenarioR :: Handler Html
getRedirectToCurrentScenarioR = do
  mUsrId <- maybeAuthId
  mExId  <- (fmap join . mapM maybeFetchExerciseId) mUsrId
  let lastUpdatedSc usrId = selectFirst [CurrentScenarioAuthorId ==. usrId]
                                        [Desc CurrentScenarioLastUpdate]
  mcScEnt <- runDB $
    case (mUsrId, mExId) of
      (Just usrId, Just exId) -> do
        mcScEnt' <- selectFirst [ CurrentScenarioAuthorId ==. usrId
                                , CurrentScenarioTaskId ==. exId] []
        if isJust mcScEnt'
          then return mcScEnt'
          else lastUpdatedSc usrId
      (Just usrId, Nothing) -> lastUpdatedSc usrId
      _ -> return Nothing
  let mScId = currentScenarioHistoryScenarioId <$> entityVal <$> mcScEnt

  redirect $ case (mScId, mExId) of
    (Just scId, _)       -> SubmissionR scId
    (Nothing, Just exId) -> NewSubmissionForExerciseR exId
    _                    -> NewSubmissionR

-- | Serve GeoJSON for saved submission or empty JSON object otherwise
getSubmissionGeometryR :: ScenarioId -> Handler Text
getSubmissionGeometryR scId =
  maybe "{}" (decodeUtf8 . scenarioGeometry) <$> runDB (get scId)

-- | Make new submission
postSubmissionsR :: ScenarioProblemId -> Handler Value
postSubmissionsR scpId = runJSONExceptT $ do
  submissionPost <- requireJsonBody
  time   <- liftIO getCurrentTime
  userId <- maybeE "You must login to post." maybeAuthId
  let (desc, geo, img) = unbundleSubPost submissionPost
  medxResId <- lift $ getsSafeSession userSessionEdxResourceId
  scId <- ExceptT $ runDB $ runExceptT $ do
    medxGrading <- case medxResId of
                     Nothing -> return Nothing
                     Just ri -> lift . getBy $ EdxGradeKeys ri userId
    let sc = Scenario
               userId
               scpId
               img
               geo
               desc
               time
    scId <- lift $ insert sc
    void . lift . insert $ CurrentScenario scId
                              (scenarioAuthorId      sc)
                              (scenarioTaskId        sc)
                              (scenarioDescription   sc)
                              (entityKey <$> medxGrading)
                              Nothing
                              (scenarioLastUpdate    sc)
    return scId

  -- set grading/ratings
  lift $ runDB $ updateRatingOnSubmission scId
  lift $ setGrade userId
  return scId
  where
    setGrade userId = do
      ye <- getYesod
      medxResId <- getsSafeSession userSessionEdxResourceId
      case medxResId of
        Just edxResId -> sendEdxGrade (appSettings ye) userId edxResId 0.6
                           $ Just "Automatic grade upon design submission."
        Nothing -> return ()

-- | Update submission
putSubmissionR :: ScenarioId -> Handler Value
putSubmissionR prevScId = runJSONExceptT $ do
  submissionPost <- requireJsonBody
  time   <- liftIO getCurrentTime
  userId <- maybeE "You must login to post." maybeAuthId
  prevSc <- lift $ runDB $ get404 prevScId
  when (userId /= scenarioAuthorId prevSc) $
    throwE "You cannot update someone else's submission."
  let (desc, geo, img) = unbundleSubPost submissionPost
  ExceptT $ runDB $ runExceptT $ do
    newScId <- lift . insert $ prevSc {
                                   scenarioImage       = img
                                 , scenarioDescription = desc
                                 , scenarioGeometry    = geo
                                 , scenarioLastUpdate  = time
                                 }
    success <- lift $ do
      mcs <- getBy $ LatestSubmissionId prevScId
      case mcs of
        Nothing -> return False
        Just (Entity csId _) -> do
          update csId [ CurrentScenarioHistoryScenarioId =. newScId
                      , CurrentScenarioDescription =. desc
                      , CurrentScenarioLastUpdate =. time
                      ]
          -- update grading/ratings
          updateRatingOnSubmission newScId
          return True
    return success

unbundleSubPost :: QtS.SubmissionPost -> (Text, ByteString, ByteString)
unbundleSubPost subPost = (desc, geo, img)
  where
    desc = QtS.subPostDescription subPost
    geo  = encodeUtf8 $ QtS.subPostGeometry subPost
    img  = BSB.decodeLenient $ encodeUtf8 $ drop 1 $ dropWhile (',' /=) $
             QtS.subPostPreviewImage subPost

quaW :: Widget
quaW = do
  setTitle "qua-kit"

  toWidgetHead
    [hamlet|
      <link href=@{StaticR css_qua_view_css} rel="stylesheet" type="text/css">
    |]

  [whamlet|
    <!-- A special element for showing loading splash very early -->
    <div .qua-view-loading-busy #qua-view-loading-div>
      <div #qua-view-loading-background>
      <svg #qua-view-loading-splash fill="none" version="1.1" viewBox="0 0 72 72"
          xmlns="http://www.w3.org/2000/svg">
        <circle #qua-view-loading-splash-red-circle cx="36" cy="36" r="28"
          stroke="#FF5722" stroke-dasharray="10, 5, 50, 40, 30.929188601, 40"
          stroke-opacity="1" stroke-width="16">
        <circle #qua-view-loading-splash-grey-circle cx="36" cy="36" r="28"
          stroke="#BF360C" stroke-dasharray="38, 14, 8, 14, 65.929188601, 14, 8, 14"
          stroke-opacity=".2" stroke-width="8">

    <!-- WebGL drawing canvas -->
    <canvas #qua-view-webgl-canvas>

    <!-- All dynamic widgets from qua-view.js go inside here -->
    <div #qua-view-widgets>

    <!-- All dynamic modals from qua-view.js go inside here -->
    <div #qua-view-modals>

    <!-- Qua-view generated code -->
    <script src=@{StaticR js_qua_view_js} async type="text/javascript">
  |]
