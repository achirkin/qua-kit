{-|
This module contains:
1. HTML Handlers for displaying or redirecting to a qua-view
2. JSON Handlers that form the REST API for Submissions (aka Scenarios)
   see e.g. http://www.restapitutorial.com/lessons/httpmethods.html
-}

module Handler.Submissions
    ( getQuaViewEditorR
    , getSubmissionR, putSubmissionR
    , getSubmissionInfoR
    , getSubmissionGeometryR
    , getRedirectToQuaViewEditR
    ) where

import Application.Edx (sendEdxGrade)
import Application.Grading (updateRatingOnSubmission)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Base64 as BSB (decodeLenient)
import Handler.Mooc.User (maybeFetchExerciseId)
import Import
import Import.Util
import qualified QuaTypes.Submission as QtS
import qualified Data.Text as Text
import Database.Persist.Sql (fromSqlKey)

-- | Serve GUI for free-form editor
getQuaViewEditorR :: Handler Html
getQuaViewEditorR = minimalLayout $ do
  toWidgetHead [hamlet|
    <meta property="qua-view:settingsUrl" content="@{QuaViewEditorSettingsR}" /> |]
  quaW

-- | Serve GUI to edit an exercise submission
getSubmissionR :: ExerciseId -> UserId -> Handler Html
getSubmissionR exId uId = do
  authorName <- maybe "anonymous" userName <$> runDB (get uId)
  minimalLayout $ do
    toWidgetHead [hamlet|
      <meta property="qua-view:settingsUrl" content="@{QuaViewExerciseSettingsR exId uId}" />
      <meta property="og:url"         content="@{SubmissionR exId uId}" />
      <meta property="og:type"        content="website" />
      <meta property="og:title"       content="Qua-kit: #{authorName}'s design" />
      <meta property="og:description" content="#{authorName} made a design proposal on qua-kit website. Check it out!" />
      <meta property="og:image"       content="@{ProposalPreviewR exId uId}" />
    |]
    quaW



-- | Redirect to the user's most recent submission,
--   or to new submission if no current submission exists
getRedirectToQuaViewEditR :: Handler Html
getRedirectToQuaViewEditR = do
  mUsrId <- maybeAuthId
  -- get latest enrolled exercise
  mExId  <- (fmap join . mapM maybeFetchExerciseId) mUsrId
  -- either return exercise link, or a pure editor
  redirect . fromMaybe QuaViewEditorR $ SubmissionR <$> mExId <*> mUsrId


-- | Serve GeoJSON for saved submission.
--   If userId == authorId, this function guarantees to return scenario
--   (new or current).
--   Otherwise, it may return current submsission or fail with 404.
--
--   This hanlder also sets etag and never expire headers.
--   It uses scenario update timestamp as an etag value.
getSubmissionGeometryR :: ExerciseId -> UserId -> Handler Text
getSubmissionGeometryR exId authorId = do
  mcsc <- runDB $ getBy $ SubmissionOf authorId exId
  case mcsc of
    Just (Entity _ csc) -> do
      neverExpires
      setEtag $ tshow $ currentScenarioLastUpdate csc
      maybe "{}" (decodeUtf8 . scenarioGeometry) <$>
        runDB (get $ currentScenarioHistoryScenarioId csc)
    Nothing -> do
      userId <- requireAuthId
      if userId == authorId
      then do
        neverExpires
        -- some random thing here, just different from show date
        setEtag "JndlubDludsbSKludoSjjbSafb17Ud"
        maybe "{}" (decodeUtf8 . exerciseGeometry) <$> runDB (get exId)
      else do
        alreadyExpired
        notFound



-- | Update or create new submission
putSubmissionR :: ExerciseId -> UserId -> Handler Value
putSubmissionR exId authorId = runJSONExceptT $ do
    -- get all provided information
    submissionPost <- requireJsonBody
    time   <- liftIO getCurrentTime
    userId <- maybeE "You must login to post." maybeAuthId
    when (userId /= authorId) $
      throwE "You cannot update someone else's submission."
    let (desc, geo, img) = unbundleSubPost submissionPost

    -- check if this is a new submission or an updated one
    mCurSubmission <- lift . runDB . runMaybeT $ do
      cSc <- MaybeT . getBy $ SubmissionOf authorId exId
      sc <- MaybeT . get . currentScenarioHistoryScenarioId $ entityVal cSc
      return (entityKey cSc, sc)

    case mCurSubmission of

      -- if this is an updated version
      Just (cScId, prevSc) -> lift . runDB $ do
        newScId <- insert $ prevSc { scenarioImage       = img
                                   , scenarioDescription = desc
                                   , scenarioGeometry    = geo
                                   , scenarioLastUpdate  = time
                                   }
        update cScId [ CurrentScenarioHistoryScenarioId =. newScId
                     , CurrentScenarioDescription =. desc
                     , CurrentScenarioLastUpdate =. time
                     ]
        -- update grading/ratings
        updateRatingOnSubmission newScId

      -- if this is a new submission
      Nothing -> lift $ do
        medxResId <- getsSafeSession userSessionEdxResourceId
        runDB $ do
          medxGrading <- case medxResId of
            Nothing -> return Nothing
            Just ri -> fmap (fmap entityKey) . getBy $ EdxGradeKeys ri userId
          let sc = Scenario userId exId img geo desc time
          newScId  <- insert sc
          insert_ $ CurrentScenario newScId
                            (scenarioAuthorId      sc)
                            (scenarioExerciseId    sc)
                            (scenarioDescription   sc)
                            medxGrading
                            Nothing
                            (scenarioLastUpdate    sc)
          -- update grading/ratings
          updateRatingOnSubmission newScId

        -- send an initial grade to edX if necessary
        forM_ medxResId $ \edxResId -> do
          ye <- getYesod
          sendEdxGrade (appSettings ye) userId edxResId 0.6
            $ Just "Automatic grade upon design submission."

    mOnSubmitMsg <- lift . runDB . runMaybeT $ do
      u <- MaybeT $ get userId
      e <- MaybeT $ get exId
      return $ interpolateSubmitMsg exId userId (userName u) (exerciseOnSubmitMsg e)

    returnJson $ QtS.SubmitResponse $ fromMaybe "Thank you!" mOnSubmitMsg


interpolateSubmitMsg :: ExerciseId -- ^ exerciseId
                     -> UserId -- ^ userId
                     -> Text -- ^ userName
                     -> Text -- ^ template
                     -> Text
interpolateSubmitMsg eId uId uName
  = Text.replace "${exerciseId}" (tshow $ fromSqlKey eId)
  . Text.replace "${userId}"     (tshow $ fromSqlKey uId)
  . Text.replace "${userName}"    uName



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


getSubmissionInfoR :: ExerciseId -> UserId -> Handler Value
getSubmissionInfoR exId authorId = do
  Entity _ csc <- runDB $ getBy404 $ SubmissionOf authorId exId
  neverExpires
  setEtag $ tshow $ currentScenarioLastUpdate csc
  user <- runDB $ get404 authorId
  return $ toJSON
    QtS.SubmissionInfo
     { subInfoDescription = currentScenarioDescription csc
     , subInfoUserName    = userName user
     , subInfoTime        = currentScenarioLastUpdate csc
     }
