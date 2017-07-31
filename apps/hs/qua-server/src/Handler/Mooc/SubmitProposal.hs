{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.SubmitProposal
  ( postSubmitProposalR
  ) where

import           Control.Monad.Trans.Maybe
import qualified Data.Text                 as Text (pack)
import           Import

import qualified Data.ByteString.Base64    as BSB (decodeLenient)
import           Application.Edx

-- | Submit a design proposal
--   session params:
--     * qua_view_mode=edit -- means a user works on a scenario in edit mode => can save something
--     * scenario_id -- id of previous submission
--   post params:
--     * geometry    -- geoJSON, obligatory
--     * preview     -- image, obligatory
--     * description -- text, optional

postSubmitProposalR :: Handler Html
postSubmitProposalR = do
    Entity uId user <- requireAuth
    geometry <- encodeUtf8
        <$> requirePostParam "geometry" "Geometry input is not found!"
    preview  <- BSB.decodeLenient . encodeUtf8 . drop 1 . dropWhile (',' /=)
        <$> requirePostParam "preview" "Preview input is not found!"
    description  <- fromMaybe ""
        <$> lookupPostParam "description"

    qua_view_mode <- requireSession "qua_view_mode" "You must be in an 'edit' mode to save scenario."
    when (qua_view_mode /= "edit") $ invalidArgsI ["You must be in an 'edit' mode to save scenario." :: Text]

    tryMaybies resolveBySesScenarioId resolveBySesExerciseId >>= \mscenario -> case mscenario of
        Just prevScenario -> do
          when (uId /= scenarioAuthorId prevScenario) $
            invalidArgsI ["You can work only on your own scenarios!" :: Text]
          t <- liftIO getCurrentTime
          runDB . insert_ $ prevScenario
                 { scenarioImage =  preview
                 , scenarioDescription = description
                 , scenarioGeometry = geometry
                 , scenarioLastUpdate = t
                 }
          setMessage . toHtml $ "Thank you, " <> userName user <> ", your design proposal has been saved."
          redirectUltDest MoocHomeR
        Nothing -> return ()

    completelyNewOne preview geometry description >>= \mscenarioId -> case mscenarioId of
        Just _ -> do
          setMessage . preEscapedToMarkup $
              "Thank you, " <> userName user <> ", your design proposal has been saved.<br>"
              <> "Now you can come back to edX or stay at qua-kit and explore submissions of other students.<br>"
              <> "<p class=\"text-brand-accent\"><b>Note!</b><br>We identified you via a special link on edX (when you pressed \"Go!\" button). "
              <> "You can come back and continue the work at the current state by going on the same link (button).<br>"
              <> "We sent to edX your base grade (60% of maximum); the grade will be updated as soon"
              <> " as other students start to vote and discuss your submission.</p>"
          ye <- getYesod
          meResId <- getsSafeSession userSessionEdxResourceId
          case meResId of
            Nothing -> return ()
            Just eResId -> sendEdxGrade (appSettings ye) uId eResId 0.6 (Just "Automatic grade on design submission.")
          redirectUltDest MoocHomeR
        Nothing -> do
          ses <- getSession
          $(logError) $ "A student failed to submit a proposal. Here is their session: "
                     <> Text.pack (show ses)
          invalidArgsI ["Some error occurred. Consult the developer team." :: Text]
  where
    tryMaybies m1 m2 = m1 >>= \mv1 -> case mv1 of
                          Nothing -> m2
                          Just _  -> return mv1


resolveBySesScenarioId :: Handler (Maybe Scenario)
resolveBySesScenarioId = do
  mescenario_id <- getsSafeSession userSessionScenarioId
  deleteSafeSession userSessionScenarioId
  case mescenario_id of
      Just i -> runDB (get i)
      Nothing -> return Nothing

resolveBySesExerciseId :: Handler (Maybe Scenario)
resolveBySesExerciseId = runMaybeT $ do
  userId <- MaybeT maybeAuthId
  mscp_id <- lift $ getsSafeSession userSessionCustomExerciseId
  fmap entityVal . MaybeT . runDB $ selectSource
                        (case mscp_id of
                           Just i  -> [ ScenarioAuthorId ==. userId
                                      , ScenarioTaskId   ==. i
                                      ]
                           _ -> [ScenarioAuthorId ==. userId]
                        )
                        [ Desc ScenarioLastUpdate
                        ] $$ await

completelyNewOne :: ByteString -> ByteString -> Text -> Handler (Maybe EdxResourceId)
completelyNewOne img geometry desc = runMaybeT $ do
  userId <- MaybeT maybeAuthId
  medxResId <- lift $ getsSafeSession userSessionEdxResourceId
  scpId <- MaybeT $ getsSafeSession userSessionCustomExerciseId
  MaybeT . runDB . runMaybeT $ do
    medxGrading <- case medxResId of
        Nothing -> return Nothing
        Just ri -> lift . getBy $ EdxGradeKeys ri userId
    t <- liftIO getCurrentTime
    let sc = Scenario
               userId
               scpId
               img
               geometry
               desc
               t
    scId <- lift $ insert sc
    void . lift . insert $ CurrentScenario scId
                           (scenarioAuthorId      sc)
                           (scenarioTaskId        sc)
                           (scenarioDescription   sc)
                           (entityVal <$> medxGrading)
                           Nothing
                           (scenarioLastUpdate    sc)
                           False
    MaybeT $ return medxResId
