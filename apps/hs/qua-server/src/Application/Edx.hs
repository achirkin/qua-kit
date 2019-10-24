module Application.Edx where

import Control.Monad.Logger (runLoggingT, LogSource, LogStr, Loc)
import Control.Concurrent   (forkIO)
import Data.Pool            (Pool)
import Database.Persist.Sql (runSqlPool)
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Web.LTI
import Model.Session
import Text.Shakespeare.Text (st)

import Import.NoFoundation
import Application.Grading

-- | Write correct data into EdxGrading, EdxCourse, EdxResource, etc.
setupEdxGrading :: ( YesodAuth app
                   , YesodPersist app
                   , YesodAuthPersist app
                   , AuthId app ~ UserId
                   , BaseBackend (YesodPersistBackend app) ~ SqlBackend
                   , PersistUniqueWrite (YesodPersistBackend app)
                   )
                => UserId
                -> [(Text,Text)] -- ^ cred params we originally got from edx (via dispatchLti)
                -> HandlerT app IO ()
setupEdxGrading userId params = do
  mserviceUrl      <- lookupAndSave userSessionEdxLisOutcomeServiceUrl
  msourcedId       <- lookupAndSave userSessionEdxLisResultSourcedId
  mresourceLinkId  <- lookupAndSave userSessionEdxResourceLinkId
  mcontextId       <- lookupAndSave userSessionEdxContextId
  maybe (return ()) (setSafeSessionForUid userId userSessionCustomExerciseCount)
    (Map.lookup (convKey userSessionCustomExerciseCount) pm >>= parseInt)
  let mexercise_id = Map.lookup "custom_exercise_id" pm >>= parseSqlKey --below saved as currentExerciseId
  mapM_ (uncurry setSession) $ filter (isPrefixOf "custom_". fst ) params
  runDB $
    case (,,) <$> mresourceLinkId <*> mcontextId <*> mexercise_id of
      Nothing  -> return ()
      Just (resource_link_id, contextId, exercise_id) -> do
        Entity edxCourseId _ <- upsert (EdxCourse contextId Nothing) []
        mEdxRes <- getBy (EdxResLinkId resource_link_id edxCourseId)
        edxResId <- case mEdxRes of
          Just (Entity ek _) -> return ek
          Nothing -> insert $ EdxResource resource_link_id edxCourseId exercise_id
                                (Map.lookup "custom_component_display_name" pm)
        -- update generic edx resource parameters
        saveCustomParams edxResId
        lift $ setSafeSessionForUid userId userSessionEdxResourceId edxResId
        lift $ setSafeSessionForUid userId userSessionCurrentExerciseId exercise_id
        -- update personal grade link
        case (,) <$> mserviceUrl <*> msourcedId of
          Just (outcome_url, resultId) -> void $ upsert (EdxGrading edxResId userId outcome_url resultId) []
          Nothing -> return ()
  where
    lookupAndSave sl = do
      let mval = Map.lookup (convKey sl) pm
      maybe (return ()) (setSafeSessionForUid userId sl) mval
      return mval
    pm = Map.fromList params
    -- the EdxResourceParam data is probably never used, just so the data is saved somewhere:
    saveCustomParams ek = mapM_ ((\(k,v) -> void $ upsert (EdxResourceParam ek k v) []) . first (drop 7))
                                $ filter (isPrefixOf "custom_". fst ) params


-- | Send a grade to edX via an http request immediately
sendEdxGrade :: ( YesodAuth app
                , YesodPersist app
                , YesodAuthPersist app
                , AuthId app ~ UserId
                , YesodPersistBackend app ~ SqlBackend
                , PersistUniqueWrite (YesodPersistBackend app)
                , HasHttpManager app
                )
             => AppSettings -- ^ our yesod AppSettings
             -> UserId
             -> EdxResourceId -- ^ Exercise unit in edX
             -> Double -- ^ Grade on a scale [0,1]
             -> Maybe Text -- ^ Grade comment
             -> HandlerT app IO ()
sendEdxGrade aSettings userId exUnitId grade comment = do
    mgrading <- runDB . getBy $ EdxGradeKeys exUnitId userId
    executeEdxGrading aSettings (entityVal <$> mgrading) grade comment

executeEdxGrading :: ( MonadReader env m
                     , HasHttpManager env
                     , MonadIO m
                     , MonadLogger m
                     , MonadCatch m)
                  => AppSettings
                  -> Maybe EdxGrading
                  -> Double
                  -> Maybe Text -> m ()
executeEdxGrading _ Nothing _ _ = $(logWarn) "[GRADING][edX] Could not send a grade to a student because EdxGrading record is not found."
executeEdxGrading aSettings (Just (EdxGrading _ _ outcomeUrl resultId)) grade comment =
  ( replaceResultRequest (appLTICredentials aSettings) (Text.unpack outcomeUrl) resultId grade comment
      >>= void . httpNoBody
  ) `catch` (\e -> $(logWarn) $ "[GRADING][edX] " <> Text.pack (show (e :: SomeException)))




-- | Send all pending grades to edX
executeEdxGradingQueue :: (MonadLogger a, MonadIO a, MonadResource a, MonadCatch a)
                       => AppSettings
                       -> ReaderT SqlBackend (ReaderT Manager a) ()
executeEdxGradingQueue aSettings = do
    $(logWarn) [st| Executing edX grading queue... |]
    -- send requests
    gradeRequests $$ awaitForever processReq
    $(logWarn) [st| edx grading queue execution finished! |]
  where
    gradeRequests = selectSource ([] :: [Filter EdxGradingQueue]) []
    processReq (Entity gqk EdxGradingQueue {..}) = lift $ do
      mgr <- get edxGradingQueueEdxGradingId
      lift $ executeEdxGrading aSettings mgr edxGradingQueueGrade edxGradingQueueComment
      delete gqk

-- | Schedule sending grades to edX.
scheduleUpdateGrades :: Int -- ^ time interval in seconds
                     -> AppSettings
                     -> Manager
                     -> Pool SqlBackend
                     -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
                     -> IO ()
scheduleUpdateGrades dt aSettings httpMan pool logFunc = void . forkIO . forever $ do
    -- wait ten seconds just to let yesod do other work first
    threadDelay (10*1000000)
    runResourceT ( executeEdxGradingQueue aSettings
                    `runSqlPool` pool
                    `runReaderT` httpMan
                 )
        `runLoggingT` logFunc
    threadDelay dts
  where
    dts = dt * 1000000



-- | Insert a record into EdxGradingQueue table to be executed
--   on next batch edX grading.
queueForGrading :: (MonadLogger a, MonadIO a)
                => UserId
                -> EdxResourceId -- ^ Exercise unit in edX
                -> Double -- ^ Grade on a scale [0,1]
                -> Maybe Text -- ^ Grade comment
                -> ReaderT SqlBackend a ()
queueForGrading userId eResId grade mcomment = do
    meg <- getBy $ EdxGradeKeys eResId userId
    case meg of
      Nothing -> $(logWarn) [st| [GRADING] Student #{show userId} should be graded on exercise #{show eResId},
                                 but misses their corresponding EdxGrading record!
                               |]
      Just eg -> void $ upsertBy (EdxGradingRef $ entityKey eg)
        (EdxGradingQueue (entityKey eg) grade mcomment)
        [ EdxGradingQueueGrade =. grade
        , EdxGradingQueueComment =. mcomment
        ]

-- | Grade design based on its rating
queueDesignGrade :: (MonadLogger a, MonadIO a)
                 => ScenarioId
                 -> ReaderT SqlBackend a ()
queueDesignGrade scId = void . runMaybeT $ do
   cs <- fmap entityVal . MaybeT  $ getBy (LatestSubmissionId scId)
   csGrade <- MaybeT . pure $ currentScenarioGrade cs
   gradingId <- MaybeT . pure . currentScenarioEdxGradingId $ cs
   EdxGrading {..} <- MaybeT $ get gradingId
   lift $ queueForGrading (currentScenarioAuthorId cs)
                          edxGradingResourceId
                          (csGradeEdxGrade csGrade)
                          (Just "Qua-kit rating-based vote grade.")

-- | Grade voter based on their rating.
queueVoteGrade :: (MonadLogger a, MonadIO a)
               => VoteId
               -> ReaderT SqlBackend a ()
queueVoteGrade voteId =  void . runMaybeT $ do
    Vote{..} <- MaybeT $ get voteId
    eGrdId <- MaybeT $ pure voteEdxGradingId
    EdxGrading{..} <- MaybeT $ get eGrdId
    EdxResource{..} <- MaybeT $ get edxGradingResourceId
    Entity _ VoteRating{..} <- MaybeT . getBy $ VoteRatingOf voteVoterId edxResourceExerciseId
    lift $ queueForGrading voteVoterId
                           edxGradingResourceId
                           (compareRatingToEdxGrade voteRatingValue)
                           (Just "Qua-kit rating-based vote grade.")
