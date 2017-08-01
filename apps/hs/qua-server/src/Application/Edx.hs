module Application.Edx where


import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Web.LTI
import Model.Session
import Text.Shakespeare.Text (st)

import Import.NoFoundation


-- | Write correct data into EdxGrading, EdxCourse, EdxResource, etc.
setupEdxGrading :: ( YesodAuth app
                   , YesodPersist app
                   , YesodAuthPersist app
                   , AuthId app ~ UserId
                   , BaseBackend (YesodPersistBackend app) ~ SqlBackend
                   , PersistUniqueWrite (YesodPersistBackend app)
                   )
                => UserId
                -> [(Text,Text)] -- ^ session parameters
                -> HandlerT app IO ()
setupEdxGrading userId params = do
  lookupAndSave "lis_outcome_service_url"
  lookupAndSave "lis_result_sourcedid"
  lookupAndSave "resource_link_id"
  lookupAndSave "context_id"
  mapM_ (uncurry setSession) $ filter (isPrefixOf "custom_". fst ) params
  runDB $
    case (,,) <$> mresource_link_id <*> mcontext_id <*> mexercise_id of
      Nothing  -> return ()
      Just (resource_link_id, context_id, exercise_id) -> do
        Entity edxCourseId _ <- upsert (EdxCourse context_id Nothing) []
        mEdxRes <- getBy (EdxResLinkId resource_link_id edxCourseId)
        edxResId <- case mEdxRes of
          Just (Entity ek _) -> return ek
          Nothing -> insert $ EdxResource resource_link_id edxCourseId exercise_id (Map.lookup "custom_component_display_name" pm)
        -- update generic edx resource parameters
        saveCustomParams edxResId
        lift $ setSafeSession userSessionEdxResourceId edxResId
        -- update personal grade link
        case (,) <$> Map.lookup "lis_outcome_service_url" pm
                 <*> Map.lookup "lis_result_sourcedid" pm of
          Just (outcome_url, resultId) -> void $ upsert (EdxGrading edxResId userId outcome_url resultId) []
          Nothing -> return ()
  where
    mresource_link_id = Map.lookup "resource_link_id" pm
    mcontext_id       = Map.lookup "context_id" pm
    mexercise_id      = Map.lookup "custom_exercise_id" pm >>= parseSqlKey
    lookupAndSave t = forM_ (Map.lookup t pm) (setSession t)
    pm = Map.fromList params
    saveCustomParams ek = mapM_ ((\(k,v) -> void $ upsert (EdxResourceParam ek k v) []) . first (drop 7))
                                $ filter (isPrefixOf "custom_". fst ) params


-- | Send a grade to edX via an http request immediately
sendEdxGrade :: ( YesodAuth app
                , YesodPersist app
                , YesodAuthPersist app
                , AuthId app ~ UserId
                , BaseBackend (YesodPersistBackend app) ~ SqlBackend
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
                     , MonadLogger m)
                  => AppSettings
                  -> Maybe EdxGrading
                  -> Double
                  -> Maybe Text -> m ()
executeEdxGrading _ Nothing _ _ = $(logWarn) "Could not send a grade to a student because EdxGrading record is not found."
executeEdxGrading aSettings (Just (EdxGrading _ _ outcomeUrl resultId)) grade comment =
  void $ replaceResultRequest (appLTICredentials aSettings) (Text.unpack outcomeUrl) resultId grade comment >>= httpNoBody


-- | Send all pending grades to edX
executeEdxGradingQueue :: (MonadLogger a, MonadIO a, MonadResource a)
                       => AppSettings
                       -> ReaderT SqlBackend (ReaderT Manager a) ()
executeEdxGradingQueue aSettings = do
    $(logInfo) [st| Executing edX grading queue... |]
    -- send requests
    gradeRequests $$ awaitForever processReq
    -- delete all requests in the queue
    deleteWhere ([] :: [Filter EdxGradingQueue])
    $(logInfo) [st| edx grading queue execution finished! |]
  where
    gradeRequests = selectSource ([] :: [Filter EdxGradingQueue]) []
    processReq (Entity _ EdxGradingQueue {..}) = lift $ do
      mgr <- get edxGradingQueueEdxGradingId
      lift $ executeEdxGrading aSettings mgr edxGradingQueueGrade edxGradingQueueComment



-- | Insert a record into EdxGradingQueue table to be executed
--   on next batch edX grading.
queueForGrading :: ( YesodAuth app
                   , YesodPersist app
                   , YesodAuthPersist app
                   , AuthId app ~ UserId
                   , BaseBackend (YesodPersistBackend app) ~ SqlBackend
                   , PersistUniqueWrite (YesodPersistBackend app)
                   , HasHttpManager app
                   )
                => UserId
                -> EdxResourceId -- ^ Exercise unit in edX
                -> Double -- ^ Grade on a scale [0,1]
                -> Maybe Text -- ^ Grade comment
                -> HandlerT app IO ()
queueForGrading userId eResId grade mcomment = runDB $ do
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
