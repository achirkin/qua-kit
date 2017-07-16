module Model.Session
    ( SessionLens
    , getsSafeSession
    , deleteSafeSession
    , setSafeSession
    , parseSqlKey
    , userSessionContextId
    , userSessionResourceLink
    , userSessionOutcomeServiceUrl
    , userSessionResultSourceId
    , userSessionCustomExerciseId
    , userSessionCustomExerciseCount
    , userSessionCustomExerciseType
    , userSessionCompareCounter
    , userSessionQuaViewMode
    , userSessionScenarioId
    ) where

import Import.NoFoundation

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Database.Persist.Sql

data SessionLens a = SessionLens
    { convFunc :: Maybe Text -> Maybe a
    , convInvFunc :: a -> Text
    , convKey :: Text
    }

textSessionLens :: Text -> SessionLens Text
textSessionLens = SessionLens id id

readSessionLens :: (Show a, Read a) => Text -> SessionLens a
readSessionLens = SessionLens (>>= readMay) (T.pack . show)

userSessionContextId :: SessionLens Text
userSessionContextId = textSessionLens "context_id"

userSessionResourceLink :: SessionLens Text
userSessionResourceLink = textSessionLens "resource_link_id"

userSessionOutcomeServiceUrl :: SessionLens Text
userSessionOutcomeServiceUrl = textSessionLens "lis_outcome_service_url"

userSessionResultSourceId :: SessionLens Text
userSessionResultSourceId = textSessionLens "lis_result_sourcedid"

userSessionCustomExerciseId :: SessionLens ScenarioProblemId
userSessionCustomExerciseId =
    SessionLens
        (>>= parseSqlKey)
        (T.pack . show . fromSqlKey)
        "custom_exercise_id"

userSessionCustomExerciseCount :: SessionLens Int
userSessionCustomExerciseCount = readSessionLens "custom_exercise_count"

userSessionCustomExerciseType :: SessionLens Text
userSessionCustomExerciseType = readSessionLens "custom_exercise_type"

userSessionCompareCounter :: SessionLens Int
userSessionCompareCounter = readSessionLens "compare_counter"

userSessionQuaViewMode :: SessionLens Text
userSessionQuaViewMode = textSessionLens "qua_view_mode"

userSessionScenarioId :: SessionLens ScenarioId
userSessionScenarioId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "scenario_id"

sessionVar :: SessionLens a -> Text
sessionVar = ("session_" <>) . convKey

getsSafeSession ::
       ( YesodAuth app
       , YesodPersist app
       , YesodAuthPersist app
       , AuthId app ~ UserId
       , BaseBackend (YesodPersistBackend app) ~ SqlBackend
       , PersistUniqueWrite (YesodPersistBackend app)
       )
    => SessionLens b
    -> HandlerT app IO (Maybe b)
getsSafeSession sl@SessionLens {..} = do
    mval <- convFunc <$> lookupSession convKey
    case mval of
        Just val -> pure $ Just val
        Nothing -> do
            mauth <- maybeAuthId
            case mauth of
                Nothing -> pure Nothing
                Just uid -> do
                    mprop <- runDB $ getBy $ UserProperty uid $ sessionVar sl
                    pure $ convFunc ((userPropValue . entityVal) <$> mprop)

deleteSafeSession ::
       ( YesodAuth app
       , YesodPersist app
       , YesodAuthPersist app
       , AuthId app ~ UserId
       , BaseBackend (YesodPersistBackend app) ~ SqlBackend
       , PersistUniqueWrite (YesodPersistBackend app)
       )
    => SessionLens b
    -> HandlerT app IO ()
deleteSafeSession sl = do
    mauth <- maybeAuthId
    case mauth of
        Nothing -> pure ()
        Just uid -> runDB $ deleteBy $ UserProperty uid $ sessionVar sl
    deleteSession $ convKey sl

setSafeSession ::
       ( YesodAuth app
       , YesodPersist app
       , YesodAuthPersist app
       , AuthId app ~ UserId
       , BaseBackend (YesodPersistBackend app) ~ SqlBackend
       , PersistUniqueWrite (YesodPersistBackend app)
       )
    => SessionLens b
    -> b
    -> HandlerT app IO ()
setSafeSession sl@SessionLens {..} val = do
    setSession convKey $ convInvFunc val
    mauth <- maybeAuthId
    case mauth of
        Nothing -> pure ()
        Just uid ->
            void $
            runDB $
            upsertBy
                (UserProperty uid $ sessionVar sl)
                (UserProp uid (sessionVar sl) $ convInvFunc val)
                [UserPropValue =. convInvFunc val]

parseSqlKey :: (ToBackendKey SqlBackend a) => Text -> Maybe (Key a)
parseSqlKey t =
    case decimal t of
        Right (i, _) -> Just $ toSqlKey i
        _ -> Nothing
