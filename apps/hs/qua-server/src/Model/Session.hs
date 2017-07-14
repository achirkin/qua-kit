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

import ClassyPrelude.Yesod as Import hiding (Handler, race_)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Database.Persist
import Database.Persist.Sql
import Model
import Model as Import
import Settings as Import
import Settings.StaticFiles as Import
import Yesod
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2
import Yesod.Default.Config2 as Import

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

getsSafeSession ::
       (MonadBaseControl IO m, MonadThrow m, MonadIO m)
    => SessionLens b
    -> HandlerT app m (Maybe b)
getsSafeSession SessionLens {..} = fmap convFunc $ lookupSession convKey

deleteSafeSession ::
       (MonadBaseControl IO m, MonadThrow m, MonadIO m)
    => SessionLens b
    -> HandlerT app m ()
deleteSafeSession = deleteSession . convKey

setSafeSession ::
       (MonadBaseControl IO m, MonadThrow m, MonadIO m)
    => SessionLens b
    -> b
    -> HandlerT app m ()
setSafeSession SessionLens {..} = setSession convKey . convInvFunc

parseSqlKey :: (ToBackendKey SqlBackend a) => Text -> Maybe (Key a)
parseSqlKey t =
    case decimal t of
        Right (i, _) -> Just $ toSqlKey i
        _ -> Nothing
