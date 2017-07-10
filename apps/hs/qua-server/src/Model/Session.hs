module Model.Session
    ( SessionLens
    , getsSafeSession
    , deleteSafeSession
    , setSafeSession
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

import Import

import qualified Data.Text as T
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

getsSafeSession :: SessionLens b -> Handler (Maybe b)
getsSafeSession SessionLens {..} = fmap convFunc $ lookupSession convKey

deleteSafeSession :: SessionLens b -> Handler ()
deleteSafeSession = deleteSession . convKey

setSafeSession :: SessionLens b -> b -> Handler ()
setSafeSession SessionLens{..} = setSession convKey . convInvFunc
