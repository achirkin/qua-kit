module Model.Session
    ( SessionLens
    , getsSafeSession
    , deleteSafeSession
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

data SessionLens a = SessionLens
    { convFunc :: Maybe Text -> a
    , convKey :: Text
    }

userSessionContextId :: SessionLens (Maybe Text)
userSessionContextId = SessionLens id "context_id"

userSessionResourceLink :: SessionLens (Maybe Text)
userSessionResourceLink = SessionLens id "resource_link_id"

userSessionOutcomeServiceUrl :: SessionLens (Maybe Text)
userSessionOutcomeServiceUrl = SessionLens id "lis_outcome_service_url"

userSessionResultSourceId :: SessionLens (Maybe Text)
userSessionResultSourceId = SessionLens id "lis_result_sourcedid"

userSessionCustomExerciseId :: SessionLens (Maybe ScenarioProblemId)
userSessionCustomExerciseId = SessionLens (>>= parseSqlKey) "custom_exercise_id"

userSessionCustomExerciseCount :: SessionLens (Maybe Int)
userSessionCustomExerciseCount =
    SessionLens (>>= readMay) "custom_exercise_count"

userSessionCustomExerciseType :: SessionLens (Maybe Text)
userSessionCustomExerciseType = SessionLens (>>= readMay) "custom_exercise_type"

userSessionCompareCounter :: SessionLens (Maybe Int)
userSessionCompareCounter = SessionLens (>>= readMay) "compare_counter"

userSessionQuaViewMode :: SessionLens (Maybe Text)
userSessionQuaViewMode = SessionLens id "qua_view_mode"

userSessionScenarioId :: SessionLens (Maybe ScenarioId)
userSessionScenarioId = SessionLens (>>= parseSqlKey) "scenario_id"

getsSafeSession :: SessionLens b -> Handler b
getsSafeSession SessionLens {..} = fmap convFunc $ lookupSession convKey

deleteSafeSession :: SessionLens b -> Handler ()
deleteSafeSession = deleteSession . convKey
