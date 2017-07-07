module Model.Session
    ( UserSession(..)
    , getsSafeSession
    ) where

import Import

data UserSession = UserSession
    { userSessionRole :: UserRole
    , userSessionContextId :: Maybe Text
    , userSessionResourceLink :: Maybe Text
    , userSessionOutcomeServiceUrl :: Maybe Text
    , userSessionResultSourceId :: Maybe Text
    , userSessionCustomExerciseId :: Maybe ScenarioProblemId
    , userSessionCustomExerciseCount :: Maybe Int
    , userSessionCustomExerciseType :: Maybe Text
    , userSessionCompareCounter :: Maybe Int
    , userSessionQuaViewMode :: Maybe Text
    , userSessionScenarioId :: Maybe ScenarioId
    } deriving (Show, Eq)

getSafeSession :: Handler UserSession
getSafeSession =
    UserSession <$> (muserRole <$> maybeAuth) <*> lookupSession "context_id" <*>
    lookupSession "resource_link_id" <*>
    lookupSession "lis_outcome_service_url" <*>
    lookupSession "lis_result_sourcedid" <*>
    readSqlKey "custom_exercise_id" <*>
    readSession "custom_exercise_count" <*>
    readSession "custom_exercise_type" <*>
    readSession "compare_counter" <*>
    lookupSession "qua_view_mode" <*>
    readSqlKey "scenario_id"
  where
    readSession key = ((>>= readMay) <$> lookupSession key)
    readSqlKey key = (>>= parseSqlKey) <$> lookupSession key

getsSafeSession :: (UserSession -> b) -> Handler b
getsSafeSession func = fmap func getSafeSession
