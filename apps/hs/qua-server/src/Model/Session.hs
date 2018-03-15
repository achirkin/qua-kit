{-# OPTIONS_HADDOCK hide, prune #-}
-- | Currently, all session variable is stored in the database table user_prop.
--   It has nothing todo with user cookies since the last update!
module Model.Session
    ( SessionLens(..)
    , getsSafeSession
    , deleteSafeSession
    , setSafeSession
    , setSafeSessionForUid
    , parseInt
    , parseSqlKey
    , userSessionCurrentExerciseId
    , userSessionCustomExerciseCount
    , userSessionCompareCounter
    , userSessionEdxResourceId
    , userSessionEdxLisOutcomeServiceUrl
    , userSessionEdxLisResultSourcedId
    , userSessionEdxResourceLinkId
    , userSessionEdxContextId
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

readSessionLens :: (Show a, Read a) => Text -> SessionLens a
readSessionLens = SessionLens (>>= readMay) (T.pack . show)

userSessionCurrentExerciseId :: SessionLens ExerciseId
userSessionCurrentExerciseId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "currentExerciseId"

userSessionCustomExerciseCount :: SessionLens Int
userSessionCustomExerciseCount = readSessionLens "custom_exercise_count"

userSessionCompareCounter :: SessionLens Int
userSessionCompareCounter = readSessionLens "compare_counter"

userSessionEdxResourceId :: SessionLens EdxResourceId
userSessionEdxResourceId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "edx_resource_id"

userSessionEdxLisOutcomeServiceUrl :: SessionLens Text
userSessionEdxLisOutcomeServiceUrl =
    SessionLens id id "lis_outcome_service_url"

userSessionEdxLisResultSourcedId :: SessionLens Text
userSessionEdxLisResultSourcedId =
    SessionLens id id "lis_result_sourcedid"

userSessionEdxResourceLinkId :: SessionLens Text
userSessionEdxResourceLinkId =
    SessionLens id id "resource_link_id"

userSessionEdxContextId :: SessionLens Text
userSessionEdxContextId =
    SessionLens id id "context_id"

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


-- | sets cookie and also upserts UserProp in DB
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
setSafeSession sl val = do
    mauth <- maybeAuthId
    case mauth of
      Nothing -> pure ()
      Just uid -> setSafeSessionForUid uid sl val

setSafeSessionForUid ::
       ( YesodAuth app
       , YesodPersist app
       , YesodAuthPersist app
       , AuthId app ~ UserId
       , BaseBackend (YesodPersistBackend app) ~ SqlBackend
       , PersistUniqueWrite (YesodPersistBackend app)
       )
    => UserId
    -> SessionLens b
    -> b
    -> HandlerT app IO ()
setSafeSessionForUid uid sl@SessionLens {..} val =
  void $ runDB $ upsertBy
    (UserProperty uid $ sessionVar sl)
    (UserProp uid (sessionVar sl) $ convInvFunc val)
    [UserPropValue =. convInvFunc val]

parseInt :: Integral a => Text -> Maybe a
parseInt t =
  case decimal t of
      Right (i, _) -> Just i
      _ -> Nothing

parseSqlKey :: (ToBackendKey SqlBackend a) => Text -> Maybe (Key a)
parseSqlKey = (fmap toSqlKey) . parseInt
