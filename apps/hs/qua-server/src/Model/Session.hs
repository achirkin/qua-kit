{-# OPTIONS_HADDOCK hide, prune #-}
-- | Currently, all session variable is stored in the database table user_prop.
--   It has nothing todo with user cookies since the last update!
module Model.Session
    ( SessionLens
    , getsSafeSession
    , deleteSafeSession
    , setSafeSession
    , parseSqlKey
    , userSessionCurrentExerciseId
    , userSessionCustomExerciseCount
    , userSessionCompareCounter
    , userSessionEdxResourceId
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

userSessionCustomExerciseCount :: SessionLens Int
userSessionCustomExerciseCount = readSessionLens "custom_exercise_count"

userSessionCompareCounter :: SessionLens Int
userSessionCompareCounter = readSessionLens "compare_counter"

userSessionEdxResourceId :: SessionLens EdxResourceId
userSessionEdxResourceId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "edx_resource_id"

userSessionCurrentExerciseId :: SessionLens ExerciseId
userSessionCurrentExerciseId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "currentExerciseId"

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
setSafeSession sl@SessionLens {..} val = do
    mauth <- maybeAuthId
    case mauth of
      Nothing -> pure ()
      Just uid -> void $ runDB $ upsertBy
        (UserProperty uid $ sessionVar sl)
        (UserProp uid (sessionVar sl) $ convInvFunc val)
        [UserPropValue =. convInvFunc val]

parseSqlKey :: (ToBackendKey SqlBackend a) => Text -> Maybe (Key a)
parseSqlKey t =
    case decimal t of
        Right (i, _) -> Just $ toSqlKey i
        _ -> Nothing
