{-# OPTIONS_HADDOCK hide, prune #-}
module Model.Session
    ( SessionLens
    , getsSafeSession
    , deleteSafeSession
    , setSafeSession
    , parseSqlKey
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

textSessionLens :: Text -> SessionLens Text
textSessionLens = SessionLens id id

readSessionLens :: (Show a, Read a) => Text -> SessionLens a
readSessionLens = SessionLens (>>= readMay) (T.pack . show)

userSessionCustomExerciseCount :: SessionLens Int
userSessionCustomExerciseCount = readSessionLens "custom_exercise_count"

userSessionCompareCounter :: SessionLens Int
userSessionCompareCounter = readSessionLens "compare_counter"

userSessionEdxResourceId :: SessionLens EdxResourceId
userSessionEdxResourceId =
    SessionLens (>>= parseSqlKey) (T.pack . show . fromSqlKey) "edx_resource_id"


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
