{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           ClassyPrelude.Yesod
import           Control.Monad.Logger        (LogLevel (..), LogSource,
                                              filterLogger, runStdoutLoggingT)
import           Data.Aeson                  (withObject)
import           Data.Pool                   (Pool, withResource)
import           Data.Typeable               (typeRep)
import           Data.Yaml.Config
import qualified Database.Persist.Postgresql as Postgres
import           Database.Persist.Sql        (fromSqlKey, runSqlPool)
import qualified Database.Persist.Sqlite     as Sqlite
import           Model


createPostgresPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
                   => Postgres.PostgresConf -> m (Pool SqlBackend)
createPostgresPool c = Postgres.createPostgresqlPool (Postgres.pgConnStr c) (Postgres.pgPoolSize c)


createSqlitePool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
                 => Sqlite.SqliteConf -> m (Pool SqlBackend)
createSqlitePool (Sqlite.SqliteConf db ps) = Sqlite.createSqlitePool db ps
createSqlitePool (Sqlite.SqliteConfInfo ci ps) = Sqlite.createSqlitePoolFromInfo ci ps

data AppSettings = AppSettings
  { appPostgresConf :: Postgres.PostgresConf
  , appSqliteConf   :: Sqlite.SqliteConf
  } deriving (Show)


newtype DBSettings a = DBSettings a

instance FromJSON a => FromJSON (DBSettings a) where
    parseJSON = withObject "DBSettings" $ fmap DBSettings . (.: "database")

getAppSettings :: MonadIO m => m AppSettings
getAppSettings = liftIO $ do
    DBSettings appPostgresConf <- loadYamlSettings ["config/settingsPostgreSQL.yml"] [] useEnv
    DBSettings appSqliteConf   <- loadYamlSettings ["config/settingsSqlite.yml"] [] useEnv
    return AppSettings {..}


filterLogLevel :: LogSource -> LogLevel -> Bool
filterLogLevel _ l = l >= LevelInfo

main :: IO ()
main = runStdoutLoggingT $ filterLogger filterLogLevel $ do
    appSettings  <- getAppSettings
    poolPostgres <- createPostgresPool $ appPostgresConf appSettings
    poolSqlite   <- createSqlitePool   $ appSqliteConf   appSettings

    -- make sure the database is there
    runSqlPool (runMigration migrateAll) poolSqlite

    -- and go table by table...
    -- make sure the order is correct
    -- also omit tables, which are not needed for the "expo" version of qua-kit
    transferTable poolPostgres poolSqlite (keep @Criterion)
    transferTable poolPostgres poolSqlite (keep @Exercise)
    transferTable poolPostgres poolSqlite (keep @ExerciseCriterion)
    transferTable poolPostgres poolSqlite clearSensistiveUserData
    -- transferTable poolPostgres poolSqlite (keep @UserProp)
    transferTable poolPostgres poolSqlite (keep @UserExercise)

    transferTable poolPostgres poolSqlite (keep @EdxCourse)
    transferTable poolPostgres poolSqlite (keep @EdxResource)
    transferTable poolPostgres poolSqlite (keep @EdxResourceParam)
    transferTable poolPostgres poolSqlite (keep @EdxGrading)
    -- transferTable poolPostgres poolSqlite (keep @EdxGradingQueue)

    transferTable poolPostgres poolSqlite (keep @Scenario)
    transferTable poolPostgres poolSqlite (keep @CurrentScenario)
    transferTable poolPostgres poolSqlite (keep @Review)
    transferTable poolPostgres poolSqlite (keep @ExpertReview)

    transferTable poolPostgres poolSqlite (keep @Vote)
    transferTable poolPostgres poolSqlite (keep @Rating)
    transferTable poolPostgres poolSqlite (keep @VoteRating)

    -- transferTable poolPostgres poolSqlite (keep @Feedback)
    -- transferTable poolPostgres poolSqlite (keep @Survey)
    transferTable poolPostgres poolSqlite clearIpAddress


-- | Keep record as-is.
--   Just a version of id that takes one type parameter
keep :: Key a -> a -> a
keep = const id

clearSensistiveUserData :: Key User -> User -> User
clearSensistiveUserData i u = u
  { userEthUserName = Nothing
  , userEdxUserId   = Nothing
  , userEmail       = Just $ "user" <> itxt <> "@notgmail.com"
  , userPassword    = Just "sha256|16|AK5Dd0IF3Hdkgywn506B5Q==|MxrRScUOlKp7dXOEGMpEYiiMvN/Us7S9XRKVXJnAQlg="
  }
  where
    itxt = tshow $ fromSqlKey i


clearIpAddress :: Key QuaViewWebLogging -> QuaViewWebLogging -> QuaViewWebLogging
clearIpAddress _ q = q { quaViewWebLoggingIpAddress = Just "127.0.0.1" }

transferTable :: ( MonadIO m
                 , MonadBaseControl IO m
                 , MonadThrow m
                 , PersistRecordBackend record SqlBackend
                 , Typeable record
                 )
              => Pool SqlBackend
              -> Pool SqlBackend
              -> (Key record -> record -> record)
              -> m ()
transferTable sourcePool destPool f = do
    runResourceT $
      withResource sourcePool $ \keysBackend ->
      withResource sourcePool $ \sourceBackend ->
      withResource destPool   $ \destBackend ->
      runConduit $
        withBackend keysBackend (selectKeys [] [])
        .|
        withBackend sourceBackend (mapMC getJustEntity)
        .|
        withBackend destBackend
        (awaitForever (\(Entity i r) -> lift (repsert i (f i r))))
    putStrLn $ "Done: " <> tshow (typeRep f)
  where
    withBackend b = transPipe (`runReaderT` b)
