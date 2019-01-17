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
import           Database.Persist.Sql        (runSqlPool)
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
    transferTable poolPostgres poolSqlite (P @Criterion)
    transferTable poolPostgres poolSqlite (P @Exercise)
    transferTable poolPostgres poolSqlite (P @ExerciseCriterion)
    transferTable poolPostgres poolSqlite (P @User)
    -- transferTable poolPostgres poolSqlite (P @UserProp)
    transferTable poolPostgres poolSqlite (P @UserExercise)

    transferTable poolPostgres poolSqlite (P @EdxCourse)
    transferTable poolPostgres poolSqlite (P @EdxResource)
    -- transferTable poolPostgres poolSqlite (P @EdxResourceParam)
    transferTable poolPostgres poolSqlite (P @EdxGrading)
    -- transferTable poolPostgres poolSqlite (P @EdxGradingQueue)

    transferTable poolPostgres poolSqlite (P @Scenario)
    transferTable poolPostgres poolSqlite (P @CurrentScenario)
    transferTable poolPostgres poolSqlite (P @Review)
    transferTable poolPostgres poolSqlite (P @ExpertReview)

    transferTable poolPostgres poolSqlite (P @Vote)
    transferTable poolPostgres poolSqlite (P @Rating)
    transferTable poolPostgres poolSqlite (P @VoteRating)

    -- transferTable poolPostgres poolSqlite (P @Feedback)
    -- transferTable poolPostgres poolSqlite (P @Survey)
    -- transferTable poolPostgres poolSqlite (P @QuaViewWebLogging)


-- Proxy type
data P t = P

transferTable :: ( MonadIO m
                 , MonadBaseControl IO m
                 , MonadThrow m
                 , PersistRecordBackend record SqlBackend
                 , Typeable record
                 )
              => Pool SqlBackend
              -> Pool SqlBackend
              -> P record
              -> m ()
transferTable sourcePool destPool p = do
    runResourceT $
      withResource destPool $ \destBackend ->
      withResource sourcePool $ \sourceBackend ->
      runConduit $
        withBackend sourceBackend (selectKeys [] [])
        .|
        withBackend sourceBackend (mapMC getJustEntity)
        .|
        withBackend destBackend
        (awaitForever (\(Entity i r) -> lift (repsert i (enforceType p r))))
    -- asource <- runSqlPool (selectSourceRes [] []) sourcePool
    -- flip runSqlPool sourcePool $ runConduit $
    --   withAcquire asource $ \source ->
    --    source .| (awaitForever (\(Entity i r) -> lift (repsert i (enforceType p r))))
    putStrLn $ "Done: " <> tshow (typeRep p)
  where
    enforceType :: P t -> t -> t
    enforceType _ = id
    withBackend b = transPipe (`runReaderT` b)
