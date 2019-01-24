{-# OPTIONS_HADDOCK hide, prune #-}
{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Data.Pool                   (Pool)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import Database.Persist.Sql
#if POSTGRESQL
import Database.Persist.Postgresql
#else
import Database.Persist.Sqlite
import Control.Lens ((%~))
import Data.Function ((&))
#endif
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
--import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import Web.LTI

#if EXPO
import Language.Haskell.TH
import qualified Unsafe.Coerce as Unsafe (unsafeCoerce)
import qualified System.IO.Unsafe as Unsafe (unsafePerformIO)
#endif

-- Go with Sqlite on development, and Postgres on production
#if POSTGRESQL
type PersistConf = PostgresConf
#else
type PersistConf = SqliteConf
#endif


createAppSqlPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, IsSqlBackend backend)
                 => PersistConf -> m (Pool backend)
#if POSTGRESQL
createAppSqlPool c = prepareSqlPool
   <$> createPostgresqlPool (pgConnStr c) (pgPoolSize c)
  -- encodeUtf8 . processConnStr . decodeUtf8 $ pgConnStr c
#else
createAppSqlPool c = prepareSqlPool
   <$> createSqlitePoolFromInfo (getCInfo c & sqlConnectionStr %~ processConnStr) (getPoolSize c)
  where
    getCInfo (SqliteConf db _) = mkSqliteConnectionInfo $ "file:" <> db
    getCInfo (SqliteConfInfo ci _) = ci
    getPoolSize (SqliteConf _ n) = n
    getPoolSize (SqliteConfInfo _ n) = n
#endif



-- Modify resource at the moment of its creation.
-- Very ugly workaround, but seems to work. 
prepareSqlPool :: IsSqlBackend backend
               => Pool backend -> Pool backend
#if EXPO
prepareSqlPool origPool = $(do
    -- I know Pool data type has one constructor and its first field is "create"
    -- this is what I am looking for.
    (TyConI (DataD _ _ _ _ [RecC _ ((createN,_,_):_)] _)) <- reify ''Pool
    recUpdE
      [e|origPool|]
      [(,) createN <$> [e| updateCreate ($(varE createN) origPool) |] ]
  )
  where
    updateCreate = fmap (Unsafe.unsafeCoerce mkBackendReadOnly)
#else
prepareSqlPool = id
#endif


#if EXPO
-- Try to make the database read-only by modifying a backend object.
mkBackendReadOnly :: SqlBackend -> SqlBackend
mkBackendReadOnly bk = bk
    { connInsertSql = const $ const $ Unsafe.unsafePerformIO roError
    , connUpsertSql = Nothing
    , connInsertManySql = Nothing
    , connMigrateSql = const $ const $ const roError
    -- , connBegin = const roError
    -- , connCommit = const roError
    -- , connRollback = const roError
    }
  where
    roError = ioError $ mkIOError
      illegalOperationErrorType
      "Attempted to write into the database using read-only connection"
      Nothing Nothing
#endif


processConnStr :: Text -> Text
#if EXPO
processConnStr cs = combineConnStr base (readOnlyMode <> pams)
  where
    (base, pams) = splitConnStr cs
#else
processConnStr = id
#endif

-- | Setting a connection string parameter for a read-only database.
readOnlyMode :: Map Text Text
#if POSTGRESQL
-- this is not working, because libpq support only 'any' (default) or 'read-write'
readOnlyMode = Map.singleton "target_session_attrs" "read-only"
#else
readOnlyMode = Map.singleton "mode" "ro"
#endif

--   Assumes that the question marks in the file name are escaped and parameters do not have '&' symbol unescaped.
splitConnStr :: Text -> (Text, Map Text Text)
splitConnStr s = (base, pams)
  where
    (base, pamsStr) = drop 1 <$> Text.breakOn "?" s
    pamsLst = Text.splitOn "&" pamsStr
    pams = Map.fromList $ fmap (drop 1) . Text.breakOn "=" <$> pamsLst

-- Assemble a base and params into a connection string uri
combineConnStr :: Text -> Map Text Text -> Text
combineConnStr base pams
    | null pams = base
    | otherwise = base <> "?" <> Text.intercalate "&" (kv <$> Map.toList pams)
  where
    kv (k, v) = k <> "=" <> v



-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PersistConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    , appLuciAddress            :: Maybe ClientSettings
    -- ^ Default address of luci to redirect WebSockets to
    , appLTICredentials         :: LTIProvider
    -- ^ LTI credentials (i.e. for edX authorization)
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        lhost <- o .:? "luci-host"
        lport <- o .:? "luci-port"
        let appLuciAddress = clientSettings <$> lport <*> (BS.pack <$> lhost)

        ltiKey    <- o .: "lti-key"
        ltiSecret <- o .: "lti-secret"
        let appLTICredentials = newLTIProvider (Text.encodeUtf8 ltiKey) (Text.encodeUtf8 ltiSecret)

        return AppSettings {..}


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlDB :: FilePath
#if POSTGRESQL
configSettingsYmlDB = "config/settingsPostgreSQL.yml"
#else
configSettingsYmlDB = "config/settingsSqlite.yml"
#endif

configSettingsYmlDBBS :: ByteString
#if POSTGRESQL
configSettingsYmlDBBS = $(embedFile "config/settingsPostgreSQL.yml")
#else
configSettingsYmlDBBS = $(embedFile "config/settingsSqlite.yml")
#endif

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id $ decodeEither'
    (configSettingsYmlBS <> configSettingsYmlDBBS)

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
