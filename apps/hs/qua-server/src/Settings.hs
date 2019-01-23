{-# OPTIONS_HADDOCK hide, prune #-}
{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Data.Function ((&))
import ClassyPrelude.Yesod
import Control.Lens ((%~))
import qualified Control.Exception as Exception (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Data.Pool                   (Pool)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql        (IsSqlBackend)
#if POSTGRESQL
import Database.Persist.Postgresql (PostgresConf (..), createPostgresqlPool)
#else
import Database.Persist.Sqlite     (SqliteConf (..), createSqlitePoolFromInfo, sqlConnectionStr, mkSqliteConnectionInfo)
#endif
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
--import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified Data.ByteString.Char8 as BS
import Data.Conduit.Network
import Web.LTI

-- Go with Sqlite on development, and Postgres on production
#if POSTGRESQL
type PersistConf = PostgresConf
#else
type PersistConf = SqliteConf
#endif


isExpo :: Bool
#if EXPO
isExpo = True
#else
isExpo = False
#endif

isPostgres :: Bool
#if POSTGRESQL
isPostgres = True
#else
isPostgres = False
#endif

isDev :: Bool
#if DEVELOPMENT
isDev = True
#else
isDev = False
#endif

createAppSqlPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, IsSqlBackend backend)
                 => PersistConf -> m (Pool backend)
#if POSTGRESQL
createAppSqlPool c = createPostgresqlPool (pgConnStr c) (pgPoolSize c)
#else
createAppSqlPool (SqliteConf db ps) = createSqlitePoolFromInfo (mkSqliteConnectionInfo $ "file:" <> db <> "?" <> askReadOnly) ps
createAppSqlPool (SqliteConfInfo ci ps) = createSqlitePoolFromInfo (ci & sqlConnectionStr %~ perhapsReadOnly) ps
#endif

askReadOnly :: Text
#if POSTGRESQL
askReadOnly = "target_session_attrs=read-only"
#else
askReadOnly = "mode=ro"
#endif

perhapsReadOnly :: Text -> Text
#if EXPO
perhapsReadOnly s = case unsnoc start of
    Just (_, '?') -> start <> askReadOnly <> end
    Just (_, '&') -> start <> askReadOnly <> end
    _             -> start <> "?" <> askReadOnly
  where
    (start, end) = drop 1 . snd . Text.breakOn "&" <$> Text.breakOn "mode=" s
#else
perhapsReadOnly = id
#endif


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
