{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where


import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sql
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

-- Run grading simulation (long time proc).
-- Set to either 1 or 0 to switch it.
#ifndef SIMULATE_GRADING_LEARNING
#define SIMULATE_GRADING_LEARNING 0
#endif

#if EXPO
import Web.Browser
#else
#if SIMULATE_GRADING_LEARNING == 1
import Application.Grading (simulateGradingLearning)
#endif

#if DEVELOPMENT
-- automatically import some data to start with
import Application.SetupProblemData (importProblemData)
#else
-- schedule sending grades to edX from time to time
import Application.Edx (scheduleUpdateGrades)
#endif
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.About
import Handler.Feedback
import Handler.LuciProxy
import Handler.QuaViewReviewSettings
import Handler.QuaViewSettings
import Handler.Mooc
import Handler.Mooc.Analysis
import Handler.Mooc.Analysis.VoteOrder
import Handler.Mooc.Criteria
import Handler.Mooc.RenameMe
import Handler.Mooc.Admin
import Handler.Mooc.Admin.CriterionEditor
import Handler.Mooc.Admin.ReviewRequest
import Handler.Mooc.Admin.ExerciseEditor
import Handler.Mooc.Admin.UserManager
import Handler.Mooc.Admin.LuciScenarioDownloader
import Handler.Mooc.Reviews
import Handler.Mooc.ExpertReview
import Handler.Mooc.Exercise
import Handler.Mooc.BrowseProposals
import Handler.Mooc.CompareProposals
import Handler.Mooc.ProposalPreview
import Handler.Mooc.User
import Handler.Mooc.Survey
import Handler.Mooc.FAQ
import Handler.Submissions
import Handler.LoggingWS
#if DEVELOPMENT
import Handler.Mooc.Tests
#endif

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appWSChan <- newTChanIO

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- runLoggingT (createAppSqlPool $ appDatabaseConf appSettings) logFunc

#if EXPO
    let txtLine = "-------------------------------------------------------"
        txtBdrs = "--                                                   --"
        putStrInLine s = putStrLn $ "-- " <> s <> drop (length s + 3) txtBdrs
        myUrl = case appRoot appSettings of
          Just r -> unpack r
          Nothing -> "http://localhost:" <> show (appPort appSettings) <> "/"
    _ <- openBrowser myUrl
    putStrLn txtLine
    putStrInLine "Qua-kit is running now"
    putStrInLine ""
    putStrInLine "Open this page in a browser:"
    putStrInLine $ "        " <> pack myUrl
    putStrInLine ""
    putStrLn txtLine
#else
    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- register a single admin user if there are no users in the database
    registerAdmin pool

#if DEVELOPMENT
    -- Fill database with some problem-specific important data.
    importProblemData pool
#else
    -- send grades once every day
    scheduleUpdateGrades (3600*24) appSettings appHttpManager pool logFunc
#endif

#if SIMULATE_GRADING_LEARNING == 1
    -- run simulation of grading procedure
    runLoggingT (runResourceT $ runSqlPool simulateGradingLearning pool) logFunc
#endif
#endif
    return $ mkFoundation pool



-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ methodOverride $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
#if EXPO
-- disable request logging in expo mode
makeLogWare = const $ return id
#else
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }
#endif

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml,configSettingsYmlDB] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB


#if EXPO
#else
-- | Create a default admin user and set a password at the program startup.
--
--   username: "admin@qua-kit.hs"
--   password: "make it some random thing"
--     (no quotes in both cases)
registerAdmin :: ConnectionPool -> IO ()
registerAdmin pool = flip runSqlPool pool $ do
    userCount <- count ([] :: [Filter User])
    when (userCount <= 0) $ do
      -- make me the first admin
      let un = Just "admin@qua-kit.hs"
      mme' <- getBy $ UserEmailId un
      case mme' of
        Nothing ->
          let pw = Just "sha256|16|AK5Dd0IF3Hdkgywn506B5Q==|MxrRScUOlKp7dXOEGMpEYiiMvN/Us7S9XRKVXJnAQlg="
          in insert_ $ User
              "Qua-kit Super Admin" UR_ADMIN Nothing Nothing un pw True
        Just _ -> return ()
       --Just (Entity key _) -> update key [UserRole =. UR_ADMIN]
#endif
