{-# OPTIONS_GHC -fno-warn-orphans #-}
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


import Data.Time.Clock (DiffTime)
import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sql
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

import qualified Data.ByteString as BS (readFile)
--import qualified Data.ByteString.Base64 as BSB (encode)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.About
import Handler.Feedback
import Handler.Home
import Handler.LuciProxy
import Handler.QuaViewSettings
import Handler.Mooc
import Handler.Mooc.Criteria
import Handler.Mooc.RenameMe
import Handler.Mooc.Admin
import Handler.Mooc.Comment
import Handler.Mooc.Scenario
import Handler.Mooc.BrowseProposals
import Handler.Mooc.EditProposal
import Handler.Mooc.ViewProposal
import Handler.Mooc.SubmitProposal
import Handler.Mooc.CompareProposals
import Handler.Mooc.ProposalPreview
import Handler.Mooc.User
import Handler.LoggingWS


import Handler.Mooc.Tests

import Model.Rating

import qualified Data.Map.Strict as Map

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
    pool <- createPoolConfig $ appDatabaseConf appSettings

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    sctaskfile <- BS.readFile "static/data/mooctask.geojson"
    sctaskpreview <- BS.readFile "static/data/mooctask.png"
    criteriaIconSecurity      <- decodeUtf8 <$> BS.readFile "static/data/security.svg"
    criteriaIconFire          <- decodeUtf8 <$> BS.readFile "static/data/fire.svg"
    criteriaIconShading       <- decodeUtf8 <$> BS.readFile "static/data/shading.svg"
    criteriaIconAccessibility <- decodeUtf8 <$> BS.readFile "static/data/accessibility.svg"
    criteriaIconAesthetic     <- decodeUtf8 <$> BS.readFile "static/data/aesthetic.svg"
    flip runSqlPool pool $ do
      -- add dev sample problem
      repsert (toSqlKey 0) (ScenarioProblem sctaskpreview sctaskfile "Empower Shack scenario" 0.001)
      -- make me the first admin
      mme <- getBy $ ETHUserName (Just "achirkin")
      case mme of
        Nothing -> insert_ $ User "Artem Chirkin" UR_ADMIN (Just "achirkin") Nothing
        Just (Entity key _) -> update key [UserRole =. UR_ADMIN]
      -- add test criteria into DB
      _ <- upsert (Criterion "Security" "Avoid dark corners and keep all paths easily visually observable." sctaskpreview criteriaIconSecurity) []
      _ <- upsert (Criterion "Fire safety" "Keep distance between buildings or groups of buildings to reduce the chance of spreading fire." sctaskpreview criteriaIconFire) []
      _ <- upsert (Criterion "Shading" "The area exhibits hight temperatures, therefore it makes sense to keep pathways in shade." sctaskpreview criteriaIconShading) []
      _ <- upsert (Criterion "Accessibility" "All buildings must be accessible from outside, there should be no labyrinths in the discrict." sctaskpreview criteriaIconAccessibility) []
      _ <- upsert (Criterion "Aesthetic" "The district should look attractive." sctaskpreview criteriaIconAesthetic) []
      return ()

    -- | Update ratings once in an hour
    flip runSqlPool pool $ scheduleUpdateRatings 3600 reviewRating compareRating combR

    -- Return the foundation
    return $ mkFoundation pool
  where

    reviewRating :: [(DiffTime,NDCount,Bool)] -> Double
    reviewRating = min 1 . max 0 . (+0.4) . uncurry (/) . second (max 1) . foldl' (\(x, a) (_dt,dn,p) -> let d = vote dn in (if p then x + d else x - d, a+d)) (0,0)
      where
        vote = recip . fromIntegral . (+1)

    compareRating :: [(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)]
    compareRating xs = Map.toList $ iterN 10 naiveMap
      where
        -- cumsum
        nn = recip $ foldl' (\s (_, (_,dn1), (_,dn2)) -> s + vote dn1 + vote dn2) 0 xs / 2

        -- discount votes with nr of versions passed since last vote
        vote = recip . fromIntegral . (+1)

        -- at first, we naively count votes for and against each proposal
        updateHMNaive hm (_dt, (u1,dn1), (u2,dn2)) = Map.alter (Just . (+(- vote dn2)) . fromMaybe 0) u2 $ Map.alter (Just . (+vote dn1) . fromMaybe 0) u1 hm
        naiveMap :: Map.Map UserId Double
        naiveMap = normMap $ foldl' updateHMNaive Map.empty xs

        -- then we use this logic to iteratively come closer to solution
        updateR (ob,ow) (cb,cw) = let diff = log (1 + exp (ow-ob)) * nn
                                  in (cb + diff, cw - diff)

        iterN 0 hm = hm
        iterN n hm = hm `seq` iterN (n-1 :: Int) (updateAllHM hm)

        updateAllHM hm = normMap $ foldl' (updateHM hm) hm xs

        updateHM :: Map.Map UserId Double -> Map.Map UserId Double -> (DiffTime, (UserId, NDCount), (UserId, NDCount)) -> Map.Map UserId Double
        updateHM ohm hm (_, (u1,_), (u2,_)) = let ov1 = fromMaybe 0 $ Map.lookup u1 ohm
                                                  ov2 = fromMaybe 0 $ Map.lookup u2 ohm
                                                  v1 = fromMaybe 0 $ Map.lookup u1 hm
                                                  v2 = fromMaybe 0 $ Map.lookup u2 hm
                                                  (v1', v2') = updateR (ov1,ov2) (v1,v2)
                                              in Map.alter (const $ Just v1') u1 $ Map.alter (const $ Just v2') u2 hm

--        evaluateL :: [(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> Map.Map UserId Double -> Double
--        evaluateL ((_, (u1,dn1), (u2,dn2)):ss) hm = let v1 = fromMaybe 0 $ Map.lookup u1 hm
--                                                        v2 = fromMaybe 0 $ Map.lookup u2 hm
--                                                        l = (1 - signum (v1 - v2)) * (vote dn1 + vote dn2)
--                                                    in l + evaluateL ss hm
--        evaluateL [] _ = 0
--        rn = foldl' (\(a:ss) _ -> updateAllHM a : a : ss) [naiveMap,Map.empty] [1..10::Double]
--        ln = map (round . (100*) . evaluateL xs) $ reverse rn :: [Int]

        normMap hm = let (mi, ma) = Map.foldl' (\(l,u) v -> (min l v, max u v)) (0,0) hm
                         diff = max (ma-mi) 1
                     in Map.map (\v -> (v - mi) / diff) hm

    combR :: (Int, Int, Maybe Rating) -> (Int, Int, Maybe Rating) -> Rating
    combR (_,_,Nothing) (_,_,Nothing) = error "[combR] Impossible: both ratings not here."
    combR (n,i,Just (Rating pid cid uid v)) (_,_,Nothing) = Rating pid cid uid $ filterV n i v
    combR (_,_,Nothing) (m,j,Just (Rating pid cid uid v)) = Rating pid cid uid $ filterV m j v
    combR rv@(n,i,Just (Rating pid cid uid v)) ru@(m,j,Just (Rating _ _ _ u))
        | n >= minNn && i >= minNi = combR (n,i, Nothing) ru
        | m >= minNn && j >= minNi = combR rv (m,j, Nothing)
        | otherwise = let cv = fromIntegral $ i * m
                          cu = fromIntegral $ j * n
                      in Rating pid cid uid $ (cv * v + cu * u) / (cv + cu)
    filterV n i x = if n >= minNn && i >= minNi then x else 0

-- mininum number of votes for this particular design to participate
minNi :: Int
minNi = 2

-- minimum number of votes for a criterion to participate
minNn :: Int
minNn = 5


-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
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
