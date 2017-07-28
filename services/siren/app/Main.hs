{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

-- import Foreign (withForeignPtr, peekByteOff, pokeByteOff)
import Luci.Connect
import Luci.Messages
import Luci.Connect.Base
import Control.Monad (void)
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Conduit
import Data.Int
import Data.Semigroup
--import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import Control.Lens.Operators ((%=), (<+=))

import           Data.List (stripPrefix)
-- import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char

import           System.Environment (getArgs)
-- import Lib.ParseGeoJSON
-- import Lib.Scenario
-- import Numeric.EasyTensor
-- import Numeric.Commons


import           Lib


----------------------------------------------------------------------------------------------------

data ServiceState = ServiceState
  { _tokenGen    :: !Token
  , _connection  :: !Connection
  , _subscribers :: !(HashMap Int64 [Token])
  }


Lens.makeLenses ''ServiceState

genToken :: MonadState ServiceState m => m Token
genToken = tokenGen <+= 1


-- | entry point
main :: IO ()
main = withPostgres sets $ \conn ->
    void $ runLuciClient (ServiceState 0 conn HashMap.empty) processMessages
  where
    sets = PSSettings
      { uName  = "siren"
      , uPass  = "sirenpass"
      , dbHost = "localhost"
      , dbPort = 5432
      , dbName = "sirendb"
      }

data RunSettings = RunSettings
  { hostS :: ByteString
  , portS :: Int
  , logLevelS :: LogLevel
  }

defaultRunSettings :: RunSettings
defaultRunSettings = RunSettings
  { hostS = "localhost"
  , portS = 7654
  , logLevelS = LevelInfo
  }



----------------------------------------------------------------------------------------------------


-- | The main program
processMessages :: Conduit Message (LuciProgram ServiceState) ByteString
processMessages = do
  -- send a first message to register as a service
  genToken >>= yield . headerBytes . registerGetList
  genToken >>= yield . headerBytes . registerGetScenario
  genToken >>= yield . headerBytes . registerCreateScenario
  genToken >>= yield . headerBytes . registerUpdateScenario
  genToken >>= yield . headerBytes . registerDeleteScenario
  genToken >>= yield . headerBytes . registerRecoverScenario
  genToken >>= yield . headerBytes . registerSubscribeTo

  -- reply to all run requests
  awaitForever responseMsgs

-- | Respond to one message at a time
responseMsgs :: Message -> Conduit Message (LuciProgram ServiceState) ByteString

-- Respond only to run messages with correct input

responseMsgs (MsgRun token "scenario.GetList" pams _)
  | mUserId <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-id" pams
  , mAuthRole <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-role" pams = do
    conn <- Lens.use connection
    eresultBS <- liftIO $ listScenarios conn (fromIntegral token) mUserId mAuthRole
    yieldAnswer token eresultBS

responseMsgs (MsgRun token "scenario.geojson.Get" pams _)
  | Just (Success scID) <- fromJSON <$> HashMap.lookup "ScID" pams = do
    conn <- Lens.use connection
    eresultBS <- liftIO $ getScenario conn (fromIntegral token) (ScenarioId scID)
    yieldAnswer token eresultBS

responseMsgs (MsgRun token "scenario.geojson.Create" pams _)
  | Just (Success scName) <- fromJSON <$> HashMap.lookup "name" pams
  , Just geom_input <- BSL.toStrict . JSON.encode <$> HashMap.lookup "geometry_input" pams
  , mUserId <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-id" pams
  , mAuthRole <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-role" pams = do
    conn <- Lens.use connection
    eresultBS <- liftIO $ createScenario conn (fromIntegral token) mUserId mAuthRole (BSC.pack scName) geom_input
    yieldAnswer token eresultBS

responseMsgs (MsgRun token "scenario.geojson.Update" pams _)
  | Just (Success scID) <- fromJSON <$> HashMap.lookup "ScID" pams
  , Just geom_input <- BSL.toStrict . JSON.encode <$> HashMap.lookup "geometry_input" pams
  , mUserId <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-id" pams
  , mAuthRole <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-role" pams = do
    conn <- Lens.use connection
    eresultBS <- liftIO $ updateScenario conn (fromIntegral token)  mUserId mAuthRole (ScenarioId scID) geom_input
    yieldAnswer token eresultBS
    -- send update to all subscribers
    subTokens <- map fromIntegral . fromMaybe [] . HashMap.lookup scID <$> Lens.use subscribers
    eGetMsgs <- liftIO $ getLastScUpdates conn subTokens (ScenarioId scID)
    case eGetMsgs of
      Left errbs -> logWarnN $ Text.decodeUtf8 errbs
      Right rezs -> mapM_ yield rezs

responseMsgs (MsgRun token "scenario.geojson.Delete" pams _)
  | Just (Success scID) <- fromJSON <$> HashMap.lookup "ScID" pams
  , mUserId <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-id" pams
  , mAuthRole <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-role" pams = do
      conn <-  Lens.use connection
      eresultBS <- liftIO $ deleteScenario conn (fromIntegral token) mUserId mAuthRole (ScenarioId scID)
      yieldAnswer token eresultBS

responseMsgs (MsgRun token "scenario.geojson.Recover" pams _)
  | Just (Success scID) <- fromJSON <$> HashMap.lookup "ScID" pams
  , mUserId <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-id" pams
  , mAuthRole <- (resultToMaybe . fromJSON) =<< HashMap.lookup "user-role" pams = do
    conn <- Lens.use connection
    eresultBS <- liftIO $ recoverScenario conn (fromIntegral token) (ScenarioId scID) mUserId mAuthRole
    yieldAnswer token eresultBS

responseMsgs (MsgRun token "scenario.SubscribeTo" pams _)
  | Just (Success scIDs) <- fromJSON <$> HashMap.lookup "ScIDs" pams = do
    subscribers %= flip (foldr (HashMap.alter addSubscriber)) (scIDs :: [Int64])
    yield . headerBytes $ MsgProgress token 0 Nothing []
  where
    addSubscriber Nothing = Just [token]
    addSubscriber (Just xs) = Just (token:xs)

responseMsgs (MsgRun token _ _ _) = yield . headerBytes $ MsgError token
    "Failed to understand scenario service request: incorrect input in the 'run' message."

responseMsgs (MsgCancel token) =
    -- remove a subscriber with this token
    subscribers %= HashMap.map (filter (token /=))

responseMsgs (MsgError token s) = do
    -- remove a subscriber with this token
    subscribers %= HashMap.map (filter (token /=))
    -- log a little
    logWarnN $ "[Luci error message] " <> s

responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe (Error _) = Nothing


----------------------------------------------------------------------------------------------------


-- | A message we send to register in luci
registerGetList :: Token -> Message
registerGetList token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Get list of available scenarios"
      , "serviceName"        .= String "scenario.GetList"
      , "inputs"             .= object []
      , "outputs"            .= object
          [ "scenarios"  .= String "list" ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.GetList" ]
      ]


-- | A message we send to register in luci
registerGetScenario :: Token -> Message
registerGetScenario token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Get a scenario in GeoJSON format"
      , "serviceName"        .= String "scenario.geojson.Get"
      , "inputs"             .= object
          [ "ScID" .= String "number" ]
      , "outputs"            .= object
          [ "created" .= String "number"
          , "lastmodified" .= String "number"
          , "geometry_output" .= object
            [ "format"      .= String "'GeoJSON'"
            , "name"        .= String "string"
            , "geometry"    .= String "{FeatureCollection}"
            , "properties"  .= String "object"
            ]
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.geojson.Get"
          , "ScId"   .= Number 1
          ]
      ]


-- | A message we send to register in luci
registerCreateScenario :: Token -> Message
registerCreateScenario token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Create a new scenario in GeoJSON format"
      , "serviceName"        .= String "scenario.geojson.Create"
      , "inputs"             .= object
          [ "name" .= String "string"
          , "geometry_input" .= object
            [ "geometry" .= String "FeatureCollection"
            , "OPT A: lat" .= String "number"
            , "OPT A: lon" .= String "number"
            , "OPT A: alt" .= String "number"
            , "OPT B: srid" .= String "number"
            , "properties" .= String "object"
            ]
          ]
      , "outputs"            .= object
          [ "ScID" .= String "number"
          , "created" .= String "number"
          , "lastmodified" .= String "number"
          , "name" .= String "name"
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.geojson.Create"
          , "name"   .= String "My First Scenario"
          , "geometry_input" .= object
            [ "geometry" .= object
              [ "type" .= String "FeatureCollection"
              , "features" .= [object [ "type" .= String "Feature", "geometry" .= object []]]
              , "lat"  .= Number 12.162
              , "lon"  .= Number 8.222
              , "alt"  .= Number 1200
              ]
            ]
          ]
      ]


-- | A message we send to register in luci
registerDeleteScenario :: Token -> Message
registerDeleteScenario token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Delete a scenario (mark it as dead to make it invisible)."
      , "serviceName"        .= String "scenario.geojson.Delete"
      , "inputs"             .= object
          [ "ScID" .= String "number"
          ]
      , "outputs"            .= object
          [ "ScID" .= String "number"
          , "created" .= String "number"
          , "lastmodified" .= String "number"
          , "name" .= String "name"
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.geojson.Delete"
          , "ScID"   .= Number 3
          ]
      ]

-- | A message we send to register in luci
registerRecoverScenario :: Token -> Message
registerRecoverScenario token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Recover a scenario (mark it as alive to make it visible)."
      , "serviceName"        .= String "scenario.geojson.Recover"
      , "inputs"             .= object
          [ "ScID" .= String "number"
          ]
      , "outputs"            .= object
          [ "ScID" .= String "number"
          , "created" .= String "number"
          , "lastmodified" .= String "number"
          , "name" .= String "name"
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.geojson.Recover"
          , "ScID"   .= Number 3
          ]
      ]

-- | A message we send to register in luci
registerUpdateScenario :: Token -> Message
registerUpdateScenario token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Update a scenario in GeoJSON format"
      , "serviceName"        .= String "scenario.geojson.Update"
      , "inputs"             .= object
          [ "ScID" .= String "number"
          , "geometry_input" .= object
            [ "geometry" .= String "FeatureCollection"
            , "properties" .= String "object"
            ]
          ]
      , "outputs"            .= object
          [ "ScID" .= String "number"
          , "created" .= String "number"
          , "lastmodified" .= String "number"
          , "name" .= String "name"
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.geojson.Update"
          , "ScID"   .= Number 3
          , "geometry_input" .= object
            [ "geometry" .= object
              [ "type" .= String "FeatureCollection"
              , "features" .= [object [ "type" .= String "Feature", "geometry" .= object []]]
              ]
            ]
          ]
      ]

-- | A message we send to register in luci
registerSubscribeTo :: Token -> Message
registerSubscribeTo token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= Text.unlines
              [ "Subscribe to all changes in listed scenarios"
              , "Note, this service never returns; instead, it send all updates in 'progress' messages"
              ]
      , "serviceName"        .= String "scenario.SubscribeTo"
      , "nonBlocking"        .= Bool True
      , "inputs"             .= object
          [ "ScIDs" .= String "[ScID]"
          ]
      , "outputs"            .= object
          [ "created" .= String "number"
          , "lastmodified" .= String "number"
          , "geometry_output" .= object
            [ "format"      .= String "'GeoJSON'"
            , "name"        .= String "string"
            , "geometry"    .= String "{FeatureCollection}"
            , "properties"  .= String "object"
            ]
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "scenario.SubscribeTo"
          , "ScIDs"  .= [Number 3, Number 6]
          ]
      ]



--------------------------------------------------------------------------------

headerBytes :: Message -> ByteString
headerBytes = BSL.toStrict . JSON.encode . fst . makeMessage



yieldAnswer :: Token
            -> Either ByteString ByteString
            -> Conduit Message (LuciProgram ServiceState) ByteString
yieldAnswer token eresultBS = yield $ case eresultBS of
  Left errbs -> headerBytes $ MsgError token (Text.decodeUtf8 errbs)
  Right resultBS -> resultBS

--------------------------------------------------------------------------------

runLuciClient :: s
              -- ^ Initial state of a program set by user.
              -> Conduit Message (LuciProgram s) ByteString
              -- ^ How to process LuciMessage
              -> IO s
              -- ^ Program runs and in the end returns a final state.
runLuciClient s0 pipe = do
    sets <- setSettings defaultRunSettings <$> getArgs
    putStrLn "[Info] SERVICE START - Running service with following command-line arguments:"
    putStrLn $ "\tport=" ++ show (portS sets)
    putStrLn $ "\thost=" ++ BSC.unpack (hostS sets)
    putStrLn $ "\tloglevel=" ++ showLevel (logLevelS sets)
    runLuciProgram s0 (logLevelS sets) $
          talkToLuci (portS sets)
                     (hostS sets)
                      Nothing
                     (logWarnNS "PIPELINE ERROR" . Text.pack . show)
                     (processTo =$= pipe =$= processFrom)
  where
    showLevel (LevelOther l) = Text.unpack l
    showLevel lvl = map Char.toLower . drop 5 $ show lvl
    setSettings s [] = s
    setSettings s (par:xs) | Just x <- stripPrefix "port="     par = setSettings s{portS = read x} xs
                           | Just x <- stripPrefix "host="     par = setSettings s{hostS = BSC.pack x} xs
                           | Just x <- stripPrefix "loglevel=" par = case x of
                                 "debug"   -> setSettings s{logLevelS = LevelDebug} xs
                                 "info"    -> setSettings s{logLevelS = LevelInfo} xs
                                 "warn"    -> setSettings s{logLevelS = LevelWarn} xs
                                 "warning" -> setSettings s{logLevelS = LevelWarn} xs
                                 "error"   -> setSettings s{logLevelS = LevelError} xs
                                 h -> setSettings s{logLevelS = LevelOther $ Text.pack h} xs
                           | otherwise = setSettings s xs


-- | helper for parsing incoming messages
processTo :: Conduit LuciMessage (LuciProgram s) Message
processTo = awaitForever $ \lmsg -> case parseMessage lmsg of
    JSON.Error s -> logWarnN $ "[Parsing header] " <> Text.pack s <> " Received: " <>  (showJSON . toJSON $ fst lmsg)
    JSON.Success m -> yield m

-- | helper for sending outgoing messages
processFrom :: Conduit ByteString (LuciProgram s) (LuciProcessing Text LuciMessage)
processFrom = awaitForever $ yieldRawMessage . flip (,) []
