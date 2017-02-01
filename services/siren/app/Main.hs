{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

-- import Foreign (withForeignPtr, peekByteOff, pokeByteOff)
import Luci.Connect
import Luci.Messages
import Luci.Connect.Base
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Conduit
import Data.Semigroup
--import Data.Monoid ((<>))
-- import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import Control.Lens.Operators ((%%=), (%=))

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
  { _currentRunToken :: Token
  , _tokenGen :: Token
  , _connection :: Connection
  }

Lens.makeLenses ''ServiceState

genToken :: MonadState ServiceState m => m Token
genToken = tokenGen %%= (id &&& (+1))



-- | entry point
main :: IO ()
main = withPostgres sets $ \conn -> do
    listScenarios conn 14 >>= print
    getScenario conn 2 >>= print
    void $ runLuciClient (ServiceState 0 0 conn) processMessages
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
  regMsgToken <- genToken
  yield . headerBytes $ registerGetList regMsgToken
  -- reply to all run requests
  awaitForever responseMsgs

-- | Respond to one message at a time
responseMsgs :: Message -> Conduit Message (LuciProgram ServiceState) ByteString

-- Respond only to run messages with correct input

responseMsgs (MsgRun token "scenario.GetList" _ _) = do
  conn <- _connection <$> get
  eresultBS <- liftIO $ listScenarios conn (fromIntegral token)
  yield $ case eresultBS of
    Left errbs -> headerBytes $ MsgError token (Text.decodeUtf8 errbs)
    Right resultBS -> resultBS


-- responseMsgs (MsgRun token "scenario.geojson.Get" pams [pts])
--   | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = do
--     currentRunToken %= const token
--     conn <- _connection <$> get
--     escenarioBS <- liftIO $ getScenario conn scId
--     yield $ case escenarioBS of
--       Left errbs -> headerBytes $ MsgError token (Text.decodeUtf8 errbs)
--       Right scenarioBS -> undefined -- resultScenarioGet token scenarioBS
--   --  liftIO (deserializePoints pts) >>= evaluate scId
-- -- {\"result\":{\"lastmodified\":1485936144,\"geometry_output\":{\"format\":\"geojson\",\"name\":\"My great scenario\",\"geometry\":{\"features\":[{\"geometry\"



responseMsgs (MsgRun token _ _ _) = do
    currentRunToken %= const token
    yield . headerBytes $ MsgError token "Incorrect input in the 'run' message."
-- Log luci errors

responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg



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

-- -- | A message we send to register in luci
-- registerMessage :: Token -> Message
-- registerMessage token = MsgRun token "RemoteRegister" o []
--   where
--     o = HashMap.fromList
--       [ "description"        .= String "Show the distance to the closest building line"
--       , "serviceName"        .= String "DistanceToWalls"
--       , "qua-view-compliant" .= Bool True
--       , "inputs"             .= object
--           [ "ScID"   .= String "number"
--           , "mode"   .= String "string"
--           , "points" .= String "attachment"
--           ]
--       , "outputs"            .= object
--           [ "units"  .= String "string"
--           , "values" .= String "attachment"
--           ]
--       , "constraints"         .= object
--           [ "mode" .= ["points" :: Text]
--           ]
--       , "exampleCall"        .= object
--           [ "run"    .= String "DistanceToWalls"
--           , "ScId"   .= Number 1
--           , "mode"   .= String "points"
--           , "points" .= object
--             [ "format"     .= String "Float32Array"
--             ]
--           ]
--       ]




headerBytes :: Message -> ByteString
headerBytes = BSL.toStrict . JSON.encode . fst . makeMessage


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
