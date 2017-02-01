{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foreign (withForeignPtr, peekByteOff, pokeByteOff)
import Luci.Connect
import Luci.Messages
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Conduit
import Data.Semigroup
--import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import Control.Lens.Operators ((%%=), (%=))

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
    listScenarios conn >>= print
    getScenario conn 2 >>= print
    void $ runReallySimpleLuciClient (ServiceState 0 0 conn) processMessages
  where
    sets = PSSettings
      { uName  = "siren"
      , uPass  = "sirenpass"
      , dbHost = "localhost"
      , dbPort = 5432
      , dbName = "sirendb"
      }



----------------------------------------------------------------------------------------------------


-- | The main program
processMessages :: Conduit Message (LuciProgram ServiceState) Message
processMessages = do
  -- send a first message to register as a service
  regMsgToken <- genToken
  yield $ registerMessage regMsgToken
  -- reply to all run requests
  awaitForever responseMsgs

-- | Respond to one message at a time
responseMsgs :: Message -> Conduit Message (LuciProgram ServiceState) Message
-- Respond only to run messages with correct input
responseMsgs (MsgRun token "scenario.geojson.Get" pams [pts])
  | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = do
    currentRunToken %= const token
    conn <- _connection <$> get
    escenarioBS <- liftIO $ getScenario conn scId
    yield $ case escenarioBS of
      Left errbs -> MsgError token (Text.decodeUtf8 errbs)
      Right scenarioBS -> resultScenarioGet token scenarioBS
  --  liftIO (deserializePoints pts) >>= evaluate scId
-- {\"result\":{\"lastmodified\":1485936144,\"geometry_output\":{\"format\":\"geojson\",\"name\":\"My great scenario\",\"geometry\":{\"features\":[{\"geometry\"


responseMsgs (MsgRun token _ _ _) = do
    currentRunToken %= const token
    yield $ MsgError token "Incorrect input in the 'run' message."
-- Log luci errors

responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg



----------------------------------------------------------------------------------------------------

-- | A message we send to register in luci
registerMessage :: Token -> Message
registerMessage token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Show the distance to the closest building line"
      , "serviceName"        .= String "DistanceToWalls"
      , "qua-view-compliant" .= Bool True
      , "inputs"             .= object
          [ "ScID"   .= String "number"
          , "mode"   .= String "string"
          , "points" .= String "attachment"
          ]
      , "outputs"            .= object
          [ "units"  .= String "string"
          , "values" .= String "attachment"
          ]
      , "constraints"         .= object
          [ "mode" .= ["points" :: Text]
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "DistanceToWalls"
          , "ScId"   .= Number 1
          , "mode"   .= String "points"
          , "points" .= object
            [ "format"     .= String "Float32Array"
            ]
          ]
      ]


-- | This is what we return to Luci
resultScenarioGet :: Token -> ByteString -> Message
resultScenarioGet t bs = MsgResult t (ServiceResult o) []
  where
    o = HashMap.fromList -- "lastmodified\":1485936144,\"geometry_output\":{\"format\":\"geojson\",\"name\":\"My great scenario\",\"geometry\":{\"features\":[{\"geometry\"
      [ "lastmodified"     .= Number 1485936144
      , "geometry_output"  .= Object
        ( HashMap.fromList
          [ "format" .= String "geojson"
          , "name"   .= String "this is a name"
          , "geometry" .= fromMaybe Null (JSON.decodeStrict' bs)
          ]
        )
      ]

-- | Ask luci for a scenario
getScenarioMessage :: Token -> Int -> Message
getScenarioMessage token scId = MsgRun token "scenario.geojson.Get" o []
  where
    o = HashMap.fromList [ "ScID" .= scId ]
