-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- An implementation of a simple luci service.
--
-----------------------------------------------------------------------------
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
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import qualified Data.Vector as Vector
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import Control.Lens.Operators ((%%=), (%=))


import Lib.ParseGeoJSON
import Lib.Scenario
import Lib.Region
import Numeric.EasyTensor

----------------------------------------------------------------------------------------------------

data ServiceState = ServiceState
  { _currentRunToken :: Token
  , _tokenGen :: Token
  }

Lens.makeLenses ''ServiceState

genToken :: MonadState ServiceState m => m Token
genToken = tokenGen %%= (id &&& (+1))



-- | entry point
main :: IO ()
main = void $ runReallySimpleLuciClient (ServiceState 0 0) processMessages

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
responseMsgs (MsgRun token "Test multi-parameter" pams [pts])
  | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = do
    currentRunToken %= const token
    liftIO (deserializePoints pts) >>= evaluatePoints scId
responseMsgs (MsgRun token _ _ _) = do
    currentRunToken %= const token
    yield $ MsgError token "Incorrect input in the 'run' message."
-- Log luci errors
responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg


evaluatePoints :: Int -> [Vec2f] -> Conduit Message (LuciProgram ServiceState) Message
evaluatePoints scId pts = do
  curToken <- _currentRunToken <$> get
  logInfoN "***Received a task***"
  logDebugN $ Text.pack (show pts)
  logDebugN $ "***Asking for scenario " <> Text.pack (show scId) <> " ***"
  mscenario <- obtainScenario scId
  case mscenario of
    Nothing -> yield . MsgError curToken $ "Could not get scenario from luci (" <> Text.pack (show scId) <> ")"
    Just buildings -> do
      logDebugN "***Got scenario***"
      logDebugN $ Text.pack (show buildings)
      let result = map (\p -> case findClosest p buildings of
                                Nothing -> 0
                                (Just (Arg x _)) -> realToFrac x
                       ) pts
      logDebugN "***Result computed:***"
      logDebugN $ Text.pack (show result)
      resultBytes <- liftIO $ serializeValules result
      yield $ resultMessagePoints curToken resultBytes
      logInfoN "***Sent results back****"


-- | Try our best to get scenario by its ScID
obtainScenario :: Int -> ConduitM Message Message (LuciProgram ServiceState) (Maybe (Scenario ()))
obtainScenario scId = do
    scToken <- genToken
    yield (getScenarioMessage scToken scId)
    waitForScenario scToken
  where
    waitForScenario scToken = do
      mmsg <- await
      case mmsg of
        -- end of input
        Nothing -> logWarnN "[Get scenario] Conduit returned end of input" >> return Nothing
        -- a proper result
        Just msg@(MsgResult rtoken (ServiceResult sc) _) ->
          if rtoken /= scToken
          then do
            logInfoN . ("[Get scenario - ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg
            waitForScenario scToken
          else
            case parseGeoJSONValue <$> (HashMap.lookup "geometry_output" sc >>= lookupVal "geometry") of
               Nothing          -> logWarnN ("[Get scenario] Cannot find (result.geometry_output.geometry). Result was: " <> showJSON (JSON.Object sc))
                                >> return Nothing
               Just (Left s)   -> logWarnN ("[Get scenario] Not a FeatureCollection: " <> Text.pack s <> " Result was: " <> showJSON (JSON.Object sc))
                                >> return Nothing
               Just (Right g) -> return $ Just g
        -- Luci error
        Just (MsgError _ err) -> logWarnN ("[Get scenario message error] " <> err) >> return Nothing
        -- keep waiting for message
        Just msg -> do
           logInfoN . ("[Get scenario - ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg
           waitForScenario scToken
    lookupVal s (JSON.Object o) = HashMap.lookup s o
    lookupVal _ _ = Nothing


-- | convert BytesString to vector of 3D points
deserializePoints :: ByteString -> IO [Vec2f]
deserializePoints (BSI.PS fptr off len) = withForeignPtr fptr $ \ptr -> mapM (fmap unStore . peekByteOff ptr) [off,off+12..off+len-12]

-- | convert a list of floating points to a bytestring
serializeValules :: [Float] -> IO ByteString
serializeValules xs = BSI.create (length xs * 4) $ \ptr -> mapM_ (uncurry $ pokeByteOff ptr) $ zip [0,4..] xs



----------------------------------------------------------------------------------------------------

-- | A message we send to register in luci
registerMessage :: Token -> Message
registerMessage token = MsgRun token "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Service with many modes and parameters"
      , "serviceName"        .= String "Test multi-parameter"
      , "qua-view-compliant" .= Bool True
      , "inputs"             .= object
          [ "ScID"   .= String "number" -- in our case this parameter is always obligatory, so do not put OPT
          , "mode"   .= String "string" -- mode is obligatory always
          , "OPT points"  .= String "attachment" -- Optional, because required only for mode "points"
          , "OPT geomIDs" .= String "attachment" -- Optional, because required only for mode "objects"
          -- Further go several service parameters
          , "rating"      .= String "number"
          , "goodness"    .= String "number"
          , "OPT message" .= String "string"
          , "type"        .= String "string"
          , "joke"        .= String "boolean"
          ]
      , "outputs"            .= object
          [ "OPT units"   .= String "string"     -- Optional, because returned only for modes "points" and "objects"
          , "OPT values"  .= String "attachment" -- Optional, because returned only for modes "points" and "objects"
          , "OPT image"   .= String "attachment" -- .png Image; optional, because only for "scenario" mode.
                                                 -- Note, you should use only either image or answer in that mode.
          , "OPT ScID"    .= String "number"     -- Only makes sense for mode "new"
          , "OPT timestamp_accessed" .= String "number" -- Mode "new"
          , "OPT timestamp_modified" .= String "number" -- Mode "new"
          ]
      , "constraints"         .= object
          [ "mode" .= ["points", "objects", "scenario", "new" :: Text]
          , "rating" .= object
            [ "min" .= Number 1
            , "max" .= Number 5
            , "def" .= Number 3
            , "integer" .= Bool True
            ]
          , "goodness" .= object
            [ "min" .= Number 0
            , "def" .= Number 10
            , "integer" .= Bool False
            ]
          , "type" .= Array (Vector.fromList
                [ String "Medium" -- medium is a default type
                , String "Easy"
                , String "Hard"
                ])
          , "joke" .= object ["def" .= Bool True]
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "Test multi-parameter"
          , "ScId"   .= Number 1
          , "mode"   .= String "points"
          , "points" .= object
            [ "format"     .= String "Float32Array"
            ]
          , "rating"   .= Number 4
          , "goodness" .= Number 15.1
          , "message"  .= String "Hello world!"
          , "type"     .= String "Easy"
          , "joke"     .= Bool False
          ]
      ]

-- | This is what we return to Luci
resultMessagePoints :: Token -> ByteString -> Message
resultMessagePoints t bs = MsgResult t (ServiceResult o) [bs]
  where
    o = HashMap.fromList
      [ "units"  .= String "meters?"
      , "values" .= makeAReference bs "Float32Array" 1 Nothing
      ]

-- | Ask luci for a scenario
getScenarioMessage :: Token -> Int -> Message
getScenarioMessage token scId = MsgRun token "scenario.geojson.Get" o []
  where
    o = HashMap.fromList [ "ScID" .= scId ]
