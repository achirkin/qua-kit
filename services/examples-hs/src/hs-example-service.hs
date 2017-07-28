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
import           Data.FileEmbed
import qualified Data.Text as Text
import Data.Conduit
import Data.Word
-- import qualified Data.Vector as Vector
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
responseMsgs (MsgRun token _ _ "hs-example-service" pams atts)
  | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = do
    currentRunToken %= const token
    case fromJSON <$> HashMap.lookup "mode" pams of
      Nothing -> yield $ MsgError token "Cannot find execution mode field."
      Just (Error s) -> yield $ MsgError token ("Cannot parse execution mode field: " <> Text.pack s)
      Just (Success "points") -> case atts of
                                   [pts] -> liftIO (deserializePoints pts) >>= evaluatePoints scId
                                   _ -> yield $ MsgError token "Incorrect number of attachments"
      Just (Success "scenario") -> evaluateScenario (fromJSON <$> HashMap.lookup "scenarioResultType" pams)
                                                    (fromJSON <$> HashMap.lookup "message" pams)
      Just (Success "objects") -> case atts of
                                   [gs] -> liftIO (deserializeGeomIds gs) >>= evaluateObjects scId
                                   _ -> yield $ MsgError token "Incorrect number of attachments"
      Just (Success m) -> yield $ MsgError token ("Execution mode " <> m <> " is not supported.")

responseMsgs (MsgRun token _ _ _ _ _) = do
    currentRunToken %= const token
    yield $ MsgError token "Incorrect input in the 'run' message."
-- Log luci errors
responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg


evaluateScenario :: Maybe (Result Text) -> Maybe (Result Text) -> Conduit Message (LuciProgram ServiceState) Message
evaluateScenario rezType mmsg = do
  token <- Lens.use currentRunToken
  case rezType of
    Nothing -> yield $ MsgError token "Cannot find scenarioResultType parameter"
    Just (Error e) -> yield $ MsgError token ("Cannot parse scenarioResultType field: " <> Text.pack e)
    Just (Success "text") -> case mmsg of
        Just (Success msg) -> yield $ MsgResult token (ServiceResult $ HashMap.fromList
                                        [ "answer"  .= String ("Your message was: " <> msg)
                                        ]) []
        _ -> yield $ MsgResult token (ServiceResult $ HashMap.fromList
                                        [ "answer"  .= String "Hi there, I am hs-example-service in echo mode."
                                        ]) []
    Just (Success "image") -> let img = $(embedFile "res/resultImg.png")
                              in yield $ MsgResult token (ServiceResult $ HashMap.fromList
                                    [ "image"  .= makeAReference img "image/png" 1 Nothing
                                    ]) [img]

    Just (Success s) -> yield $ MsgError token ("scenarioResultType '" <> s <> "' is not supported.")




evaluatePoints :: Int -> [Vec2f] -> Conduit Message (LuciProgram ServiceState) Message
evaluatePoints scId pts = do
  curToken <- Lens.use currentRunToken
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


evaluateObjects :: Int -> [Int] -> Conduit Message (LuciProgram ServiceState) Message
evaluateObjects scId geomIds = do
  curToken <- Lens.use currentRunToken
  logInfoN "***Received a task***"
  logDebugN $ Text.pack (show geomIds)
  logDebugN $ "***Asking for scenario " <> Text.pack (show scId) <> " ***"
  mscenario <- obtainScenario scId
  case mscenario of
    Nothing -> yield . MsgError curToken $ "Could not get scenario from luci (" <> Text.pack (show scId) <> ")"
    Just buildings -> do
      logDebugN "***Got scenario***"
      logDebugN $ Text.pack (show buildings)
      let pc = Lens.view rCenter buildings
          f p = unScalar $ normL2 (pc - p)
          parseI Nothing = 0
          parseI (Just val) = case fromJSON val of
              Error _ -> 0
              Success i -> i
          bmap = HashMap.fromList . map (\b -> ( parseI .  HashMap.lookup "geomID" . snd $ _content b , _bCenter b)) $ toList buildings
          result = (\gId -> maybe 0 f $ HashMap.lookup gId bmap) <$> geomIds :: [Float]
      logDebugN "***Result computed:***"
      logDebugN $ Text.pack (show result)
      resultBytes <- liftIO $ serializeValules result
      yield $ resultMessagePoints curToken resultBytes
      logInfoN "***Sent results back****"





-- | Try our best to get scenario by its ScID
obtainScenario :: Int -> ConduitM Message Message (LuciProgram ServiceState) (Maybe (Scenario GeoJSONProps))
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

deserializeGeomIds :: ByteString -> IO [Int]
deserializeGeomIds (BSI.PS fptr off len) = withForeignPtr fptr
  $ \ptr -> mapM (fmap (fromIntegral :: Word32 -> Int) . peekByteOff ptr) [off,off+4..off+len-4]


-- | convert a list of floating points to a bytestring
serializeValules :: [Float] -> IO ByteString
serializeValules xs = BSI.create (length xs * 4) $ \ptr -> mapM_ (uncurry $ pokeByteOff ptr) $ zip [0,4..] xs



----------------------------------------------------------------------------------------------------

-- | A message we send to register in luci
registerMessage :: Token -> Message
registerMessage token = MsgRun token Nothing Local "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= String "Haskell qua-comliant service example"
      , "serviceName"        .= String "hs-example-service"
      , "qua-view-compliant" .= Bool True
      , "inputs"             .= object
          [ "ScID"   .= String "number" -- in our case this parameter is always obligatory, so do not put OPT
          , "mode"   .= String "string" -- mode is obligatory always
          , "OPT points"  .= String "attachment" -- Optional, because required only for mode "points"
          , "OPT geomIDs" .= String "attachment" -- Optional, because required only for mode "objects"
          -- Further go several service parameters
          , "OPT message" .= String "string"
          -- Further go several service parameters
          , "OPT scenarioResultType" .= String "string"
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
          [ "mode" .= ["points", "objects", "scenario" :: Text] -- , "new"
          , "scenarioResultType" .= ["text", "image" :: Text]
          ]
      , "exampleCall"        .= object
          [ "run"    .= String "hs-example-service"
          , "ScId"   .= Number 1
          , "mode"   .= String "points"
          , "points" .= object
            [ "format"     .= String "Float32Array"
            ]
          , "message"  .= String "Hello world!"
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
getScenarioMessage token scId = MsgRun token Nothing Local "scenario.geojson.Get" o []
  where
    o = HashMap.fromList [ "ScID" .= scId ]
