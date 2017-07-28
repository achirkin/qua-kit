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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import Data.Semigroup
--import Data.Monoid ((<>))
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
responseMsgs (MsgRun token _ _ "DistanceToWalls" pams [pts])
  | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = do
    currentRunToken %= const token
    liftIO (deserializePoints pts) >>= evaluate scId
responseMsgs (MsgRun token _ _ _ _ _) = do
    currentRunToken %= const token
    yield $ MsgError token "Incorrect input in the 'run' message."
-- Log luci errors
responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg


evaluate :: Int -> [Vec2f] -> Conduit Message (LuciProgram ServiceState) Message
evaluate scId pts = do
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
      yield $ resultMessage curToken resultBytes
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
registerMessage token = MsgRun token Nothing Local "RemoteRegister" o []
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
resultMessage :: Token -> ByteString -> Message
resultMessage t bs = MsgResult t (ServiceResult o) [bs]
  where
    o = HashMap.fromList
      [ "mode"   .= String "points"
      , "units"  .= String "meters?"
      , "values" .= makeAReference bs "Float32Array" 1 Nothing
      ]

-- | Ask luci for a scenario
getScenarioMessage :: Token -> Int -> Message
getScenarioMessage token scId = MsgRun token Nothing Local "scenario.geojson.Get" o []
  where
    o = HashMap.fromList [ "ScID" .= scId ]
