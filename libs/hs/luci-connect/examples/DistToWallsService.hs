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
module Main (main) where

import Foreign
import Luci.Connect
import Luci.Connect.Base
import Luci.Messages
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Geometry as G
import           Data.Geospatial
import           Data.LinearRing
import qualified Control.Lens as Lens


----------------------------------------------------------------------------------------------------

-- | entry point
main :: IO ()
main = runReallySimpleLuciClient () $
   processTo =$= processMessages =$= processFrom

-- | helper for parsing incoming messages
processTo :: Conduit LuciMessage (LuciProgram ()) Message
processTo = awaitForever $ \lmsg -> case parseMessage lmsg of
    Error s -> logWarnN $ "[Parsing header] " <> Text.pack s <> " Received: " <>  (showJSON . toJSON $ fst lmsg)
    Success m -> yield m

-- | helper for sending outgoing messages
processFrom :: Conduit Message (LuciProgram ()) (LuciProcessing Text LuciMessage)
processFrom = awaitForever $ yieldMessage . makeMessage

----------------------------------------------------------------------------------------------------


-- | The main program
processMessages :: Conduit Message (LuciProgram ()) Message
processMessages = do
  -- send a first message to register as a service
  yield registerMessage
  -- reply to all run requests
  awaitForever responseMsgs

-- | Respond to one message at a time
responseMsgs :: Message -> Conduit Message (LuciProgram ()) Message
-- Respond only to run messages with correct input
responseMsgs (MsgRun "DistanceToWalls" pams [pts]) | Just (Success scId) <- fromJSON <$> HashMap.lookup "ScID" pams = liftIO (deserializePoints pts) >>= evaluate scId
-- Log luci errors
responseMsgs (MsgError _ s) = logWarnN $ "[Luci error message] " <> s
responseMsgs msg = logInfoN . ("[Ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg


evaluate :: Int -> [G.Vector3 Float] -> Conduit Message (LuciProgram ()) Message
evaluate scId pts = do
  logInfoN "***Received a task***"
  mscenario <- obtainScenario scId
  case mscenario of
    Nothing -> yield . MsgError Nothing $ "Could not get scenario from luci (" <> Text.pack (show scId) <> ")"
    Just scenario -> do
      let segments = Lens.view (geofeatures . traverse . geometry . Lens.lens polygonLines const) scenario
          result = map (`distToClosest` segments) pts
      resultBytes <- liftIO $ serializeValules result
      yield $ resultMessage resultBytes
      logInfoN "***Sent results back****"
  where
    polygonLines ::  GeospatialGeometry -> [(G.Vector3 Float, G.Vector3 Float)]
    polygonLines NoGeometry = []
    polygonLines Point{} = []
    polygonLines MultiPoint{} = []
    polygonLines Line{} = []
    polygonLines MultiLine{} = []
    polygonLines (Polygon p) = concatMap (splitRings . map toVect) $ fromLinearRing <$>_unGeoPolygon p
    polygonLines (MultiPolygon p) = concatMap (splitRings . map toVect) $ fromLinearRing <$> concat (_unGeoMultiPolygon p)
    polygonLines (Collection xs) = xs >>= polygonLines
    splitRings :: [G.Vector3 Float] -> [(G.Vector3 Float, G.Vector3 Float)]
    splitRings (a:b:xs) = (a,b) : splitRings (b:xs)
    splitRings [_] = []
    splitRings [] = []
    toVect (a:b:c:_) = G.vector3 (realToFrac a) (realToFrac b) (realToFrac c)
    toVect [a,b] = G.vector3 (realToFrac a) (realToFrac b) 0
    toVect [a] = G.vector3 (realToFrac a) 0 0
    toVect [] = G.vector3 0 0 0

    distToClosest x (s:ss) = min (distToSegment x s) (distToClosest x ss)
    distToClosest _ []     = read "Infinity"


distToSegment :: G.Vector3 Float -> (G.Vector3 Float, G.Vector3 Float) -> Float
distToSegment x0 (x1,x2) = sqrt $ if between then d2 else min lu2 lv2
   where
     a = x2 - x1
     u = x0 - x1
     v = x2 - x0
     between = G.dot a u > 0 && G.dot a v > 0
     lu2 = G.dot u u
     lv2 = G.dot v v
     la2 = G.dot a a
     c = cross a u
     d2 = G.dot c c / la2

cross :: G.Vector3 Float -> G.Vector3 Float -> G.Vector3 Float
cross u v | (a,b,c) <- G.unpackV3 u
          , (x,y,z) <- G.unpackV3 v = G.vector3 (b*z - c*y) (c*x - a*z) (a*y - b*x)

-- | Try our best to get scenario by its ScID
obtainScenario :: Int -> ConduitM Message Message (LuciProgram ()) (Maybe (GeoFeatureCollection JSON.Object))
obtainScenario scId = yield (getScenarioMessage scId) >> waitForScenario
  where
    waitForScenario = do
      mmsg <- await
      case mmsg of
        -- end of input
        Nothing -> logWarnN "[Get scenario] Conduit returned end of input" >> return Nothing
        -- a proper result
        Just (MsgResult (Just LuciMsgInfo{lmiServiceName = "scenario.geojson.Get"}) (ServiceResult sc) _) ->
            case JSON.fromJSON <$> (HashMap.lookup "geometry_output" sc >>= lookupVal "geometry") of
               Nothing          -> logWarnN ("[Get scenario] Cannot find (result.geometry_output.geometry). Result was: " <> showJSON (JSON.Object sc))
                                >> return Nothing
               Just (Error s)   -> logWarnN ("[Get scenario] Not a FeatureCollection: " <> Text.pack s <> " Result was: " <> showJSON (JSON.Object sc))
                                >> return Nothing
               Just (Success g) -> return $ Just g
        -- Luci error
        Just (MsgError _ err) -> logWarnN ("[Get scenario message error] " <> err) >> return Nothing
        -- keep waiting for message
        Just msg -> (logInfoN . ("[Get scenario - ignore Luci message] " <>) . showJSON . toJSON . fst $ makeMessage msg)
            >> waitForScenario
    lookupVal s (JSON.Object o) = HashMap.lookup s o
    lookupVal _ _ = Nothing


-- | convert BytesString to vector of 3D points
deserializePoints :: ByteString -> IO [G.Vector3 Float]
deserializePoints (BSI.PS fptr off len) = withForeignPtr fptr $ \ptr -> mapM (\i -> do
    x <- peekByteOff ptr i
    y <- peekByteOff ptr (i + 4)
    z <- peekByteOff ptr (i + 8)
    return (G.vector3 x y z)) [off,off+12..off+len-12]

-- | convert a list of floating points to a bytestring
serializeValules :: [Float] -> IO ByteString
serializeValules xs = BSI.create (length xs * 4) $ \ptr -> mapM_ (uncurry $ pokeByteOff ptr) $ zip [0,4..] xs


showJSON :: JSON.Value -> Text
showJSON = LText.toStrict . LText.decodeUtf8 . encode

----------------------------------------------------------------------------------------------------

-- | A message we send to register in luci
registerMessage :: Message
registerMessage = MsgRun "RemoteRegister" o []
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
          [ "mode"   .= String "string"
          , "unit"   .= String "string"
          , "values" .= String "attachment"
          ]
      , "constraint"         .= object
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
resultMessage :: ByteString -> Message
resultMessage bs = MsgResult Nothing (ServiceResult o) [bs]
  where
    o = HashMap.fromList
      [ "mode"   .= String "points"
      , "unit"   .= String "meters?"
      , "values" .= makeAReference bs "Float32Array" 1 Nothing
      ]

-- | Ask luci for a scenario
getScenarioMessage :: Int -> Message
getScenarioMessage scId = MsgRun "scenario.geojson.Get" o []
  where
    o = HashMap.fromList [ "ScID" .= scId ]
