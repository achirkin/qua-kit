-----------------------------------------------------------------------------
-- |
-- Module      :  Helen.Core.Service.Information
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Local service providing "serviceList", "serviceInfo" task.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Helen.Core.Service.Information
  ( infoService
  ) where

--import           Control.Monad (when, unless, foldM, forM_)
import qualified Control.Lens            as Lens
import           Data.Aeson              ((.=))
import qualified Data.Aeson              as JSON
import qualified Data.HashMap.Strict     as HashMap
import           Data.List               ((\\))
import qualified Data.List               as List (foldl', intersect, sort,
                                                  union)
import           Data.Maybe              (fromMaybe, maybeToList)
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText

import           Luci.Connect
import           Luci.Messages

import           Helen.Core.Types

infoService :: HelenWorld ()
infoService = do
  helen <- get
  -- connect as being an ordinary client
  (clientId, _) <- registerClient helen $ Client runInfoService ""
  -- register ServiceList
  sendMessage helen $ SourcedMessage clientId serviceListMessage
  -- register ServiceInfo
  sendMessage helen $ SourcedMessage clientId serviceInfoMessage
  -- register FilterServices
  sendMessage helen $ SourcedMessage clientId filterServicesMessage


runInfoService :: TargetedMessage -> HelenWorld ()

-- ServiceList
runInfoService (TargetedMessage _ myId (MsgRun token "ServiceList" _ _)) = do
  helen <- get
  let snames = HashMap.keys $ Lens.view (serviceManager.serviceMap) helen
  sendMessage helen $ SourcedMessage myId $ MsgResult token (
    resultJSON ["serviceNames" .= snames]
    ) []

-- ServiceInfo
runInfoService (TargetedMessage _ myId (MsgRun token "ServiceInfo" pams _)) = do
    helen <- get
    let allNames = HashMap.keys $ Lens.view (serviceManager.serviceMap) helen
        sNames = reqNames allNames
        sInfos = concatMap (\n@(ServiceName s) -> maybeToList . fmap ( (s .=) . _serviceInfo)
                    $ Lens.view (serviceManager . namedPool n) helen) sNames
    sendMessage helen $ SourcedMessage myId $ MsgResult token (resultJSON sInfos) []
  where
    reqNames xs = case JSON.fromJSON <$> HashMap.lookup "serviceNames" pams of
                 Just (JSON.Success l) -> List.sort xs `List.intersect` List.sort l
                 _ -> xs

-- FilterServices
runInfoService (TargetedMessage _ myId (MsgRun token "FilterServices" pams _)) = do
    helen <- get
    let snames = map fst . filter (match . JSON.toJSON . _serviceInfo . snd) . HashMap.toList
               $ Lens.view (serviceManager.serviceMap) helen
    sendMessage helen $ SourcedMessage myId $ MsgResult token (
      resultJSON ["serviceNames" .= snames]
      ) []
  where
    match | not (null keys) = (keys ==) . findKeys rcrLevel keys
          | jsonMatch /= JSON.object [] = findJSONMatches rcrLevel
          | otherwise = const True
    rcrLevel = case JSON.fromJSON <$> HashMap.lookup "rcrLevel" pams of
                 Just (JSON.Success l) -> max 0 (l :: Int)
                 _                     -> 0
    keys = case JSON.fromJSON <$> HashMap.lookup "keys" pams of
                 Just (JSON.Success l) -> List.sort l
                 _                     -> []
    jsonMatch = fromMaybe (JSON.object []) $ HashMap.lookup "jsonMatch" pams
    findKeys 0 ks (JSON.Object v) = List.foldl' List.union ks' . map (findKeys 0 (ks \\ ks') . snd) $ HashMap.toList (HashMap.filter nonNullary v)
                             where ks' = ks `List.intersect` List.sort (HashMap.keys $ HashMap.filter nonNullary v)
    findKeys 1 ks (JSON.Object v) = ks `List.intersect` List.sort (HashMap.keys $ HashMap.filter nonNullary v)
    findKeys n ks (JSON.Object v) = List.foldl' List.union [] . map (findKeys (n-1) ks . snd) $ HashMap.toList v
    findKeys _ _ _ = []
    matchJSON JSON.Null _  = True
    matchJSON (JSON.Array  t) (JSON.Array  v) = t == v
    matchJSON (JSON.String t) (JSON.String v) = t == v
    matchJSON (JSON.Number t) (JSON.Number v) = t == v
    matchJSON (JSON.Bool   t) (JSON.Bool   v) = t == v
    matchJSON (JSON.Object t) (JSON.Object v) = (HashMap.size t == HashMap.size v') && HashMap.foldl' (&&) True v'
      where v' = HashMap.intersectionWith matchJSON t v
    matchJSON _ _ = False
    findJSONMatches 0 val@(JSON.Object v) = matchJSON jsonMatch val || HashMap.foldl' (\a x -> findJSONMatches 0 x || a) False v
    findJSONMatches 0 val = matchJSON jsonMatch val
    findJSONMatches 1 val = matchJSON jsonMatch val
    findJSONMatches n (JSON.Object v) = HashMap.foldl' (\a x -> findJSONMatches (n-1) x || a) False v
    findJSONMatches _ _ = False
    nonNullary JSON.Null       = False
    nonNullary (JSON.Array _)  = True
    nonNullary (JSON.String _) = True
    nonNullary (JSON.Number _) = True
    nonNullary (JSON.Bool   b) = b
    nonNullary (JSON.Object b) = not $ HashMap.null b

--jsonMatch

-- unknown messages are ignored
runInfoService (TargetedMessage _ _ msg)   -- ignore register responses
                                         | -123456782 == msgToken msg = return ()
                                         | -123456781 == msgToken msg = return ()
                                         | -123456780 == msgToken msg = return ()
                                           -- warn wrong route
                                         | otherwise  = logWarnNS "RegistrationService" $
  "Received unexpected message. " <> Text.pack (show $ msgToken msg)
  <> ". Header: " <> (LText.toStrict . LText.decodeUtf8 . JSON.encode . JSON.toJSON . fst $ makeMessage msg)


serviceInfoMessage :: Message
serviceInfoMessage = MsgRun (-123456780) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= JSON.String ( "Returns all known information on selected services such"
                                           <> " as a short description like this one, in- and output"
                                           <> " description and an example call.")
      , "serviceName"        .= JSON.String "ServiceInfo"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= JSON.object
          [ "OPT serviceNames"     .= JSON.String "list"
          ]
      , "outputs"            .= JSON.object
          [ "ANY serviceName"      .= JSON.object
            [ "description"        .= JSON.String "string"
            , "serviceName"        .= JSON.String "string"
            , "qua-view-compliant" .= JSON.String "boolean"
            , "inputs"             .= JSON.String "json"
            , "outputs"            .= JSON.String "json"
            , "exampleCall"        .= JSON.String "json"
            ]
          ]
      , "exampleCall"        .= JSON.object
          [ "run"         .= JSON.String "ServiceInfo"
          , "serviceNames" .= ["RemoteRegister" :: Text.Text]
          ]
      ]


serviceListMessage :: Message
serviceListMessage = MsgRun (-123456781) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= JSON.String ( "Get a list of all loaded and registered services"
                                           <> " as a list of service names.")
      , "serviceName"        .= JSON.String "ServiceList"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= JSON.object []
      , "outputs"            .= JSON.object
          [ "serviceNames"   .= JSON.String "[\"string\"]"
          ]
      , "exampleCall"        .= JSON.object
          [ "run" .= JSON.String "ServiceList"
          ]
      ]


filterServicesMessage :: Message
filterServicesMessage = MsgRun (-123456782) "RemoteRegister" o []
  where
    o = HashMap.fromList
      [ "description"        .= JSON.String ( "Filters services according to"
                                           <> "\n a) its keys at a given recursion level."
                                           <> "\n b) a given json template with values equal to 'null' meaning only keys are compared"
                                           <> "\n Recursion level 0 means all levels are checked; restrict to one level with rcrLevel = 1.")
      , "serviceName"        .= JSON.String "FilterServices"
      , "qua-view-compliant" .= JSON.Bool False
      , "inputs"             .= JSON.object
          [ "XOR keys"       .= JSON.String "['string']"
          , "XOR jsonMatch"  .= JSON.String "json"
          , "OPT rcrLevel" .= JSON.String "number"
          ]
      , "outputs"            .= JSON.object
          [ "serviceNames" .= JSON.String "['string']" ]
      , "exampleCall"        .= JSON.object
          [ "run"  .= JSON.String "FilterServices"
          , "keys" .= ["customKey1", "customKey2" :: Text.Text]
          ]
      ]
