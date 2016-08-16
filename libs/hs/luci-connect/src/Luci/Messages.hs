-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Messages
-- Copyright   :  Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-- Collection of data types with converters from\/to JSON
-- for common Luci messages.
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Luci.Messages
    ( -- * Attachment references
      -- | Helper functions to check and make references for attachments in JSON message header.
      AttachmentReference (..)
    , checkAttachment, makeAReference, attachment
      -- * Helpers
    , simpleMessage, fromMessage
      -- * Common Messages
    , RemoteRegister (..)
      -- * Parsed message types
    , Message (..), LuciMsgInfo (..), parseMessage, makeMessage
    , ServiceName (..), CallId (..), Percentage (..), ServiceResult (..)
    ) where



import qualified Data.ByteString as BS
--import qualified Data.ByteString.Base64 as BS
import           Data.HashMap.Strict as HashMap
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (MD5)
--import           Control.Monad (when)


import           Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import           Data.Time (DiffTime)

import Luci.Connect.Base
import Luci.Connect.Internal


-- | Make a message without attachments
simpleMessage :: ToJSON a => a -> LuciMessage
simpleMessage v = (toMsgHead v, [])

-- | Parse a message header
fromMessage :: FromJSON a => LuciMessage -> Result a
fromMessage (MessageHeader v,_) = fromJSON v

-- | Register a service in Luci
data RemoteRegister = RemoteRegister
  { exampleCall :: !Value
  , serviceName :: !Text
  , description :: !Text
  , inputs  :: !(Maybe Value)
  , outputs :: !(Maybe Value)
  }

instance ToJSON RemoteRegister where
  toJSON RemoteRegister{..} = objectM
    [ "run"         .=! ("RemoteRegister" :: Text)
    , "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
    , "description" .=! description
    , "inputs"      .=? inputs
    , "outputs"     .=? outputs
    ]


--Right (Object (fromList [("newCallID",Number 10.0)]),[])
--Right (Object (fromList [("instanceID",Number 0.0),("progress",Object (fromList [])),("callID",Number 10.0),("serviceName",String "RemoteRegister"),("percentage",Number 0.0)]),[])
--Right (Object (fromList [("instanceID",Number 0.0),("result",Object (fromList [("registeredName",String "CoolTestService")])),("callID",Number 10.0),("serviceName",String "RemoteRegister")]),[])


-- | All message attachments must be specified in JSON header of the message.
--   However, one attachment may be referred in more than one place in the header.
--   Luci specifies a special format for the attachments:
--
-- @
--  { \'format\'    : String,
--  , \'attachment\':
--       { \'length\'  : Number
--       , \'checksum\': String
--       , \'position\': Number (starting at 1; 0 = undefined position)
--       }
--  , 'OPT name'  : String
--  , 'ANY key'   : String
--  }
-- @
--   Here we ignore @ANY key@, because it does not seem to be useful.
--
data AttachmentReference = AttachmentReference
  { attFormat :: !Text         -- ^ Hint for a client telling which data type represents the ByteString.
  , attLength :: !Int          -- ^ Bytelength of an attachment.
  , attMD5    :: !String       -- ^ MD5 hash value of an attachment.
  , attIndex  :: !Int          -- ^ Position of an attachment in a Luci protocol message.
  , attName   :: !(Maybe Text) -- ^ Optional name of an attachment.
  }

-- | Check an attachment;
--   returns true if an attachment length and an MD5 hash are correct.
checkAttachment :: AttachmentReference -> ByteString -> Bool
checkAttachment aref bs = BS.length bs == attLength aref
                        && attMD5 aref == show computedHash
  where computedHash = Hash.hash bs :: Hash.Digest MD5

-- | Make a valid reference to an attachment.
makeAReference :: ByteString  -- ^ attachment content
               -> Text        -- ^ attFormat
               -> Int         -- ^ attIndex - position of the attachment
               -> Maybe Text  -- ^ attName
               -> AttachmentReference
makeAReference bs format idx mname = AttachmentReference
  { attFormat = format
  , attLength = BS.length bs
  , attMD5    = show (Hash.hash bs :: Hash.Digest MD5)
  , attIndex  = idx
  , attName   = mname
  }

-- | This instance ignores @ANY key@ when converts from JSON.
instance FromJSON AttachmentReference where
  parseJSON (Object v) = AttachmentReference
      <$> v .: "format"
      <*> att ..: "length"
      <*> att ..: "checksum"
      <*> att ..: "position"
      <*> v .:? "name"
    where
      att = v .: "attachment"
  parseJSON invalid = JSON.typeMismatch "AttachmentReference" invalid

-- | This instance does not provide a way to add @ANY key@.
instance ToJSON AttachmentReference where
  toJSON AttachmentReference{..} = objectM
    [ "format"     .=! attFormat
    , "attachment" .=! object
          [ "length"   .= attLength
          , "checksum" .= attMD5
          , "position" .= attIndex
          ]
    , "name" .=? attName
    ]


----------------------------------------------------------------------------------------------------
-- * Parsed known message types
----------------------------------------------------------------------------------------------------


-- | Luci service name
newtype ServiceName = ServiceName Text
  deriving (Eq,Ord,Show,IsString, FromJSON, ToJSON)

-- | Luci callID is used to reference client's calls to luci and services
newtype CallId = CallId Int64
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral, FromJSON, ToJSON)

-- | Luci taskID is used in the context of luci workflows to refer to tasks
newtype TaskId = TaskId Int64
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral, FromJSON, ToJSON)

-- | Percentage [0..100]%; used in luci messages to indicate state of a service computation
newtype Percentage = Percentage Double
  deriving (Eq,Ord,Num,Real,RealFrac,RealFloat,Fractional,Floating, FromJSON, ToJSON)

-- | Result or progress records is an arbitrary JSON Object
newtype ServiceResult = ServiceResult JSON.Object
  deriving (Eq, Show, FromJSON, ToJSON)


-- | This portion of information is filled by middleware (Luci or Helen).
--   Service do not need to fill it, but client normally receives it as a result.
data LuciMsgInfo = LuciMsgInfo
  { lmiCallId      :: !CallId
    -- ^ Unique callID is given by luci for every user request.
  , lmiDuration    :: !DiffTime
    -- ^ Duration of a service execution
  , lmiServiceName :: !ServiceName
    -- ^ Name of running service
  , lmiTaskId      :: !TaskId
    -- ^ taskID is assigned if a service runs within a workflow
  } deriving (Eq, Show)

instance FromJSON LuciMsgInfo where
  parseJSON (Object v) = LuciMsgInfo
      <$> v .: "callID"
      <*> (f <$> v .:? "duration")
      <*> v .: "serviceName"
      <*> (fromMaybe 0 <$> v .:? "taskID")
    where
      f :: Maybe Double -> DiffTime
      f Nothing = 0
      f (Just x) = realToFrac $ x/1000
  parseJSON invalid = JSON.typeMismatch "LuciMsgInfo" invalid

instance ToJSON LuciMsgInfo where
  toJSON LuciMsgInfo{..} = object
    [ "callID"      .= lmiCallId
    , "duration"    .= ((realToFrac lmiDuration :: Double) * 1000)
    , "serviceName" .= lmiServiceName
    , "taskID"      .= lmiTaskId
    ]


-- | Represent all registerd message types. Anything else is garbage!
data Message
  = MsgRun !ServiceName !JSON.Object ![ByteString]
    -- ^ run service message, e.g. {'run': 'ServiceList'};
    -- params: 'run', [(name, value)], optional attachments
  | MsgCancel !CallId
    -- ^ cancel service message, e.g. {'cancel': 25};
    -- params: 'callID'
  | MsgNewCallID !CallId
    -- ^ Luci call id, { newCallID: 57 };
    -- params: 'newCallID'
  | MsgResult !(Maybe LuciMsgInfo) !ServiceResult ![ByteString]
    -- ^ result of a service execution
    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result', optional attachments
  | MsgProgress !(Maybe LuciMsgInfo) !Percentage !ServiceResult ![ByteString]
    -- ^ result of a service execution,
    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, percentage: 0, progress: null};
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'percentage', 'progress', optional attachments
  | MsgError !Text
    -- ^ error message, e.g. {'error': 'We are in trouble!'};
    -- params: 'error'

-- | Get attachment from a message, checking its MD5 hash
attachment :: Message -> AttachmentReference -> Maybe ByteString
attachment (MsgRun _ _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgResult _  _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgProgress _ _ _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgCancel _) _ = Nothing
attachment (MsgNewCallID _) _ = Nothing
attachment (MsgError _) _ = Nothing

indexList :: [a] -> Int -> Maybe a
indexList xs i = headMaybe $ drop i xs
  where
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x

-- | Parse all registered message types
parseMessage :: LuciMessage -> Result Message
parseMessage (MessageHeader (JSON.Object js), bss)
  | Just (JSON.String s) <- HashMap.lookup "run" js       = Success $ MsgRun (ServiceName s) js $ seqList bss
  | Just (JSON.Number n) <- HashMap.lookup "cancel" js    = Success $ MsgCancel (round n)
  | Just (JSON.Number n) <- HashMap.lookup "newCallID" js = Success $ MsgNewCallID (round n)
  | Just (JSON.Object o) <- HashMap.lookup "result" js    = Success $ MsgResult luciInfo (ServiceResult o) $ seqList bss
  | Just (JSON.Object o) <- HashMap.lookup "progess" js   = Success $ MsgProgress luciInfo perc (ServiceResult o) $ seqList bss
  | Just (JSON.String s) <- HashMap.lookup "error" js     = Success $ MsgError s
  | otherwise = Error "None of registered keys are found (run,cancel,newCallID,result,progress,error)"
  where
    luciInfo = case fromJSON (JSON.Object js) of
                  Error _ -> Nothing
                  Success li -> Just li
    perc = case HashMap.lookup "percentage" js of
             Just (JSON.Number p) -> realToFrac p
             _ -> 0
parseMessage (MessageHeader _, _) = Error "Invalid JSON type (expected object)."

-- | Convert registered message type into generic message
makeMessage :: Message -> LuciMessage
makeMessage (MsgCancel n) = (MessageHeader $ JSON.object ["cancel" .= n], [])
makeMessage (MsgNewCallID n) = (MessageHeader $ JSON.object ["newCallID" .= n], [])
makeMessage (MsgError s) = (MessageHeader $ JSON.object ["error" .= s], [])
makeMessage (MsgRun (ServiceName s) js bss) =
  (MessageHeader . JSON.Object $ HashMap.insert "run" (JSON.String s) js, seqList bss)
makeMessage (MsgResult Nothing sr bss) =
  (MessageHeader . JSON.object $ ["result" .= sr], seqList bss)
makeMessage (MsgProgress Nothing p sr bss) =
  (MessageHeader . JSON.object $ ["progress" .= sr, "percentage" .= p], seqList bss)
makeMessage (MsgResult (Just mi) (ServiceResult sr) bss) =
  (MessageHeader . JSON.Object $ HashMap.insert "result" (JSON.Object sr) mio, seqList bss)
  where
    JSON.Object mio = JSON.toJSON mi
makeMessage (MsgProgress (Just mi) p (ServiceResult sr) bss) =
  (MessageHeader . JSON.Object . HashMap.insert "progress" (JSON.toJSON p)
                               $ HashMap.insert "result" (JSON.Object sr) mio, seqList bss)
  where
    JSON.Object mio = JSON.toJSON mi


seqList :: [a] -> [a]
seqList [] = []
seqList (x:xs) = x `seq` (x : seqList xs)




