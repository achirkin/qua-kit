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
    , Message (..), parseMessage, makeMessage, msgToken
    , ServiceName (..), Token (..), Percentage (..), ServiceResult (..)
    ) where



import qualified Data.ByteString as BS
--import qualified Data.ByteString.Base64 as BS
import           Data.HashMap.Strict as HashMap
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (MD5)
--import           Control.Monad (when)


import           Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Hashable
import           Data.String (IsString)

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
  } deriving (Eq, Show)

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
  deriving (Eq,Ord,Show,IsString, FromJSON, ToJSON, Hashable)

-- | Luci callID is used to reference client's calls to luci and services
newtype Token = Token Int64
  deriving (Eq,Ord,Show,Num,Enum, Real,Integral,FromJSON, ToJSON, Hashable)

-- | Percentage [0..100]%; used in luci messages to indicate state of a service computation
newtype Percentage = Percentage Double
  deriving (Eq,Ord,Num,Real,RealFrac,RealFloat,Fractional,Floating, FromJSON, ToJSON)

-- | Result or progress records is an arbitrary JSON Object
newtype ServiceResult = ServiceResult JSON.Object
  deriving (Eq, Show, FromJSON, ToJSON)



-- | Represent all registerd message types. Anything else is garbage!
data Message
  = MsgRun !Token !ServiceName !JSON.Object ![ByteString]
    -- ^ run service message, e.g. {'run': 'ServiceList', 'token': 'sfhrEg2sh'};
    -- params: 'token', 'run', [(name, value)], optional attachments
  | MsgCancel !Token
    -- ^ cancel service message, e.g. {'cancel': 'sfhrEg2sh'};
    -- params: 'token': 'sfhrEg2sh' };
  | MsgResult !Token !ServiceResult ![ByteString]
    -- ^ result of a service execution
    -- e.g. { 'token': 'sfhrEg2sh', 'result': Object };
    -- params: 'token', 'result', optional attachments
  | MsgProgress !Token !Percentage !(Maybe ServiceResult) ![ByteString]
    -- ^ result of a service execution,
    -- e.g. { 'token': 'sfhrEg2sh', 'progress': 47, 'intermediateResult': null};
    -- params: 'token', 'progress', 'intermediateResult', optional attachments
  | MsgError !Token !Text
    -- ^ error message, e.g. { 'token': 'sfhrEg2sh', 'error': 'We are in trouble!'};
    -- params: 'token', 'error'

-- | All messsages have a unique for connection token
msgToken :: Message -> Token
msgToken (MsgRun t _ _ _) = t
msgToken (MsgCancel t) = t
msgToken (MsgResult t _ _) = t
msgToken (MsgProgress t _ _ _) = t
msgToken (MsgError t _) = t


-- | Get attachment from a message, checking its MD5 hash
attachment :: Message -> AttachmentReference -> Maybe ByteString
attachment (MsgRun _ _ _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgResult _  _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgProgress _ _ _ bs) r = indexList bs (attIndex r - 1) >>= \x ->
    if checkAttachment r x then Just x else Nothing
attachment (MsgCancel _) _ = Nothing
attachment (MsgError _ _) _ = Nothing

indexList :: [a] -> Int -> Maybe a
indexList xs i = headMaybe $ drop i xs
  where
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x

-- | Parse all registered message types
parseMessage :: LuciMessage -> Result Message
parseMessage (MessageHeader (JSON.Object js), bss)
  | Just (JSON.String s) <- HashMap.lookup "run" js      = Success $ MsgRun token (ServiceName s) js $ seqList bss
  | Just (JSON.Number t) <- HashMap.lookup "cancel" js   = Success $ MsgCancel (round t)
  | Just (JSON.Object o) <- HashMap.lookup "result" js   = Success $ MsgResult token (ServiceResult o) $ seqList bss
  | Just (JSON.Number n) <- HashMap.lookup "progress" js = Success $ MsgProgress token (realToFrac n) (HashMap.lookup "intermediateResult" js >>= toResult) $ seqList bss
  | Just (JSON.String s) <- HashMap.lookup "error" js    = Success $ MsgError token s
  | otherwise = Error "None of registered keys are found (run,cancel,newCallID,result,progress,error)"
  where
    token = case JSON.fromJSON <$> HashMap.lookup "token" js of
              -- TODO remove dirty hack (rtoken == 9702953879202186) used to detect scenario.geojson.Get messages
              Nothing          -> if HashMap.lookup "serviceName" js == Just (JSON.String "scenario.geojson.Get")
                                  then 9702953879202186
                                  else 77777777777
              Just (Error _)   -> 66666666666
              Just (Success t) -> t
    toResult (JSON.Object o) = Just $ ServiceResult o
    toResult _ = Nothing
parseMessage (MessageHeader _, _) = Error "Invalid JSON type (expected object)."


-- | Convert registered message type into generic message
makeMessage :: Message -> LuciMessage
makeMessage (MsgCancel token) = (MessageHeader $ JSON.object ["cancel" .= token], [])
makeMessage (MsgError token s) = (MessageHeader $ JSON.object ["error" .= s, "token" .= token], [])
makeMessage (MsgRun token sname js bss) =
  (MessageHeader . JSON.object $ ["run" .= sname,  "token" .= token] ++ HashMap.toList js, seqList bss)
makeMessage (MsgResult token (ServiceResult sr) bss) =
  (MessageHeader . JSON.object $ ["result" .= sr,  "token" .= token], seqList bss)
makeMessage (MsgProgress token p msr bss) =
  (MessageHeader . JSON.object $ ["progress" .= p, "token" .= token] ++ srMaybe msr, seqList bss)
  where
    srMaybe (Just (ServiceResult x)) = ["intermediateResult" .= x]
    srMaybe _ = []


seqList :: [a] -> [a]
seqList [] = []
seqList (x:xs) = x `seq` (x : seqList xs)




