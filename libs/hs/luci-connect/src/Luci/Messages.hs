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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Luci.Messages
    ( -- * Attachment references
      -- | Helper functions to check and make references for attachments in JSON message header.
      AttachmentReference (..)
    , checkAttachment, makeAReference, attachment
      -- * Helpers
    , simpleMessage, fromMessage, luciMessageToken
      -- * Common Messages
    , ServiceInfo (..)
      -- * Parsed message types
    , Message (..), parseMessage, makeMessage, msgToken
    , ServiceName (..), Token (..), Percentage (..), ServiceResult (..)
    , UserId (..), msgSenderId, AuthRole (..), msgSenderAuthRole
    , resultJSON, objectJSON, showJSON
    ) where



import qualified Crypto.Hash             as Hash
import           Crypto.Hash.Algorithms  (MD5)
import           Data.Aeson              as JSON
import qualified Data.Aeson.Types        as JSON
import qualified Data.ByteString         as BS
import           Data.Hashable
import           Data.HashMap.Strict     as HashMap
import           Data.Maybe              (fromMaybe)
import           Data.String             (IsString)
import qualified Data.Text.Lazy          as LText
import qualified Data.Text               as SText
import qualified Data.Text.Lazy.Encoding as LText

import           Luci.Connect.Base
import           Luci.Connect.Internal


-- | Make a message without attachments
simpleMessage :: ToJSON a => a -> LuciMessage
simpleMessage v = (toMsgHead v, [])

-- | Parse a message header
fromMessage :: FromJSON a => LuciMessage -> Result a
fromMessage (MessageHeader v,_) = fromJSON v

-- | Try get token from raw JSON message
luciMessageToken :: LuciMessage -> Maybe Token
luciMessageToken (MessageHeader (JSON.Object js), _)
  | Just (JSON.Number t) <- HashMap.lookup "callID" js = Just $ round t
luciMessageToken _ = Nothing

-- | Full information about a service
data ServiceInfo = ServiceInfo
  { exampleCall      :: !JSON.Value
  , serviceName      :: !ServiceName
  , description      :: !Text
  , inputs           :: !(Maybe JSON.Value)
  , outputs          :: !(Maybe JSON.Value)
  , constraints      :: !(Maybe JSON.Value)
  , quaQitCompliance :: !Bool
    -- ^ Does service comply to qua-kit visual service specification?
  , nonBlocking      :: !Bool
    -- ^ If nonBlocking = true, then luci/helen should not manage workload for the service,
    --   i.e. the service can receive multiple run requrests and send corresponding results
    --      not in a strict order
  }


instance ToJSON ServiceInfo where
  toJSON ServiceInfo{..} = objectM
    [ "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
    , "description" .=! description
    , "inputs"      .=? inputs
    , "outputs"     .=? outputs
    , "constraints" .=? constraints
    , "qua-view-compliant" .=! quaQitCompliance
    , "nonBlocking" .=! nonBlocking
    ]

instance FromJSON ServiceInfo where
  parseJSON (JSON.Object v) = ServiceInfo
     <$> v .: "exampleCall"
     <*> v .: "serviceName"
     <*> v .: "description"
     <*> v .:? "inputs"
     <*> v .:? "outputs"
     <*> v .:? "constraints"
     <*> (fromMaybe False <$> v .:? "qua-view-compliant")
     <*> (fromMaybe False <$> v .:? "nonBlocking")
  parseJSON invalid = JSON.typeMismatch "ServiceInfo" invalid


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
  deriving (Eq,Ord,Show,Num,Enum, Real,Integral,FromJSON,ToJSON,Hashable)

-- | Percentage [0..100]%; used in luci messages to indicate state of a service computation
newtype Percentage = Percentage Double
  deriving (Eq,Ord,Num,Real,RealFrac,RealFloat,Fractional,Floating, FromJSON, ToJSON)

-- | Result or progress records is an arbitrary JSON Object
newtype ServiceResult = ServiceResult JSON.Object
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Authorization level of a sender.
--   Only use it if you trust the sender!
data AuthRole
  = Admin
  | Student
  | Local
  deriving (Show, Read, Eq)

instance FromJSON AuthRole where
  parseJSON (JSON.String "Admin")   = pure Admin
  parseJSON (JSON.String "Student") = pure Student
  parseJSON (JSON.String "Local")   = pure Local
  parseJSON _                       = pure Local

instance ToJSON AuthRole where
  toJSON r = JSON.String . SText.pack $ show r



-- | Abstract user id a sender.
--   Only use it if you trust the sender!
newtype UserId = UserId Int64
  deriving (Eq,Ord,Show,Num,Enum, Real,Integral,FromJSON, ToJSON, Hashable)

-- | Construct ServiceResult similarly to Aeson.object function
resultJSON :: [(Text,Value)] -> ServiceResult
resultJSON = ServiceResult . HashMap.fromList

-- | Construct Aeson.Object similarly to Aeson.object function
objectJSON :: [(Text,Value)] -> JSON.Object
objectJSON = HashMap.fromList

-- | Helper to print formatted JSON
showJSON :: JSON.Value -> Text
showJSON = LText.toStrict . LText.decodeUtf8 . encode

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
msgToken (MsgRun t _ _ _)  = t
msgToken (MsgCancel t)         = t
msgToken (MsgResult t _ _)     = t
msgToken (MsgProgress t _ _ _) = t
msgToken (MsgError t _)        = t


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
    headMaybe []    = Nothing
    headMaybe (x:_) = Just x

-- | Parse all registered message types
parseMessage :: LuciMessage -> Result Message
parseMessage (MessageHeader (JSON.Object js), bss)
  | Just (JSON.String s) <- HashMap.lookup "run" js      = Success $ MsgRun token (ServiceName s) js' $ seqList bss
  | Just _               <- HashMap.lookup "cancel" js   = Success $ MsgCancel token
  | Just (JSON.Object o) <- HashMap.lookup "result" js   = Success $ MsgResult token (ServiceResult o) $ seqList bss
  | Just (JSON.Number n) <- HashMap.lookup "progress" js = Success $ MsgProgress token (realToFrac n) (HashMap.lookup "intermediateResult" js >>= toResult) $ seqList bss
  | Just (JSON.String s) <- HashMap.lookup "error" js    = Success $ MsgError token s
  | otherwise = Error "None of registered keys are found (run,cancel,newCallID,result,progress,error)"
  where
    js' = HashMap.delete "callID" js
    token = case JSON.fromJSON <$> HashMap.lookup "callID" js of
              Nothing          -> 77777777777
              Just (Error _)   -> 66666666666
              Just (Success t) -> t
    toResult (JSON.Object o) = Just $ ServiceResult o
    toResult _               = Nothing
parseMessage (MessageHeader _, _) = Error "Invalid JSON type (expected object)."

-- | Access UserId of sender
msgSenderId :: Functor f => (Maybe UserId -> f (Maybe UserId)) -> Message -> f Message
msgSenderId k (MsgRun token sname js bss) = f <$> k (HashMap.lookup "UserId" js >>= result2mb . fromJSON )
  where
    f Nothing = MsgRun token sname js bss
    f (Just i) = MsgRun token sname (HashMap.insert "UserId" (toJSON i) js) bss
msgSenderId k msg = msg <$ k Nothing

-- | Access AuthRole of sender
msgSenderAuthRole :: Functor f => (AuthRole -> f AuthRole) -> Message -> f Message
msgSenderAuthRole k (MsgRun token sname js bss) = f <$> k (fromMaybe Local $ HashMap.lookup "AuthRole" js >>= result2mb . fromJSON )
  where
    f r = MsgRun token sname (HashMap.insert "AuthRole" (toJSON r) js) bss
msgSenderAuthRole k msg = msg <$ k Local



-- | Convert registered message type into generic message
makeMessage :: Message -> LuciMessage
makeMessage (MsgCancel token) = (MessageHeader $ JSON.object ["cancel" .= token, "callID" .= token], [])
makeMessage (MsgError token s) = (MessageHeader $ JSON.object ["error" .= s, "callID" .= token], [])
makeMessage (MsgRun token sname js bss) =
  (MessageHeader . JSON.object $
      ["run" .= sname,  "callID" .= token] ++ HashMap.toList js', seqList bss)
  where
    js' = HashMap.delete "run" $ HashMap.delete "callID" js
makeMessage (MsgResult token (ServiceResult sr) bss) =
  (MessageHeader . JSON.object $ ["result" .= sr,  "callID" .= token], seqList bss)
makeMessage (MsgProgress token p msr bss) =
  (MessageHeader . JSON.object $ ["progress" .= p, "callID" .= token] ++ srMaybe msr, seqList bss)
  where
    srMaybe (Just (ServiceResult x)) = ["intermediateResult" .= x]
    srMaybe _                        = []

result2mb :: Result a -> Maybe a
result2mb (Error _)   = Nothing
result2mb (Success a) = Just a

seqList :: [a] -> [a]
seqList []     = []
seqList (x:xs) = x `seq` (x : seqList xs)
