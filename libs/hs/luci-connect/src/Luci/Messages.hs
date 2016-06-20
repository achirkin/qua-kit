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
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Luci.Messages
    ( -- * Attachment references
      -- | Helper functions to check and make references for attachments in JSON message header.
      AttachmentReference (..)
    , checkAttachment, makeAReference
      -- * Helpers
    , simpleMessage, fromMessage
      -- * Common Messages
    , RemoteRegister (..)
    ) where



import qualified Data.ByteString as BS
--import qualified Data.ByteString.Base64 as BS
--import           Data.HashMap.Strict (HashMap)
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (MD5)

import           Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

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
  , inputs  :: !(Maybe Value)
  , outputs :: !(Maybe Value)
  }

instance ToJSON RemoteRegister where
  toJSON RemoteRegister{..} = objectM
    [ "run"         .=! ("RemoteRegister" :: Text)
    , "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
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
