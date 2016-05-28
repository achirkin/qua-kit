-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Connect.Base
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- Decode and encode abstract Luci protocol messages.
--
-- Luci message is a binary data, sequence of bytes sent through TCP sockets.
-- Structure of a message:
--
-- @
-- -------------------------------------------------------------------------------------------------
-- | content: [ x | y |  JSON message header   | n | y1 | attachment 1 | ... | yn | attachment n ] |
-- | size:      8   8             x              8   8         y1        ...   8         yn        |
-- -------------------------------------------------------------------------------------------------
--
--   x   :: Int64BE         -- 8 byte signed big-endian integer - bytesize of message header
--   y   :: Int64BE         -- 8 byte signed big-endian integer - bytesize of binary message body
--   msg :: JSON            -- x bytes of UTF-8 encoded json - message header
--   if y > 0 then
--     n   :: Int64BE         -- 8 byte signed big-endian integer - number of attachments
--     repeat N times:
--       y(i)   :: Int64BE    -- 8 byte signed big-endian integer - bytesize of i-th attachment
--       att(i) :: ByteString -- y(i) bytes of raw data - i-th attachment
-- @
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Luci.Connect.Base
    ( -- * Reading & writing functionality
      writeMessages
    , parseMessages
      -- * Basic data types
    , AttachmentReference (..)
    , checkAttachment, makeAReference
    , ComError (..)
    ) where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (MD5)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import           Data.Conduit
import qualified Data.Conduit.Binary as ConB
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List ((\\), sortOn)
import qualified Data.Foldable as Fold

import Luci.Connect.Internal


-- | Conduit pipe that receives Luci messages, encodes them,
--   and outputs plain bytestring.
--
--   * Connect its input to a luci message producer
--   * Connect its output to a TCP sink
writeMessages :: (ToJSON a, Monad m)
              => Conduit (a, [ByteString]) m ByteString
writeMessages = do
  minput <- await
  case minput of
    Nothing -> return ()
    Just (msg, atts) -> do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = if null atts
                    then 0
                    else 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      writeInt64be (fromIntegral $ BS.length mainpart)
      writeInt64be (fromIntegral attSize)
      yield mainpart
      if attSize == 0
      then return ()
      else do
        writeInt64be (fromIntegral $ length atts)
        mapM_ writeAtt atts
      writeMessages
  where
    writeAtt bs = do
      writeInt64be (fromIntegral $ BS.length bs)
      yield bs
    writeInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE


-- | Conduit pipe that receives binary data, parses it,
--   and outputs understandable Luci messages.
--   Messages may fail to read; in that case @Left error@ is returned instead of a message.
--
--   * Connect its input to a TCP source
--   * Connect its output to a luci message consumer
--
-- There are validation requirements that a message must satisfy
-- (overwise, 'MsgValidationError' is returned):
--
-- * A message header must be a valid JSON.
-- * Overall attachment size must be equal to the sum of attachment sizes + (n+1)*8.
-- * All attachments must be referenced in a header at least once via 'AttachmentReference'.
parseMessages :: (FromJSON a, Monad m)
              => Conduit ByteString m (Either ComError (a, [ByteString]))
parseMessages = do
    r <- runExceptT $ do
      hSize <- lift (ConB.take 8) >>= tryReadInt64be
      aSize <- lift (ConB.take 8) >>= tryReadInt64be
      emsgVal <- lift (ConB.take (fromIntegral hSize)) >>= tryDecode
      atts <- case aSize of
        0 -> return []
        s -> do
          hANum <- lift (ConB.take 8) >>= tryReadInt64be
          (ss, atts) <- unzip <$> replicateM (fromIntegral hANum) tryReadAttachment
          -- check attachment sizes!
          if s /= sum ss + (hANum + 1) * 8
          then throwE . MsgValidationError $ "Attachment sizes mismatch"
          else do
            arefs <- lookupARefs (fromIntegral hANum) emsgVal
            -- whether all attachments are correct
            mapM (\(aref,att) -> if checkAttachment aref att
                                 then return att
                                 else throwE . MsgValidationError $
                                    "Error validaring attachment " ++ show (toEncoding aref)
                 ) $ zip arefs atts
      -- decode JSON value to FromJSON a
      case JSON.fromJSON emsgVal of
        JSON.Error err   -> throwE . UnexpectedJSONError $ err
        JSON.Success msg -> return (msg, atts)
    yield r
    parseMessages
  where
    -- wrapper around Binary's runGetOrFail
    tryReadInt64be bs = case Binary.runGetOrFail Binary.getInt64be bs of
                 Left  (_,_,msg) -> throwE . ByteReadingError $ msg
                 Right (_,_,val) -> if val >= 0 then return val
                    else throwE . MsgValidationError
                         $ "Unexpected negative integer!"
    -- wrapper around Aeson's decode
    tryDecode bs = case JSON.eitherDecode' bs of
                 Left  msg -> throwE . MsgValidationError $ msg
                 Right val -> return val
    tryReadAttachment = do
      hSize <- lift (ConB.take 8) >>= tryReadInt64be
      a <- lift (BSL.toStrict <$> ConB.take (fromIntegral hSize))
      return (hSize, a)

-- | All message attachments must be specified in JSON header of the message.
--   However, one attachment may be referred in more than one place in the header.
--   Luci specifies a special format for the attachments:
--
-- @
--  { 'format'    : String,
--  , 'attachment':
--       { 'length'  : Number
--       , 'checksum': String
--       , 'position': Number (starting at 1; 0 = undefined position)
--       }
--  , 'OPT name'  : String
--  , 'ANY key'   : String
--  }
-- @
--   Here we ignore @ANY key@, because it does not seem to be useful.
--
data AttachmentReference = AttachmentReference
  { attFormat :: !Text         -- ^ Hint for a client telling which data type represents the ByteString
  , attLength :: !Int          -- ^ Bytelength of an attachment
  , attMD5    :: !String       -- ^ MD5 hash value of an attachment
  , attIndex  :: !Int          -- ^ Position of an attachment in a luci protocol message
  , attName   :: !(Maybe Text) -- ^ Optional name of an attachment
  }

-- | Check an attachment;
--   returns true if an attachment length and an MD5 hash are correct.
checkAttachment :: AttachmentReference -> ByteString -> Bool
checkAttachment aref bs = BS.length bs == attLength aref
                        && attMD5 aref == show computedHash
  where computedHash = Hash.hash bs :: Hash.Digest MD5

-- | Make a valid reference to an attachment
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

-- | This instance ignores @ANY key@ when converts from JSON
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

-- | This instance does not provide a way to add @ANY key@
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

-- | Errors that may occur while getting messages from remote side
data ComError
  = ByteReadingError !String    -- ^ Failed to read data from source
  | MsgValidationError !String  -- ^ The message is corrupted or invalid
  | UnexpectedJSONError !String -- ^ Could not convert JSON to a requested data type
  deriving (Eq, Show)


----------------------------------------------------------------------------------------------------
-- Internal supplementary stuff
----------------------------------------------------------------------------------------------------

lookupARefs :: Monad m => Int -> JSON.Value -> ExceptT ComError m [AttachmentReference]
lookupARefs n v = case JSON.fromJSON v of
  JSON.Error err -> throwE $ MsgValidationError err
  JSON.Success (GetARefs f) -> return . snd . unzip . sortOn fst
                             . HashMap.toList $ f [1..n]


-- | Lookup requested references
newtype GetARefs = GetARefs ([Int] -> HashMap Int AttachmentReference)

-- | Zero value for `GetARefs`
noARefs :: GetARefs
noARefs = GetARefs $ \_ -> HashMap.empty

-- | Actual lookup of requested references
instance FromJSON GetARefs where
  parseJSON (Object o) =
    let maybeARef = do
          a <- HashMap.lookup "attachment" o
          i <- getIdx a
          aref <- fromResult . JSON.fromJSON $ Object o
          return $ GetARefs $ \xs -> if elem i xs then HashMap.singleton i aref else HashMap.empty
    in case maybeARef of
        Just aref -> pure aref
        Nothing -> HashMap.foldl' updateARefs (pure noARefs) o
    where
      getIdx (Object a) = HashMap.lookup "position" a >>= fromResult . JSON.fromJSON
      getIdx _ = Nothing
      fromResult (JSON.Error _) = Nothing
      fromResult (JSON.Success a) = Just a
  parseJSON (Array vals) = Fold.foldl' updateARefs (pure noARefs) vals
  parseJSON _ = pure noARefs

-- | Supplementary function to fold JSON arrays and objects
updateARefs :: JSON.Parser GetARefs -> JSON.Value -> JSON.Parser GetARefs
updateARefs pGAR v = pGAR >>= \(GetARefs f) -> do
        GetARefs g <- parseJSON v
        return . GetARefs $ \xs ->
            let hm1 = f xs
                found = HashMap.keys hm1
                rems = xs \\ found
            in case rems of
                [] -> hm1
                rs -> hm1 `HashMap.union` g rs
