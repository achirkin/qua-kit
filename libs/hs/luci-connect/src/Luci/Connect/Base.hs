-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Connect.Base
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- Decode and encode arbitrary <https://bitbucket.org/treyerl/luci2 Luci> protocol messages.
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
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Luci.Connect.Base
    ( -- * Basic types
      LuciMessage, LuciProcessing, MessageHeader (..)
    , toMsgHead, fromMsgHead
      -- * Message processing
      -- | Write conduits for message processing and connect them using
      --   the following combinators.
    , (=$=), (=&=), (=*=)
      -- * Reading & writing functionality
      -- | Combine conduits for reading and writing arbitrary Luci messages.
      --   Connect the provided conduits to corresponding TCP sink and source conduits
      --   from @conduit-network@ package.
      --   For testing purposes, you can short-circuit the conduits
      --   and write-read @ByteString@ messages:
      --
      -- @
      --   runConduit $ (yield ...) =$= writeMessages =$= parseMessages =$= ...await...
      -- @
    , writeMessages
    , parseMessages
    , ComError (..)
      -- * Attachment references
      -- | Helper functions to check and make references for attachments in JSON message header.
    , AttachmentReference (..)
    , checkAttachment, makeAReference
      -- * Panic recovery procedure
      -- | If a client become uncertain whether it has read all bytes of previous message or not,
      --   it can initiate a /panic recovery procedure/.
      --   The panic recovery procedure may be initiated by either side of a communication session.
      --
      --   1. Initiator generates __32 bytes of random binary data__, let call it @panicID@.
      --   2. Initiator sends a luci message without attachments,
      --      with a following header content
      --      (/encode @panicID@ as/
      --        <https://en.wikipedia.org/wiki/Base64#RFC_4648 Base64 (RFC 4648)>):
      --
      -- @
      --        { \'panic\': /Base64-encoded/ panicID /as a string/ }
      -- @
      --   3. Responder receives the message and decodes @panicID@.
      --   4. Responder sends a raw binary sequence: 32-byte message with @panicID@.
      --      /Note: this is not a luci message, but a raw byte stream/.
      --   5. Initiator listens for a binary data byte-by-byte until finds @panicID@.
      --      Since this moment, initiator knows for sure that
      --        binary stream is aligned according to their expectations;
      --      both sides switch to a normal communication mode.
      --
      --   The panic recovery procedure complements the validation mechanism of Luci messages:
      --   an initiator should initiate the panic if
      --   it faces unexpected input or message parts' sizes do not agree.
    , panicConduit
    , panicResponseConduit
    ) where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import qualified Control.Monad.ST as ST
import qualified Crypto.Hash as Hash
import           Crypto.Hash.Algorithms (MD5)
import           Crypto.Random (getRandomBytes, MonadRandom)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Builder as BSB
import           Data.Conduit
import           Data.Conduit.Internal (ConduitM (ConduitM), Pipe (..))
import qualified Data.Conduit.Binary as ConB
import           Data.Data (Data)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List ((\\), sortOn, sort)
import qualified Data.Foldable as Fold
import qualified Data.Primitive.Array as Prim
import           Data.String (IsString)
import           Data.Word

import Luci.Connect.Internal

import Control.Monad.IO.Class

-- | Alias for Luci message containing a header and attachments
type LuciMessage = (MessageHeader, [ByteString])

-- | Alias for a processing state:
--   The data is either processed and ready for sending (Left ByteString),
--   or is not processed yet and carries input message (Right LuciMessage).
type LuciProcessing = Either ByteString LuciMessage

-- | Luci message header is a JSON value
newtype MessageHeader = MessageHeader Value
  deriving (Eq, JSON.FromJSON, JSON.ToJSON, IsString, Data)

-- | Show actual fromatted JSON, rather than Aeson's 'Value'
instance Show MessageHeader where
  show (MessageHeader val) = ("Luci MessageHeader: " ++) . BSLC.unpack $ JSON.encode val


-- | Wrapper for 'toJSON'
toMsgHead :: ToJSON a => a -> MessageHeader
toMsgHead = MessageHeader . toJSON

-- | Wrapper for 'fromJSON'
fromMsgHead :: FromJSON a => MessageHeader -> Either String a
fromMsgHead (MessageHeader v) = case JSON.fromJSON v of
  JSON.Error msg -> Left msg
  JSON.Success a -> Right a

-- | Fusion operator, combining two Conduits together into a new Conduit.
--
--   If output of the left is (Left x) then pass it further.
--   If output of the left is (Right y) then pass y to the right conduit.
(=&=) :: Monad m => Conduit a m (Either e b) -> Conduit b m (Either e c) -> Conduit a m (Either e c)
ConduitM left0 =&= ConduitM right0 = ConduitM $ \rest ->
    let
        goRight final left right =
          case left of
           HaveOutput left' final' (Left e) -> HaveOutput (goRight final left' right) final' (Left e)
           _ ->
            case right of
                HaveOutput p c o  -> goLeft' p c o final left
                NeedInput rp rc   -> goLeft rp rc final left
                Done r2           -> PipeM (final >> return (rest r2))
                PipeM mp          -> PipeM (liftM recurse mp)
                Leftover right' i -> goRight final (HaveOutput left final $ Right i) right'
          where
            recurse = goRight final left

        -- go here if we need more input
        goLeft rp rc final left =
            case left of
                HaveOutput left' final' (Right o) -> goRight final' left' (rp o)
                HaveOutput left' final' (Left  e) ->
                      HaveOutput (goRight final' left' $ NeedInput rp rc) final (Left e)
                NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
                Done r1                   -> goRight (return ()) (Done r1) (rc r1)
                PipeM mp                  -> PipeM (liftM recurse mp)
                Leftover left' i          -> Leftover (recurse left') i
          where
            recurse = goLeft rp rc final

        -- go here if we have output, but want to send everything on the left first
        goLeft' hp hc ho final left =
            case left of
                HaveOutput _ _         (Right _) -> rightOutput
                HaveOutput left' final' (Left e) -> HaveOutput (recurse left') final' (Left e)
                NeedInput left' lc        -> rightOutput
                Done _                    -> rightOutput
                PipeM _                   -> rightOutput
                Leftover left' i          -> Leftover (recurse left') i
          where
            recurse = goLeft' hp hc ho final
            rightOutput = HaveOutput (goRight final left hp) (hc >> final) ho

     in goRight (return ()) (left0 Done) (right0 Done)
{-# INLINE [1] (=&=) #-}
infixr 3 =&=

--(=&&=) :: Conduit A IO (Either E B)
--       -> Conduit B IO (Either E C)
--       -> Conduit A IO (Either E C)
--ConduitM left0 =&&= ConduitM right0 = ConduitM $ \rest ->
--    let --goRight :: IO () -> PipeA () -> PipeB () -> PipeC ()
--        goRight final left right =
--            case right of
--                HaveOutput p c o  -> goLeft' p c o final left
--                NeedInput rp rc   -> goLeft rp rc final left
--                Done r2           -> PipeM (final >> return (rest r2))
--                PipeM mp          -> PipeM (liftM recurse mp)
--                Leftover right' i -> goRight final (HaveOutput left final $ Right i) right'
--          where
--            recurse = goRight final left
--
--        --goLeft :: (B -> PipeB ()) -> (() -> PipeB ()) -> IO () -> PipeA () -> PipeC ()
--        goLeft rp rc final left =
--            case left of
--                HaveOutput left' final' (Right o) -> goRight final' left' (rp o)
--                HaveOutput left' final' (Left  e) ->
--                      HaveOutput (goRight final left' $ NeedInput rp rc) final' (Left e)
--                NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
--                Done r1                   -> goRight (return ()) (Done r1) (rc r1)
--                PipeM mp                  -> PipeM (liftM recurse mp)
--                Leftover left' i          -> Leftover (recurse left') i
--          where
--            recurse = goLeft rp rc final
--
--        goLeft' hp hc ho final left =
--            case left of
--                HaveOutput _ _ (Right _)  -> rightOutput
--                HaveOutput left' final' (Left  e) -> HaveOutput (recurse left') final' (Left e)
--                NeedInput _ _             -> rightOutput
--                Done _                    -> rightOutput
--                PipeM mp                  -> PipeM (liftM recurse mp)
--                Leftover left' i          -> Leftover (recurse left') i
--          where
--            recurse = goLeft' hp hc ho final
--            rightOutput = HaveOutput (goRight final left hp) (hc >> final) ho
--
--     in goRight (return ()) (left0 Done) (right0 Done)
--
--type PipeA u = Pipe A A (Either E B) u IO ()
--type PipeB u = Pipe B B (Either E C) u IO ()
--type PipeC u = Pipe A A (Either E C) u IO ()
--
--
--data A = A
--data B = B
--data C = C
--data E = E


-- | Fusion operator, combining two Conduits together into a new Conduit.
--
--   Map the right conduit over the output of the left.
--   Leaves a value on the Left of Either unmodified.
(=*=) :: Monad m => Conduit a m (Either e b) -> Conduit b m c -> Conduit a m (Either e c)
a =*= b = a =&= mapOutput Right b
{-# INLINE [1] (=*=) #-}
infixr 4 =*=


-- | Conduit pipe that receives Luci messages, encodes them,
--   and outputs plain bytestring.
--
--   * Connect its input to a Luci message producer.
--   * Connect its output to a TCP sink.
writeMessages :: Monad m
              => Conduit LuciMessage m ByteString
writeMessages = do
  minput <- await
  case minput of
    Nothing -> return ()
    Just (msg, atts) -> do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = if null atts
                    then 0
                    else 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      if attSize == 0
      then return ()
      else do
        yieldInt64be (fromIntegral $ length atts)
        mapM_ writeAtt atts
      writeMessages
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE


-- | Conduit pipe that receives binary data, parses it,
--   and outputs understandable Luci messages.
--   Messages may fail to read; in that case @Left error@ is returned instead of a message.
--
--   * Connect its input to a TCP source.
--   * Connect its output to a Luci message consumer.
--
-- There are validation requirements that a message must satisfy
-- (overwise, 'MsgValidationError' is returned):
--
-- * A message header must be a valid JSON.
-- * Overall attachment size must be equal to the sum of attachment sizes + (n+1)*8.
-- * All attachments must be referenced in a header at least once via 'AttachmentReference'.
parseMessages :: Monad m
              => Conduit ByteString m (Either ComError LuciMessage)
parseMessages = do
    hSizeBuf <- ConB.take 8
    if BSL.null hSizeBuf
    then return ()
    else do
      r <- runExceptT $ do
        hSize <- tryReadInt64be "header size int" hSizeBuf
        aSize <- lift (ConB.take 8)>>= tryReadInt64be "body size int"
        emsgVal <- tryYield (fromIntegral hSize) >>= tryDecode
        atts <- case aSize of
          0 -> return []
          s -> do
            hANum <- lift (ConB.take 8) >>= tryReadInt64be "attachment number int"
            (ss, atts) <- unzip <$> replicateM (fromIntegral hANum) tryReadAttachment
            -- check attachment sizes!
            if s /= sum ss + (hANum + 1) * 8
            then throwE . MsgValidationError $ "Attachment sizes mismatch"
            else do
              arefs <- lookupARefs (fromIntegral hANum) emsgVal
              when (length arefs /= length atts)
                . throwE . MsgValidationError $ "Found " ++
                    show (length atts)  ++ " attachments, but " ++
                    show (length arefs) ++ " references in a header"
              -- whether all attachments are correct
              mapM (\(aref,att) -> if checkAttachment aref att
                                   then return att
                                   else throwE . MsgValidationError $
                                      "Error validating attachment " ++ show (toEncoding aref)
                   ) $ zip arefs atts
        return (MessageHeader emsgVal, atts)
      yield r
      parseMessages
  where
    -- wrapper around Binary's runGetOrFail
    tryReadInt64be name bs = case Binary.runGetOrFail Binary.getInt64be bs of
                 Left  (_,_,msg) -> throwE . ByteReadingError $ "Decoding " ++ name ++ ": " ++ msg
                 Right (_,_,val) -> if val >= 0 then return val
                    else throwE . MsgValidationError
                         $ "Unexpected negative integer!"
    -- wrapper around Aeson's decode
    tryDecode bs = case JSON.eitherDecode' bs of
                 Left  msg -> throwE . MsgValidationError $ "JSON decoding: " ++ msg
                 Right val -> return val
    tryReadAttachment = do
      hSize <- lift (ConB.take 8) >>= tryReadInt64be "i-th attachment size int"
      a <- BSL.toStrict <$> tryYield (fromIntegral hSize)
      return (hSize, a)
    tryYield n = do
      bs <- lift (ConB.take n)
      if BSL.length bs < fromIntegral n
      then throwE . ByteReadingError $ "Not enough bytes available."
      else return bs

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

-- | Errors that may occur while getting messages from remote side.
data ComError
  = ByteReadingError !String    -- ^ Failed to read data from source.
  | MsgValidationError !String  -- ^ The message is corrupted or invalid.
  deriving (Eq, Show)



-- | Initiate a Luci panic recovery procedure:
--
-- 1. send /panic/ message;
-- 2. scan input for /panicID/;
-- 3. switch to normal mode when faces /panicID/.
-- Searching for /panicID/ is done using
-- <https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm Boyer-Moore-Horspool algorithm>
--
-- This conduit finishes its work when consumes /panicID/.
panicConduit :: (MonadRandom m, MonadIO m)
             => Conduit ByteString m ByteString
panicConduit = do
--    liftIO . putStrLn $ "PANIC: Starting panic!"
    panicID <- lift $ getRandomBytes 32
--    liftIO . putStrLn $ "PANIC: panicID: " ++ show panicID
    let panicMSG = MessageHeader $ object
           [ "panic" .= BSC.unpack (BS.encode panicID)]
    yield (panicMSG,[]) =$= writeMessages =$= awaitForever yield
    let -- length of pattern to search (BTW, must be 32)
        n = BS.length panicID
        -- shift values table
        ss = ST.runST $ do
          arr <- Prim.newArray 256 (fromIntegral n :: Word8)
          forM_ [1..n-1] $ \i ->
            Prim.writeArray arr (fromIntegral . BS.index panicID $ i-1) (fromIntegral $ n - i)
          Prim.unsafeFreezeArray arr
        -- calculate the shift according to a code of a symbol
        shift i = fromIntegral $ Prim.indexArray ss (fromIntegral i)
        -- search for the pattern (panicID), and then finish
        search bs = do
          if BS.length bs < n
          then do
            mbs <- await
            case mbs of
              Nothing -> return ()
              Just bs1 -> search (bs `BS.append` bs1)
          else do
            if BS.isPrefixOf panicID bs
            then do
              leftover (BS.drop n bs)
              liftIO . putStrLn $ "PANIC: calming! Put leftover back and finish"
            else do
              search (BS.drop (shift $ BS.index bs (n-1)) bs)
    search BS.empty

-- | Calm down Luci panic (response in panic recovery procedure):
--
-- 1. wait for /panic/ message (pass by other messages);
-- 2. decode /panicID/ from Base64, and send it in raw binary.
panicResponseConduit :: Monad m
                     => Conduit LuciMessage m LuciProcessing
panicResponseConduit = await >>= \m ->
  case m of
    -- no input - just finish
    Nothing -> return ()
    -- need to inspect the message
    Just msg@(MessageHeader val, _) -> do
      case findPanicId val of
        Nothing      -> yield (Right msg)
        Just panicID -> do
--          traceM $ "PANIC: Got a panic ID; send it back and continue."
          yield (Left panicID)
      panicResponseConduit
  where
    findPanicId (Object o) = case HashMap.lookup "panic" o of
      Nothing -> Nothing
      Just v  -> case JSON.fromJSON v of
         JSON.Error _ -> Nothing
         JSON.Success str -> Just . BS.decodeLenient $ BSC.pack str
    findPanicId _ = Nothing

----------------------------------------------------------------------------------------------------
-- Internal supplementary stuff
----------------------------------------------------------------------------------------------------


-- | Parse JSON Value to get a sorted list of attachment references.
lookupARefs :: Monad m => Int -> JSON.Value -> ExceptT ComError m [AttachmentReference]
lookupARefs n v = case JSON.fromJSON v of
  JSON.Error err -> throwE $ MsgValidationError err
  JSON.Success (GetARefs f) -> return . snd . unzip . sortOn fst
                             . HashMap.toList $ f [1..n]


-- | Lookup requested references.
newtype GetARefs = GetARefs ([Int] -> HashMap Int AttachmentReference)

-- | Zero value for GetARefs
noARefs :: GetARefs
noARefs = GetARefs $ \_ -> HashMap.empty

-- | Actual lookup of requested references.
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

-- | Supplementary function to fold JSON arrays and objects.
updateARefs :: JSON.Parser GetARefs -> JSON.Value -> JSON.Parser GetARefs
updateARefs pGAR v = pGAR >>= \(GetARefs f) -> do
        GetARefs g <- parseJSON v
        return . GetARefs $ \xs ->
            let hm1 = f xs
                found = sort $ HashMap.keys hm1
                rems = xs \\ found
            in case rems of
                [] -> hm1
                rs -> hm1 `HashMap.union` g rs
