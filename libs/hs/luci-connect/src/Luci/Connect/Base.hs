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
-- |           [ 16B ] [    x bytes           ] [                  y bytes                      ]  |
-- -------------------------------------------------------------------------------------------------
--   message total size:      16 + x + y bytes
--   message validation rule: y = (n+1)*8 + sum(y1..yn)
--
--   x   :: Int64BE         -- 8 byte signed big-endian integer - bytesize of message header
--   y   :: Int64BE         -- 8 byte signed big-endian integer - bytesize of binary message body
--   msg :: JSON            -- x bytes of UTF-8 encoded json    - message header
--   n   :: Int64BE         -- 8 byte signed big-endian integer - number of attachments
--   repeat n times:
--     y(i)   :: Int64BE    -- 8 byte signed big-endian integer - bytesize of i-th attachment
--     att(i) :: ByteString -- y(i) bytes of raw data - i-th attachment
-- @
--
-- All communication parties must check the /message validation rule/:
-- @y = (n+1)*8 + sum(y1..yn)@.
-- In case if the validation check is found to be violated,
--    /panic recovery procedure/ should be started.
--
--  Note, this specification covers only /low-level/ part of Luci communication.
--  It specifies only binary consistency of message parts,
--    but does not do any of JSON header checks.
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, OverloadedStrings, Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, TemplateHaskell #-}

module Luci.Connect.Base
    ( -- * Main types
      MessageHeader (..), toMsgHead, fromMsgHead
    , LuciMessage
    , LuciConduit, LuciConduitE
      -- * Message processing
      -- | Write conduits for message processing and connect them using
      --   the '=&=' combinator.
    , (=&=), (=$=), await, yield
    , yieldMessage, throwLuciError
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
    , ComError (..), LuciError (..)
      -- * Panic recovery procedure
      -- | If a client become uncertain whether it has read all bytes of previous message or not,
      --   it can initiate a /panic recovery procedure/ (PRC).
      --   The panic recovery procedure may be initiated by either side of a communication session.
      --   The one and only rule to start PRC is the validation failure:
      --      @ y != (n+1)*8 + sum(y1..yn) @.
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
      --      (I recommend to use <https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm Boyer-Moore-Horspool algorithm>)
      --
      --   The panic recovery procedure complements the validation mechanism of Luci messages:
      --   an initiator should initiate the panic if
      --   it faces unexpected input or message parts' sizes do not agree (the validation check is violated).
    , panicConduit
    , panicResponseConduit
      -- * Internal types and functions
    , LuciProcessing (..)
    , LuciProcessingT (..)
    , mapLuciProcessingT, withLuciError, withLuciErrorT, throwLuciError'
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Classes

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif


import qualified Control.Monad.ST as ST
import           Control.Monad.Logger
import           Crypto.Random (getRandomBytes, MonadRandom)
import qualified Data.Aeson as JSON
import           Data.Monoid
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
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import           Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Primitive.Array as Prim
import           Data.String (IsString)
import           Data.Word



import Luci.Connect.Internal



----------------------------------------------------------------------------------------------------
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

-- | Alias for Luci message containing a header and attachments
type LuciMessage = (MessageHeader, [ByteString])
----------------------------------------------------------------------------------------------------

-- | Luci message processing state
data LuciProcessing e r
  = ProcessingError e -- ^ Some error occurred
  | ProcessedData ByteString -- ^ Data succesfully processed and answer is ready
  | Processing r -- ^ Still in processing
  deriving (Eq, Show, Ord, Read)

instance Functor (LuciProcessing e) where
  fmap _ (ProcessingError e) = ProcessingError e
  fmap _ (ProcessedData d) = ProcessedData d
  fmap f (Processing m) = Processing (f m)

instance Applicative (LuciProcessing e) where
  pure = Processing
  ProcessingError e <*> _ = ProcessingError e
  ProcessedData d   <*> _ = ProcessedData d
  Processing a      <*> Processing b = Processing $ a b
  Processing _      <*> ProcessingError e = ProcessingError e
  Processing _      <*> ProcessedData   d = ProcessedData d

instance Monad (LuciProcessing e) where
  return = pure
  ProcessingError e >>= _ = ProcessingError e
  ProcessedData d   >>= _ = ProcessedData d
  Processing a      >>= pb = pb a

instance MonadFix (LuciProcessing e) where
  mfix f = let a = f (unProcessing a) in a
             where unProcessing (Processing x) = x
                   unProcessing (ProcessingError _) = error "mfix LuciProcessing: ProcessingError"
                   unProcessing (ProcessedData _) = error "mfix LuciProcessing: ProcessedData"

-- | Get the best among two processing results;
--   priority: ProcessedData > Processing > ProcessingError.
--   For Processing and ProcessingError returns the left.
--   For ProcessedData works as monoid.
instance Monoid e => Alternative (LuciProcessing e) where
  empty = ProcessedData mempty
  ProcessedData d <|> ProcessedData d'  = ProcessedData $ d <> d'
  ProcessedData d <|> _  = ProcessedData d
  _ <|> ProcessedData d  = ProcessedData d
  Processing p <|> _ = Processing p
  _ <|> Processing p = Processing p
  ProcessingError e1 <|>  ProcessingError e2 = ProcessingError $ e1 <> e2

  some (ProcessedData d) = ProcessedData d
  some (ProcessingError e) = ProcessingError e
  some (Processing p) = Processing $ repeat p

  many (ProcessedData d) = ProcessedData d
  many (ProcessingError _) = Processing []
  many (Processing p) = Processing $ repeat p

-- | Transformer for 'LuciProcessing' data type.
newtype LuciProcessingT e m r = LuciProcessingT
  { runLuciProcessingT :: m (LuciProcessing e r) }

instance (Eq e, Eq1 m) => Eq1 (LuciProcessingT e m) where
    eq1 (LuciProcessingT x) (LuciProcessingT y) = eq1 x y
instance (Ord e, Ord1 m) => Ord1 (LuciProcessingT e m) where
    compare1 (LuciProcessingT x) (LuciProcessingT y) = compare1 x y
instance (Read e, Read1 m) => Read1 (LuciProcessingT e m) where
    readsPrec1 = readsData (readsUnary1 "LuciProcessingT" LuciProcessingT)
instance (Show e, Show1 m) => Show1 (LuciProcessingT e m) where
    showsPrec1 d (LuciProcessingT m) = showsUnary1 "LuciProcessingT" d m
instance (Eq e, Eq1 m, Eq a) => Eq (LuciProcessingT e m a) where (==) = eq1
instance (Ord e, Ord1 m, Ord a) => Ord (LuciProcessingT e m a) where compare = compare1
instance (Read e, Read1 m, Read a) => Read (LuciProcessingT e m a) where readsPrec = readsPrec1
instance (Show e, Show1 m, Show a) => Show (LuciProcessingT e m a) where showsPrec = showsPrec1

-- | Map the unwrapped computation using the given function.
--
-- * @'runLuciProcessingT' ('mapLuciProcessingT' f m) = f ('runLuciProcessingT' m)@
mapLuciProcessingT :: (m (LuciProcessing e a) -> n (LuciProcessing e' b))
        -> LuciProcessingT e m a
        -> LuciProcessingT e' n b
mapLuciProcessingT f m = LuciProcessingT $ f (runLuciProcessingT m)

-- | Transform any exceptions thrown by the computation using the
-- given function.
withLuciError :: (e -> e') -> LuciProcessing e a -> LuciProcessing e' a
withLuciError f (ProcessingError e) = ProcessingError $ f e
withLuciError _ (ProcessedData d) = ProcessedData d
withLuciError _ (Processing p) = Processing p

-- | Transform any exceptions thrown by the computation using the
-- given function.
withLuciErrorT :: (Functor m)
               => (e -> e') -> LuciProcessingT e m a -> LuciProcessingT e' m a
withLuciErrorT = mapLuciProcessingT . fmap . withLuciError

-- | Put raw data into LuciProcessing pipeline
yieldMessage :: MonadLogger m
             => LuciMessage
             -> Conduit a m (LuciProcessing e LuciMessage)
yieldMessage msg = yield msg =$= mapOutput ProcessedData writeMessages


-- | Throw an error from within a conduit, much like 'ExceptT'.
throwLuciError :: Monad m => e -> Conduit a m (LuciProcessing e b)
throwLuciError = yield . ProcessingError

-- | Throw an error, much like 'ExceptT'.
throwLuciError' :: Monad m => e -> LuciProcessingT e m a
throwLuciError' = LuciProcessingT . return . ProcessingError

instance Functor m => Functor (LuciProcessingT e m) where
    fmap f = LuciProcessingT . fmap (fmap f) . runLuciProcessingT

instance Foldable f => Foldable (LuciProcessingT e f) where
    foldMap f (LuciProcessingT a) = foldMap g a
      where
        g (Processing p) = f p
        g _ = mempty

instance Traversable f => Traversable (LuciProcessingT e f) where
    traverse f (LuciProcessingT a) =
        LuciProcessingT <$> traverse g a
      where
        g (Processing p) = Processing <$> f p
        g (ProcessedData d) = pure $ ProcessedData d
        g (ProcessingError e) = pure $ ProcessingError e

instance Applicative m => Applicative (LuciProcessingT e m) where
    pure a = LuciProcessingT $ pure (Processing a)
    LuciProcessingT f <*> LuciProcessingT v = LuciProcessingT $ fmap (<*>) f <*> v

instance (Applicative m, Monoid e) => Alternative (LuciProcessingT e m) where
    empty = LuciProcessingT $ pure empty
    LuciProcessingT mx <|> LuciProcessingT my = LuciProcessingT $ fmap (<|>) mx <*> my

instance Monad m => Monad (LuciProcessingT e m) where
    return = LuciProcessingT . return . pure
    m >>= k = LuciProcessingT $ do
      a <- runLuciProcessingT m
      case a of
        ProcessingError e -> return (ProcessingError e)
        ProcessedData d -> return (ProcessedData d)
        Processing p -> runLuciProcessingT (k p)
    fail = LuciProcessingT . fail

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (LuciProcessingT e m) where
    fail = LuciProcessingT . Fail.fail
#endif

instance MonadFix m => MonadFix (LuciProcessingT e m) where
    mfix f = LuciProcessingT (mfix (runLuciProcessingT . f . unProcessing))
      where
        unProcessing (Processing x) = x
        unProcessing (ProcessingError _) = error "mfix LuciProcessing: ProcessingError"
        unProcessing (ProcessedData _) = error "mfix LuciProcessing: ProcessedData"

instance MonadTrans (LuciProcessingT e) where
    lift = LuciProcessingT . fmap Processing

instance (MonadIO m) => MonadIO (LuciProcessingT e m) where
    liftIO = lift . liftIO

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (LuciProcessingT e m) where
    mzipWith f (LuciProcessingT a) (LuciProcessingT b) = LuciProcessingT $ mzipWith (liftA2 f) a b
#endif

instance (MonadLogger m) => MonadLogger(LuciProcessingT e m) where
  monadLoggerLog a b c = lift . monadLoggerLog a b c


-- | Alias for a processing conduit.
type LuciConduit e m = ConduitM LuciMessage (LuciProcessing e LuciMessage) m ()

-- | Alias for a processing conduit (with errors not filtered out from an upstream).
type LuciConduitE e m = Conduit (LuciProcessing ComError LuciMessage) m
                                (LuciProcessing (LuciError e) LuciMessage)


-- | Conduit pipe that receives Luci messages, encodes them,
--   and outputs plain bytestring.
--
--   * Connect its input to a Luci message producer.
--   * Connect its output to a TCP sink.
writeMessages :: MonadLogger m
              => Conduit LuciMessage m ByteString
writeMessages = do
  minput <- await
  case minput of
    Nothing -> return ()
    Just (msg, atts) -> do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
      $(logDebug) $ "Write: written msg" <>
         if null atts then "." else " (" <> Text.pack (show $ length atts) <> " atts)."
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
parseMessages :: MonadLogger m
              => Conduit ByteString m (LuciProcessing ComError LuciMessage)
parseMessages = do
    hSizeBuf <- ConB.take 8
    unless (BSL.null hSizeBuf) $
      do r <- runLuciProcessingT $
                do hSize <- tryReadInt64be "header size int" hSizeBuf
                   aSize <- lift (ConB.take 8) >>= tryReadInt64be "body size int"
                   emsg <- tryAwait (fromIntegral hSize)
                   hANum <- lift (ConB.take 8) >>=
                              tryReadInt64be "attachment number int"
                   when (aSize < (hANum + 1) * 8) $
                     throwLuciError' . MsgValidationError $
                       "Attachment sizes mismatch: aSize < (hANum + 1) * 8"
                   (ss, atts) <- unzip <$> tryReadAttachments (aSize - 8) hANum
                   when (aSize /= sum ss + (hANum + 1) * 8) $
                     throwLuciError' . MsgValidationError $
                       "Attachment sizes mismatch: aSize /= sum ss + (hANum + 1) * 8"
                   emsgVal <- tryDecode emsg
                   $( logDebug ) $
                     "PARSE: parsed msg" <>
                       if hANum == 0 then "." else
                         " (" <> Text.pack (show hANum) <> " atts)."
                   return (MessageHeader emsgVal, atts)
         yield r
         parseMessages
  where
    -- wrapper around Binary's runGetOrFail
    tryReadInt64be name bs = case Binary.runGetOrFail Binary.getInt64be bs of
                 Left  (_,_,msg) -> throwLuciError' . ByteReadingError $ "Decoding " ++ name ++ ": " ++ msg
                 Right (_,_,val) ->
                    if val >= 0
                    then return val
                    else throwLuciError' . MsgValidationError $ "Unexpected negative integer!"
    -- wrapper around Aeson's decode
    tryDecode bs = case JSON.eitherDecode' bs of
                 Left  msg -> throwLuciError' . JSONError $ "JSON decoding: " ++ msg
                 Right val -> return val
    tryReadAttachments _ 0 = return []
    tryReadAttachments aRemS aRemN = do
      hSize <- lift (ConB.take 8) >>= tryReadInt64be "i-th attachment size int"
      -- inter-attachment-check
      when (hSize > aRemS - 8*aRemN)
          $ throwLuciError' . MsgValidationError $ "Attachment sizes mismatch ("
                                                 <> show aRemN
                                                 <> " attachments have not been read)."
      a <- BSL.toStrict <$> tryAwait (fromIntegral hSize)
      ((hSize, a):) <$> tryReadAttachments (aRemS - 8 - hSize) (aRemN - 1)
    tryAwait n = do
      bs <- lift (ConB.take n)
      if BSL.length bs < fromIntegral n
      then throwLuciError' . ByteReadingError $ "Not enough bytes available."
      else return bs

-- | Errors that may occur while getting messages from remote side.
data ComError
  = ByteReadingError !String    -- ^ Failed to read data from source.
  | MsgValidationError !String  -- ^ The message is corrupted or invalid.
  | JSONError !String           -- ^ Failed to parse header
  | LuciTimedOut                -- ^ We have not received an expected message under specified timeout
  deriving (Eq, Show)

-- | All possible errors, customizeable by a user app
data LuciError e
  = LuciComError ComError    -- ^ Luci communication error
  | LuciClientError e        -- ^ User error
  | LuciMsgLeft LuciMessage  -- ^ Leftover unprocessed (unexpected by a user) message
  deriving (Eq, Show)



-- | Fusion operator, combining two Luci @Conduit@s together into a new @Conduit@.
--
-- Both @Conduit@s will be closed when the newly-created @Conduit@ is closed.
--
-- Leftover data returned from the right @Conduit@ will be discarded.
--
infixr 3 =&=
(=&=) :: Monad m => Conduit a m (LuciProcessing e b)
                 -> Conduit b m (LuciProcessing e c)
                 -> Conduit a m (LuciProcessing e c)
ConduitM left0 =&= ConduitM right0 = ConduitM $ \rest ->
    let
        goRight final left right =
            case right of
                HaveOutput p c o  -> HaveOutput (recurse p) (c >> final) o
                NeedInput rp rc   -> goLeft rp rc final left
                Done r2           -> PipeM (final >> return (rest r2))
                PipeM mp          -> PipeM (fmap recurse mp)
                Leftover right' i -> goRight final (HaveOutput left final $ Processing i) right'
          where
            recurse = goRight final left

        -- go here if we need more input
        goLeft rp rc final left =
            case left of
                HaveOutput left' final' (Processing o) -> goRight final' left' (rp o)
                HaveOutput left' final' (ProcessedData d) ->
                      HaveOutput (goRight final' left' $ NeedInput rp rc) final (ProcessedData d)
                HaveOutput left' final' (ProcessingError e) ->
                      HaveOutput (goRight final' left' $ NeedInput rp rc) final (ProcessingError e)
                NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
                Done r1                   -> goRight (return ()) (Done r1) (rc r1)
                PipeM mp                  -> PipeM (fmap recurse mp)
                Leftover left' i          -> Leftover (recurse left') i
          where
            recurse = goLeft rp rc final

     in goRight (return ()) (left0 Done) (right0 Done)
{-# INLINE [1] (=&=) #-}






-- | Initiate a Luci panic recovery procedure:
--
-- 1. send /panic/ message;
-- 2. scan input for /panicID/;
-- 3. switch to normal mode when faces /panicID/.
-- Searching for /panicID/ is done using
-- <https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm Boyer-Moore-Horspool algorithm>
--
-- This conduit finishes its work when consumes /panicID/.
panicConduit :: (MonadRandom m, MonadLogger m)
             => Conduit ByteString m ByteString
panicConduit = do
    panicID <- lift $ getRandomBytes 32
    let pIDEnc = Text.decodeUtf8 (BS.encode panicID)
        panicMSG = MessageHeader $ object
           [ "panic" .= pIDEnc]
    logInfoN $ "PANIC: sending panic message; base64 panicID = " <> pIDEnc
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
        search bs
          | BS.length bs < n =
            do mbs <- await
               case mbs of
                   Nothing -> return ()
                   Just bs1 -> search (bs `BS.append` bs1)
          | BS.isPrefixOf panicID bs =
            do leftover (BS.drop n bs)
               logInfoN "PANIC: found panicID; calming."
          | otherwise =
            do let toDrop = shift $ BS.index bs (n - 1)
               $( logDebug ) $
                 "PANIC: dropping " <> Text.pack (show toDrop) <> " bytes of data."
               search (BS.drop toDrop bs)
    search BS.empty





-- | Calm down Luci panic (response in panic recovery procedure):
--
-- 1. wait for /panic/ message (pass by other messages);
-- 2. decode /panicID/ from Base64, and send it in raw binary.
panicResponseConduit :: MonadLogger m
                     => LuciConduit e m
panicResponseConduit = await >>= \m ->
  case m of
    -- no input - just finish
    Nothing -> return ()
    -- need to inspect the message
    Just msg@(MessageHeader val, _) -> do
      case findPanicId val of
        Nothing      -> yield $ Processing msg
        Just panicID -> do
          logInfoN "PANIC RESPONSE: found panic message and sending raw paniID back."
          yield $ ProcessedData panicID
      panicResponseConduit
  where
    findPanicId (Object o) = case HashMap.lookup "panic" o of
      Nothing -> Nothing
      Just v  -> case JSON.fromJSON v of
         JSON.Error _ -> Nothing
         JSON.Success str -> Just . BS.decodeLenient $ BSC.pack str
    findPanicId _ = Nothing

