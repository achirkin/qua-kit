{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Monad.IO.Class
import Control.Monad (join)
import Data.Conduit
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Builder as BSB

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base
import System.Environment (getArgs)
import Data.List (stripPrefix)
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid ((<>))

--import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control
import Control.Monad.Base (MonadBase)
import Crypto.Random (MonadRandom(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)


----------------------------------------------------------------------------------------------------

-- | Our program is a state machine.
--   We define the state type and operate with it when LuciProcessing
data LPState
  = NormalState Int -- ^ number of messages processed
  | WasInPanic Text
  deriving (Eq,Ord,Show)

initialState :: LPState
initialState = NormalState 0

simpleServerProcessing :: LuciConduit Text LuciProgram
simpleServerProcessing = do
  curState <- liftS get
  -- Just send back each message twice!
  case curState of
    WasInPanic t -> do
        yieldMessage (MessageHeader $ object [ "Iwasintrouble" .= t ], [])
        liftS . put $ NormalState 0
    _ -> return ()
  mmsg <- await
  case mmsg of
   Nothing -> throwLuciError "Finished connection!"
   Just msg -> do
      yieldMessage msg
      simpleServerProcessing

errorResponse :: LuciError Text -> LuciProgram ()
errorResponse err = do
  liftIO $ print err
  liftS' . put . WasInPanic . Text.pack $ show err

----------------------------------------------------------------------------------------------------

newtype LuciProgram t = LuciProgram { unProgram :: (StateT LPState (LoggingT IO) t) }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO)

instance MonadBaseControl IO LuciProgram where
  type StM LuciProgram a = (a, LPState)
  liftBaseWith f = LuciProgram $ liftBaseWith $ \q -> f (q . unProgram)
  restoreM = LuciProgram . restoreM

instance MonadRandom LuciProgram where
  getRandomBytes = liftIO . getRandomBytes

instance MonadLogger LuciProgram where
  monadLoggerLog a b c d = LuciProgram . lift $ monadLoggerLog a b c d

-- | lift state operations into conduit with our program
liftS :: StateT LPState (LoggingT IO) t -> ConduitM a (LuciProcessing Text b) LuciProgram t
liftS f = lift $ LuciProgram f

-- | lift state operations into our program
liftS' :: StateT LPState (LoggingT IO) t -> LuciProgram t
liftS' = LuciProgram

runLuciProgram :: LogLevel -> LuciProgram r -> IO LPState
runLuciProgram ll (LuciProgram p) = runStdoutLoggingT
                                  . filterLogger (\_ l -> l >= ll)
                                  $ execStateT p initialState

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Luci-connect testing app. Usage:\n\
             \luci-connect [args..]\n\
             \Parameters:\n\
             \\tserver       - run as server        (default: client)\n\
             \\tport=x       - choose port          (default: 7654)\n\
             \\thost=a.b.c.d - choose host          (default: 127.0.1.1)\n\
             \\tloglevel=lvl - choose logging level (default: info)\n\
             \\t   possible levels:  debug, info, warn, error\n"
    args <- getArgs
    let sets = setSettings args RunSettings
                  { host     = "127.0.1.1"
                  , port     = 7654
                  , run      = runClient
                  , logLevel = LevelInfo
                  }
    (run sets) (port sets) (host sets) (logLevel sets)
  where
    setSettings [] s = s
    setSettings ("server":xs) s = setSettings xs s{run = const . runServer}
    setSettings (par:xs) s = case stripPrefix "port=" par of
           Just n -> setSettings xs s{port = read n}
           Nothing -> case stripPrefix "host=" par of
             Just h -> setSettings xs s{host = BSC.pack h}
             Nothing -> case stripPrefix "loglevel=" par of
               Just "debug"   -> setSettings xs s{logLevel = LevelDebug}
               Just "info"    -> setSettings xs s{logLevel = LevelInfo}
               Just "warn"    -> setSettings xs s{logLevel = LevelWarn}
               Just "warning" -> setSettings xs s{logLevel = LevelWarn}
               Just "error"   -> setSettings xs s{logLevel = LevelError}
               Just h -> setSettings xs s{logLevel = LevelOther $ Text.pack h}
               Nothing -> setSettings xs s

data RunSettings = RunSettings
  { host :: ByteString
  , port :: Int
  , run :: Int -> ByteString -> LogLevel -> IO ()
  , logLevel :: LogLevel
  }

----------------------------------------------------------------------------------------------------

runServer :: Int -> LogLevel -> IO ()
runServer p llvl = do
    -- Connect using Luci.Connect
    putStrLn "Running echoing server"
    r <- runLuciProgram llvl $ talkAsLuci p Nothing errorResponse simpleServerProcessing
    putStrLn "\nLast state:"
    print r

-- | Run client sending some amount of staff
runClient :: Int -> ByteString -> LogLevel -> IO ()
runClient p h llvl = do
    -- Connect using Luci.Connect
    putStrLn "Running luci-connect client."
    r <- runLuciProgram llvl $ talkToLuciE p h (Just 2) errorResponse processMessages
    putStrLn "\nLast state:"
    print r
  where
    processMessages = do

      testMessage msgNoAtts

      testMessage msgEmpty

      testMessage msg2Att

      testMessage msg5Att

      testMessage msgSpecialAtt

      testMessage msg1Att

      testMessage msgCorruptHead

      testMessage msg2Att

      testMessage msgILongerHead
      testMessage msg2Att

      testMessage msgIShorterHead
      testMessage msg2Att

      testMessage msgIWrongNums1
      testMessage msg2Att

      testMessage msgIWrongNums2
      testMessage msg2Att

      testMessage msgIForgotAttach
      testMessage msg2Att

--      -- wait for all other input until the end
--      awaitForAWhile
--    awaitForAWhile = do
--      curState <- liftS get
--      case curState of
--        WasInPanic t -> do
--           liftIO . putStrLn $ "WASINPANICSTATE: " ++ Text.unpack t
--           liftS . put $ NormalState 0
--           logInfoN $ "Sending " <> desc msg2Att
--           yieldMessage $ message msg2Att
--        NormalState i -> do
--           mx <- await
--           case mx of
--             Nothing -> return ()
--             Just (Processing x)  -> do
--                liftIO . putStrLn $ "\nMessage N " ++ show i
--                logMsg $ Just x
--                liftS . put $ NormalState (i+1)
--                awaitForAWhile

testMessage :: MonadLogger m
            => MessageTest e m
            -> LuciConduitE e m
testMessage m = do
  logInfoN $ "Sending " <> desc m
  case corruption m of
    Nothing -> yieldMessage $ message m
    Just co -> co $ message m
  mr <- await
  case mr of
    Nothing -> logWarnN "End of input."
    Just (ProcessingError LuciTimedOut) -> logWarnN "Timed out"
    Just (Processing r) -> if r == message m || not (valid m)
      then logInfoN "Test passed."
      else logErrorN $ "Returned message is different from the message sent! ("
          <> Text.pack (show r) <> ")"
    Just (ProcessedData d) -> logDebugN "Passing by processed data" >> yield (ProcessedData d)
    Just (ProcessingError err) -> if valid m
      then logErrorN $ "Got error, even though a sent message was valid " <> Text.pack (show err) <> ")"
      else logInfoN $ "Test passed (" <> Text.pack (show err) <> ")"

--awaitTimeouts :: MonadIO m
--              => Double
--              -> Conduit (Maybe a) m b
--              -> Conduit a m b
--awaitTimeouts t c = do
--    x <- liftIO newEmptyMVar
--    _ <- liftIO . forkIO . runConduit . awaitForever $ liftIO . putMVar x
--    liftIO . threadDelay $ round (t*1000000)
--    return undefined
--  where
--    yield

data MessageTest e m = MessageTest
  { message :: LuciMessage
  , desc    :: Text
  , valid   :: Bool
  , corruption :: Maybe (LuciMessage -> LuciConduitE e m)
  }

msgNoAtts :: MessageTest e m
msgNoAtts = MessageTest
 { message = (MessageHeader msg, [])
 , desc = d
 , valid = v
 , corruption = Nothing
 } where
    d = "Normal message without attachments"
    v = True
    msg = object
      [ "messageType" .= d
      , "valid" .= v
      ]

msgEmpty :: MessageTest e m
msgEmpty = MessageTest
 { message = (MessageHeader msg, [])
 , desc = "Empty message"
 , valid = True
 , corruption = Nothing
 } where
    msg = object []


msg1Att :: MessageTest e m
msg1Att = MessageTest
 { message = (MessageHeader msg, [bs0])
 , desc = d
 , valid = v
 , corruption = Nothing
 } where
    d = "Normal message with 1 attachment"
    v = True
    bs0 = "Hello byte world!"
    bs0ref = makeAReference bs0 "ByteString" 1 (Just "Hello.world")
    msg = object
      [ "messageType" .= d
      , "valid" .= v
      , "file" .= bs0ref
      ]

msg2Att :: MessageTest e m
msg2Att = MessageTest
 { message = (MessageHeader msg, [bs0, bs1])
 , desc = d
 , valid = v
 , corruption = Nothing
 } where
    d = "Normal message with 2 attachments"
    v = True
    bs0 = "Hello byte world!"
    bs1 = "Second good attachment"
    msg = object
      [ "messageType" .= d
      , "valid" .= v
      ]

msg5Att :: MessageTest e m
msg5Att = MessageTest
 { message = (MessageHeader msg, [bs0, bs1, bs2, bs3, bs4])
 , desc = d
 , valid = v
 , corruption = Nothing
 } where
    d = "Normal message with 5 attachments"
    v = True
    bs0 = "Hello byte world!"
    bs1 = "Second good attachment"
    bs2 = bs1
    bs3 = bs1
    bs4 = "Last attachment. It is a bit longer..."
              <> BS.pack (replicate 17 0)
              <> BS.pack (join $ replicate 13 [0..255])
              <> ". There was it, we're done!"
    msg = object
      [ "messageType" .= d
      , "valid" .= v
      ]

msgSpecialAtt :: MessageTest e m
msgSpecialAtt = MessageTest
 { message = (MessageHeader msg, [bs0, bs1])
 , desc = d
 , valid = v
 , corruption = Nothing
 } where
    d = "Message with 2 empty attachments. Note, it is valid!"
    v = True
    bs0 = ""
    bs1 = ""
    msg = object
      [ "messageType" .= d
      , "valid" .= v
      ]


logMsg :: MonadIO m => Maybe LuciMessage -> m ()
logMsg Nothing = liftIO $ putStrLn "\nFinished session."
logMsg (Just (val, bss)) = liftIO $ do
  putStrLn "\nReceived:"
  print val
  mapM_ print bss


---- | Use "Luci.Messages" to construct a valid message
--remoteRegister :: (MessageHeader, [ByteString])
--remoteRegister = (toMsgHead jsv, [])
--  where
--    jsv = RemoteRegister
--        { exampleCall = JSON.object []
--        , serviceName = "CoolTestService"
--        , inputs  = Nothing
--        , outputs = Nothing
--        }
--
---- | Construct a message inplace
--testFileEcho :: (MessageHeader, [ByteString])
--testFileEcho = (MessageHeader msg,[bs0, bs1])
--  where
--    bs0 = "Hello byte world!"
--    bs0ref = makeAReference bs0 "ByteString" 1 (Just "Hello.world")
--    bs1 = "Second attachment as"
--    bs1ref = makeAReference bs1 "ByteString" 2 Nothing
--    msg = object
--     [ "run" .= ("test.FileEcho" :: Text)
--     , "myCoolFile" .= bs0ref
--     , "myPoorFile" .= bs0ref
--     , "anotherThing" .= bs1ref
--     ]
--
---- | Construct a message inplace
--testCorruptedFileEcho :: (MessageHeader, [ByteString])
--testCorruptedFileEcho = (MessageHeader msg,[bs0, bs1, bs1])
--  where
--    bs0 = "Hello byte world!"
--    bs0ref = makeAReference bs0 "ByteString" 1 (Just "Hello.world")
--    bs1 = "Second attachment as"
--    bs1ref = makeAReference bs1 "ByteString" 2 Nothing
--    msg = object
--     [ "run" .= ("test.FileEcho" :: Text)
--     , "myCoolFile" .= bs0ref
--     , "myPoorFile" .= bs0ref
--     , "anotherThing" .= bs1ref
--     ]

msgCorruptHead :: Monad m => MessageTest e m
msgCorruptHead = msg1Att
  { corruption = Just yieldMessageCorruptHeader
  , desc = "a valid message with corrupted header"
  }
yieldMessageCorruptHeader :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldMessageCorruptHeader (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = BS.map (+1) . BSL.toStrict $ JSON.encode msg
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE

msgILongerHead :: Monad m => MessageTest e m
msgILongerHead = msg1Att
  { corruption = Just yieldInvalidMessageLongerHeader
  , valid = False
  , desc = "a corrupted message with longer header"
  }
yieldInvalidMessageLongerHeader :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldInvalidMessageLongerHeader (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = (BSL.toStrict $ JSON.encode msg) <> "!"
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart - 1)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE

msgIShorterHead :: Monad m => MessageTest e m
msgIShorterHead = msg1Att
  { corruption = Just yieldInvalidMessageShorterHeader
  , valid = False
  , desc = "a corrupted message with shorter header"
  }
yieldInvalidMessageShorterHeader :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldInvalidMessageShorterHeader (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = BS.drop 1 (BSL.toStrict $ JSON.encode msg)
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE


msgIWrongNums1 :: Monad m => MessageTest e m
msgIWrongNums1 = msg1Att
  { corruption = Just yieldInvalidMessageWrongNumbers1
  , valid = False
  , desc = "a corrupted message with wrong binary numbers (1)"
  }
yieldInvalidMessageWrongNumbers1 :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldInvalidMessageWrongNumbers1 (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = 8 * length atts + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE

msgIWrongNums2 :: Monad m => MessageTest e m
msgIWrongNums2 = msg1Att
  { corruption = Just yieldInvalidMessageWrongNumbers2
  , valid = False
  , desc = "a corrupted message with wrong binary numbers (2)"
  }
yieldInvalidMessageWrongNumbers2 :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldInvalidMessageWrongNumbers2 (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt atts
  where
    writeAtt bs = do
      yieldInt64be ((\x -> x - 1) . fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE

msgIForgotAttach :: Monad m => MessageTest e m
msgIForgotAttach = msg1Att
  { corruption = Just yieldInvalidMessageForgotAttach
  , valid = False
  , desc = "a corrupted message with calculated, but not added attachment"
  }
yieldInvalidMessageForgotAttach :: Monad m
                          => LuciMessage -> LuciConduitE e m
yieldInvalidMessageForgotAttach (msg, atts) =  mapOutput ProcessedData $ do
      let mainpart = BSL.toStrict $ JSON.encode msg
          attSize = 8 * (length atts + 1) + foldr (\a s -> s + BS.length a) 0 atts
      yieldInt64be (fromIntegral $ BS.length mainpart)
      yieldInt64be (fromIntegral attSize)
      yield mainpart
      yieldInt64be (fromIntegral $ length atts)
      mapM_ writeAtt (drop 1 atts)
  where
    writeAtt bs = do
      yieldInt64be (fromIntegral $ BS.length bs)
      yield bs
    yieldInt64be = yield . BSL.toStrict . BSB.toLazyByteString . BSB.int64BE
