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
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Aeson as JSON
import           Data.Text (Text)
import           Data.ByteString (ByteString)

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
    args <- getArgs
    case args of
      "server":_ -> runServer 7654
      _ -> runClient 7654 "127.0.1.1"
--    -- Test using Luci.Connect.Base
--    putStrLn "\nRun short-circuited conduit pipe..."
--    runConduit $ (yield testFileEcho >> yield remoteRegister)
--      =$= writeMessages =$= parseMessages =$= Conduit.mapM_ (logMsgE . Just)

runServer :: Int -> IO ()
runServer port = do
    -- Connect using Luci.Connect
    putStrLn "Runing echoing server"
    talkAsLuciExt port  $ logMessages "SERVER INPUT LOG"
                       =$= processMessages
                       =$= mapOutput Left writeMessages
  where
    processMessages = do
      -- Just send back each message twice!
      mmsg <- await
      case mmsg of
       Nothing -> liftIO $ putStrLn "Finished connection!"
       Just msg -> do
          yield msg
          yield msg
          processMessages

runClient :: Int -> ByteString -> IO ()
runClient port host = do
    -- Connect using Luci.Connect
    putStrLn "\n\nRun conduit pipes connected to Luci..."
    talkToLuciExt port host $ ( do
      -- send test message
      yield testFileEcho
      -- send RemoteRegister request
      yield remoteRegister
      -- wait for couple messages for fun
      await >>= liftIO . logMsg
      await >>= liftIO . logMsg

      -- send corrupted message!
      yield testCorruptedFileEcho
      -- wait for one more message
      await >>= liftIO . logMsg

      -- right after, send couple more good messages
      yield testFileEcho
      yield testFileEcho
      await >>= liftIO . logMsg

      -- wait a bit and send again couple messages
      liftIO $ putStrLn "Sleep for a second..."
      liftIO $ threadDelay 1000000
      liftIO $ putStrLn "            ...wake up!"
      yield remoteRegister
      yield testFileEcho
      yield testFileEcho
      yield testFileEcho
      yield testFileEcho

      -- One more time!
      liftIO $ putStrLn "Sleep for a second..."
      liftIO $ threadDelay 1000000
      liftIO $ putStrLn "            ...wake up!"
      yield remoteRegister
      yield testFileEcho
      yield testFileEcho
      yield testFileEcho
      yield testFileEcho

      -- wait for all other input until the end
      awaitForever $ liftIO . logMsg . Just

      ) =$= mapOutput Left writeMessages
        =$= logProcessing "OUTPUT LOG"


logMsg :: Maybe LuciMessage -> IO ()
logMsg Nothing = putStrLn "\nFinished session."
logMsg (Just (val, bss)) = do
  putStrLn "\nReceived:"
  print val
  mapM_ print bss

-- | Use "Luci.Messages" to construct a valid message
remoteRegister :: (MessageHeader, [ByteString])
remoteRegister = (toMsgHead jsv, [])
  where
    jsv = RemoteRegister
        { exampleCall = JSON.object []
        , serviceName = "CoolTestService"
        , inputs  = Nothing
        , outputs = Nothing
        }

-- | Construct a message inplace
testFileEcho :: (MessageHeader, [ByteString])
testFileEcho = (MessageHeader msg,[bs0, bs1])
  where
    bs0 = "Hello byte world!"
    bs0ref = makeAReference bs0 "ByteString" 1 (Just "Hello.world")
    bs1 = "Second attachment as"
    bs1ref = makeAReference bs1 "ByteString" 2 Nothing
    msg = object
     [ "run" .= ("test.FileEcho" :: Text)
     , "myCoolFile" .= bs0ref
     , "myPoorFile" .= bs0ref
     , "anotherThing" .= bs1ref
     ]

-- | Construct a message inplace
testCorruptedFileEcho :: (MessageHeader, [ByteString])
testCorruptedFileEcho = (MessageHeader msg,[bs0, bs1, bs1])
  where
    bs0 = "Hello byte world!"
    bs0ref = makeAReference bs0 "ByteString" 1 (Just "Hello.world")
    bs1 = "Second attachment as"
    bs1ref = makeAReference bs1 "ByteString" 2 Nothing
    msg = object
     [ "run" .= ("test.FileEcho" :: Text)
     , "myCoolFile" .= bs0ref
     , "myPoorFile" .= bs0ref
     , "anotherThing" .= bs1ref
     ]
