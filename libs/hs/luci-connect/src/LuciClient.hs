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

import Control.Monad.IO.Class
import Data.Conduit
import Data.Aeson as JSON
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base
import System.Environment (getArgs)
import Data.List (stripPrefix)

main :: IO ()
main = do
    args <- getArgs
    let sets = setSettings args RunSettings
                  { host = "127.0.1.1"
                  , port = 7654
                  , run = runClient
                  }
    (run sets) (port sets) (host sets)
  where
    setSettings [] s = s
    setSettings ("server":xs) s = setSettings xs s{run = const . runServer}
    setSettings ("nopanic":xs) s = setSettings xs s{run = runClientNoPanic}
    setSettings ("panic":xs) s = setSettings xs s{run = runClient}
    setSettings (par:xs) s = case stripPrefix "port=" par of
                               Just n -> setSettings xs s{port = read n}
                               Nothing -> case stripPrefix "host=" par of
                                   Just h -> setSettings xs s{host = BSC.pack h}
                                   Nothing -> setSettings xs s

data RunSettings = RunSettings
  { host :: ByteString
  , port :: Int
  , run :: Int -> ByteString -> IO ()
  }


resend :: Maybe LuciMessage -> LuciConduit IO ()
resend Nothing = return ()
resend (Just msg) = yieldMsg msg

runServer :: Int -> IO ()
runServer p = do
    -- Connect using Luci.Connect
    putStrLn "Runing echoing server"
    talkAsLuciExt p resend $ logProcessing "SERVER INPUT LOG"
                       =$= processMessages
                       =$= logProcessing "SERVER OUTPUT LOG"
  where
    processMessages = do
      -- Just send back each message twice!
      mmsg <- awaitMsg
      case mmsg of
       Nothing -> liftIO $ putStrLn "Finished connection!"
       Just msg -> do
          yieldMsg msg
          yieldMsg msg
          processMessages

runClientNoPanic :: Int -> ByteString -> IO ()
runClientNoPanic p h = do
    -- Connect using Luci.Connect
    putStrLn "\n\nRun conduit pipes connected to Luci (No Panic Recovery Procedure)..."
    talkToLuci p h $ processMessages -- =$= logProcessing "OUTPUT LOG"
  where
    processMessages = do
--      yieldMsg testFileEcho
--      yieldMsg remoteRegister
      yieldMsg testFileEcho
      yieldMsg testCorruptedFileEcho
      yieldMsg testFileEcho
      -- wait for all other input until the end
      awaitForever $ \x -> case x of
                             Left b    -> liftIO $ print b
                             Right msg -> logMsg $ Just msg


-- | Run client sending some amount of staff
runClient :: Int -> ByteString -> IO ()
runClient p h = do
    -- Connect using Luci.Connect
    putStrLn "\n\nRun conduit pipes connected to Luci..."
    talkToLuciExt p h resend $ processMessages =$= logProcessing "OUTPUT LOG"
  where
    processMessages = do
--      -- send test message
      yieldMsg testFileEcho
--      -- send RemoteRegister request
      yieldMsg remoteRegister
--      -- wait for couple messages for fun
--      awaitMsg >>= logMsg
      awaitMsg >>= logMsg

      -- send corrupted message!
      yieldMsg testCorruptedFileEcho
      awaitMsg >>= logMsg
      yieldMsg testCorruptedFileEcho
      -- wait for one more message
--      awaitMsg >>= logMsg
--
--      -- right after, send couple more good messages
      yieldMsg testFileEcho
--      yieldMsg testFileEcho
      awaitMsg >>= logMsg
--
--      -- wait a bit and send again couple messages
--      liftIO $ putStrLn "Sleep for a second..."
--      liftIO $ threadDelay 1000000
--      liftIO $ putStrLn "            ...wake up!"
--      yieldMsg remoteRegister
--      yieldMsg testFileEcho


      -- wait for all other input until the end
      awaitForever $ \x -> case x of
                             Left b    -> yield  $ Left b
                             Right msg -> logMsg $ Just msg



logMsg :: MonadIO m => Maybe LuciMessage -> m ()
logMsg Nothing = liftIO $ putStrLn "\nFinished session."
logMsg (Just (val, bss)) = liftIO $ do
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
