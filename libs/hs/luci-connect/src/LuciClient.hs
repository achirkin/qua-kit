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
{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Main (main) where

import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Aeson as JSON
import qualified Data.Conduit.List as Conduit
import           Data.Text (Text)
import           Data.ByteString (ByteString)

import Luci.Messages
import Luci.Connect
import Luci.Connect.Base


main :: IO ()
main = do

    -- Test using Luci.Connect.Base
    putStrLn "\nRun short-circuited conduit pipe..."
    runConduit $ (yield testFileEcho >> yield remoteRegister)
      =$= writeMessages =$= parseMessages =$= Conduit.mapM_ (logMsgE . Just)

    -- Connect using Luci.Connect
    putStrLn "\n\nRun conduit pipes connected to Luci..."
    talkToLuciExt 7654 "127.0.1.1" $ do
      -- send test message
      yield testFileEcho
      -- easily interleave sending-receiving with 'yield' and 'await'
      answer1 <- await
      answer2 <- await
      -- also print answers via lifting into IO monad
      liftIO $ logMsg answer1
      -- send RemoteRegister request
      yield remoteRegister
      answer3 <- await
      answer4 <- await
      liftIO $ logMsg answer2
      liftIO $ logMsg answer3
      liftIO $ logMsg answer4

--    -- Connect using Luci.Connect
--    putStrLn "\n\nRun conduit pipes connected to Luci..."
--    talkToLuci 7654 "127.0.1.1" $ do
--      -- send test message
--      yield testFileEcho
--      -- easily interleave sending-receiving with 'yield' and 'await'
--      answer1 <- await
--      answer2 <- await
--      -- also print answers via lifting into IO monad
--      liftIO $ logMsgE answer1
--      -- send RemoteRegister request
--      yield remoteRegister
--      answer3 <- await
--      answer4 <- await
--      liftIO $ logMsgE answer2
--      liftIO $ logMsgE answer3
--      liftIO $ logMsgE answer4
--


logMsgE :: Maybe (Either ComError LuciMessage) -> IO ()
logMsgE Nothing = putStrLn "\nFinished session."
logMsgE (Just (Left err)) = putStrLn "\nError occured!" >> print err
logMsgE (Just (Right (val, bss))) = do
  putStrLn "\nReceived:"
  print val
  mapM_ print bss

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
