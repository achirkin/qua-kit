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

import           Data.Conduit
import qualified Data.Conduit.Network as Network
import qualified Data.Aeson as JSON
import qualified Data.Conduit.List as Conduit
import qualified Data.ByteString.Lazy.Char8 as BSL

import Luci.Messages
import Luci.Connect.Base
import Luci.Connect.Internal

main :: IO ()
main = do
    Network.runTCPClient connSettings luciSession
  where
    connSettings = Network.clientSettings 7654 "127.0.1.1" -- "129.132.6.33"

luciSession :: Network.AppData -> IO ()
luciSession appdata = do
    putStrLn $ "Remote address is " ++ show (Network.appSockAddr appdata)
    case Network.appLocalAddr appdata of
      Nothing -> putStrLn "Could not get local address"
      Just ad -> putStrLn $ "Local address is " ++ show ad
    runConduit $ (yield testFileEcho >> yield (JSON.toJSON regMsg, [])) $$ writeMessages =$= outgoing
    putStrLn "Sent request!"
    runConduit inPipe
  where
    regMsg = RemoteRegister
        { exampleCall = JSON.object []
        , serviceName = "CoolTestService"
        , inputs  = Nothing
        , outputs = Nothing
        }
    outgoing = Network.appSink appdata
    incoming = Network.appSource appdata
    inPipe = incoming =$= parseMessages =$= Conduit.mapM_ logMsg


logMsg :: Either ComError (JSON.Value, [ByteString]) -> IO ()
logMsg (Left err) = putStrLn "\nError occured!" >> print err
logMsg (Right (val, bss)) = do
  putStrLn "\nSuccesful message"
  putStrLn . BSL.unpack $ JSON.encode val
  mapM_ print bss


testFileEcho :: (JSON.Value, [ByteString])
testFileEcho = (msg,[bs0, bs1])
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
