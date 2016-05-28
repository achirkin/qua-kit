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
import          Data.ByteString (ByteString)

import Luci.Messages
import Luci.Connect.Base


main :: IO ()
main = do
    putStrLn "Hello World"
    result <- Network.runTCPClient connSettings luciSession
    putStrLn result
  where
    connSettings = Network.clientSettings 7654 "127.0.1.1" -- "129.132.6.33"

luciSession :: Network.AppData -> IO String
luciSession appdata = do
    putStrLn $ "Remote address is " ++ show (Network.appSockAddr appdata)
    case Network.appLocalAddr appdata of
      Nothing -> putStrLn "Could not get local address"
      Just ad -> putStrLn $ "Local address is " ++ show ad
    runConduit $ yield (regMsg, []) $$ writeMessages =$= outgoing
    putStrLn "Sent request!"
    runConduit inPipe
--    rez <- runConduit inPipe
--    case rez of
--      Nothing -> putStrLn "No input at all!"
--      Just (Left err) -> putStrLn err
--      Just (Right (val, _)) -> putStrLn "Success!" >> print (val :: JSON.Value)
    return "Good!"
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
logMsg = print
