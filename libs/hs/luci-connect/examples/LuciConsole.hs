-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- An executable for sending and receiving simple Luci messages.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson as JSON
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Control.Concurrent.QSem

import Luci.Connect
import Luci.Connect.Base
import Luci.Messages
import System.Environment (getArgs)
import Data.List (stripPrefix)
import Data.Maybe (isNothing)
import Control.Monad (when)
import Data.Conduit

main :: IO ()
main = do
    args <- getArgs
    let sets = setSettings args RunSettings
                  { host     = "127.0.1.1"
                  , port     = 7654
                  , logLevel = LevelInfo
                  , command  = Nothing
                  }
    when (isNothing $ command sets) $
      putStrLn "Luci-console app. Usage:\n\
               \luci-console [args..]\n\
               \Parameters:\n\
               \\tport=x             - choose port          (default: 7654)\n\
               \\thost=a.b.c.d       - choose host          (default: 127.0.1.1)\n\
               \\tloglevel=lvl       - choose logging level (default: info)\n\
               \\t   possible levels:  debug, info, warn, error\n\
               \\t-c '<command>'     - send a single JSON message to Luci and disconnect\n\
               \\nType JSON message headers and press enter to send them."
    runLuciProgram () (logLevel sets) $ luciChannels (port sets)
                                                     (Just $ host sets)
       (case command sets of
         Nothing -> return (awaitForever logResponse, source)
         Just msg -> singleAction msg
       )
    return ()
  where
    logResponse (MessageHeader h, _) = liftIO $ do
        putStr "[Server Message] "
        BSLC.putStrLn $ encode h
    source = do
      val <- fmap eitherDecodeStrict' . liftIO $ BS.getLine
      case val of
        Left err -> liftIO $ putStrLn err
        Right h -> yield (h, [])
      source
    setSettings [] s = s
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
               Nothing -> if "-c" == par
                          then s{command = Just . BSC.pack $ unwords xs}
                          else setSettings xs s


singleAction :: ByteString -> LuciProgram () (Sink LuciMessage (LuciProgram ()) (), Source (LuciProgram ()) LuciMessage)
singleAction msg = do
    sem <- liftIO $ newQSem 0
    return (waitResults sem,source sem)
  where
    waitResults sem = do
      m <- await
      case m of
        Nothing -> liftIO $ putStrLn "channel closed"
        Just lm@(h,_) -> do
          liftIO $ do
            putStr "[Server Message] "
            BSLC.putStrLn $ encode h
          case parseMessage lm of
            JSON.Success MsgResult{} -> liftIO (signalQSem sem)
            JSON.Success MsgError{} -> liftIO (signalQSem sem)
            _ -> waitResults sem
    source sem = case eitherDecodeStrict' msg of
        Left err -> liftIO $ putStrLn err
        Right h -> yield (h, []) >> liftIO (waitQSem sem)


data RunSettings = RunSettings
  { host :: ByteString
  , port :: Int
  , logLevel :: LogLevel
  , command :: Maybe ByteString
  }
