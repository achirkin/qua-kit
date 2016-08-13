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

import Luci.Connect
import Luci.Connect.Base
import System.Environment (getArgs)
import Data.List (stripPrefix)


main :: IO ()
main = do
    putStrLn "Luci-console app. Usage:\n\
             \luci-console [args..]\n\
             \Parameters:\n\
             \\tport=x             - choose port          (default: 7654)\n\
             \\thost=a.b.c.d       - choose host          (default: 127.0.1.1)\n\
             \\tloglevel=lvl       - choose logging level (default: info)\n\
             \\t   possible levels:  debug, info, warn, error\n"
    args <- getArgs
    let sets = setSettings args RunSettings
                  { host     = "127.0.1.1"
                  , port     = 7654
                  , logLevel = LevelInfo
                  }
    putStrLn "\nType JSON message headers and press enter to send them."
    runLuciProgram () (logLevel sets) $ luciChannels (port sets)
                                                     (Just $ host sets)
                                                     (return (awaitForever logResponse, source))
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
               Nothing -> setSettings xs s


data RunSettings = RunSettings
  { host :: ByteString
  , port :: Int
  , logLevel :: LogLevel
  }

