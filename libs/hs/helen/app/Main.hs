-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where



import Luci.Connect
import System.Environment (getArgs)
import Data.List (stripPrefix)

import Helen.Core

main :: IO ()
main = do
    putStrLn "helen standalone module. Usage:\n\
             \helen [args..]\n\
             \Parameters:\n\
             \\tport=x             - choose port          (default: 7654)\n\
             \\tloglevel=lvl       - choose logging level (default: info)\n\
             \\t   possible levels:  debug, info, warn, error\n"
    args <- getArgs
    let sets = setSettings args RunSettings
                  { port     = 7654
                  , logLevel = LevelInfo
                  }
    putStrLn $ "Helen is running on port " ++ show (port sets)
    helen <- initHelen
    _ <- runLuciProgram helen (logLevel sets) $ program (port sets)
    putStrLn "Helen finished."
  where
    setSettings [] s = s
    setSettings (par:xs) s = case stripPrefix "port=" par of
           Just n -> setSettings xs s{port = read n}
           Nothing -> case stripPrefix "loglevel=" par of
               Just "debug"   -> setSettings xs s{logLevel = LevelDebug}
               Just "info"    -> setSettings xs s{logLevel = LevelInfo}
               Just "warn"    -> setSettings xs s{logLevel = LevelWarn}
               Just "warning" -> setSettings xs s{logLevel = LevelWarn}
               Just "error"   -> setSettings xs s{logLevel = LevelError}
               _ -> setSettings xs s

data RunSettings = RunSettings
  { port :: Int
  , logLevel :: LogLevel
  }
