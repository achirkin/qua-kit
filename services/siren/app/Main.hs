{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib

main :: IO ()
main = withPostgres sets $ \conn -> do
    listScenarios conn >>= print
    getScenario conn 2 >>= print
  where
    sets = PSSettings
      { uName  = "siren"
      , uPass  = "sirenpass"
      , dbHost = "localhost"
      , dbPort = 5432
      , dbName = "sirendb"
      }
