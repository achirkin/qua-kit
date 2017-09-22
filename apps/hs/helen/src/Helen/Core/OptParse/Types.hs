{-# LANGUAGE OverloadedStrings #-}

module Helen.Core.OptParse.Types where

import Control.Monad
import Data.Aeson as JSON
import Data.IP
import Data.Set (Set)
import Luci.Connect
import Text.Read

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagHost :: Maybe String
    , flagPort :: Maybe Int
    , flagLogLevel :: Maybe String
    , flagTrustedClients :: [IPv4]
    } deriving (Show, Eq)

data Environment = Environment
    { envConfigFile :: Maybe FilePath
    , envHost :: Maybe String
    , envPort :: Maybe Int
    , envLogLevel :: Maybe String
    , envTrustedClients :: [IPv4]
    } deriving (Show, Eq)

data Configuration = Configuration
    { confHost :: Maybe String
    , confPort :: Maybe Int
    , confLogLevel :: Maybe String
    , confTrustedClients :: Maybe [IPv4]
    } deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON =
        withObject "Config" $ \o -> do
            h <- o .:? "host"
            p <- o .:? "port"
            ll <- o .:? "loglevel"
            tcls <-
                do mtclos <- o .:? "trusted-clients"
                   case mtclos of
                       Nothing -> pure Nothing
                       Just tclos ->
                           fmap Just $
                           forM tclos $ \tclo -> do
                               str <- parseJSON tclo
                               case readMaybe str of
                                   Nothing ->
                                       fail $
                                       "Could not parse IP address: " ++ str
                                   Just ip -> pure ip
            pure $
                Configuration
                { confHost = h
                , confPort = p
                , confLogLevel = ll
                , confTrustedClients = tcls
                }

data Settings = Settings
    { setHost :: String
    , setPort :: Int
    , setLogLevel :: LogLevel
    , setTrustedClients :: Set IPv4
    } deriving (Show, Eq)
