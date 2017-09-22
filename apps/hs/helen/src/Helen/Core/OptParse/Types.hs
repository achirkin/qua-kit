{-# LANGUAGE OverloadedStrings #-}

module Helen.Core.OptParse.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson as JSON
import Data.IP
import Data.Set (Set)
import Data.Text (Text)
import Luci.Connect
import Path
import Text.Read

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagHost :: Maybe String
    , flagPort :: Maybe Int
    , flagLogLevel :: Maybe String
    , flagTrustedClients :: [IPv4]
    , flagRestartAttempts :: Maybe Int
    } deriving (Show, Eq)

data Environment = Environment
    { envConfigFile :: Maybe FilePath
    , envHost :: Maybe String
    , envPort :: Maybe Int
    , envLogLevel :: Maybe String
    , envTrustedClients :: [IPv4]
    , envRestartAttempts :: Maybe Int
    } deriving (Show, Eq)

data Configuration = Configuration
    { confHost :: Maybe String
    , confPort :: Maybe Int
    , confLogLevel :: Maybe String
    , confTrustedClients :: Maybe [IPv4]
    , confBins :: Maybe [BinConfig]
    , confRestartAttempts :: Maybe Int
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
            bcfs <- o .:? "bundled-services"
            cra <- o .:? "restart-attempts"
            pure $
                Configuration
                { confHost = h
                , confPort = p
                , confLogLevel = ll
                , confTrustedClients = tcls
                , confBins = bcfs
                , confRestartAttempts = cra
                }

data BinConfig = BinConfig
    { binConfigName :: Text
    , binConfigPath :: Path Rel File
    , binConfigArgs :: [String]
    } deriving (Show, Eq)

instance FromJSON BinConfig where
    parseJSON =
        withObject "BinConfig" $ \o ->
            BinConfig <$> o .: "name" <*> o .: "executable" <*>
            (((o .:? "args") <|> (fmap words <$> o .:? "args")) .!= [])

data Settings = Settings
    { setHost :: String
    , setPort :: Int
    , setLogLevel :: LogLevel
    , setTrustedClients :: Set IPv4
    , setBins :: [BinConfig]
    , setBinRestartAttempts :: Int
    } deriving (Show, Eq)
