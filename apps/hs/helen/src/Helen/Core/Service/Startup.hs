{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Helen.Core.Service.Startup where

import Data.Aeson
import qualified Data.ByteString as SB
import Data.Yaml as Yaml

import Control.Monad.IO.Class

import System.Process

import Path
import Path.IO

startupServices :: MonadIO m => m ()
startupServices = do
    liftIO $ putStrLn "Starting services."
    bcf <- liftIO $ resolveFile' "binaries-config.yaml"
    errOrBc <- readBinConfigs bcf
    case errOrBc of
        Left err -> liftIO $ putStrLn err
        Right bc -> do
            cd <- liftIO getCurrentDir
            errOrBcss <- mapM readBinConfig $ map (cd </>) $ binConfigsFiles bc
            case sequence errOrBcss of
                Left err -> liftIO $ putStrLn err
                Right bcs -> mapM_ startBinary bcs

readBinConfigs :: MonadIO m => Path Abs File -> m (Either String BinConfigs)
readBinConfigs = readYamlSafe

newtype BinConfigs = BinConfigs
    { binConfigsFiles :: [Path Rel File]
    } deriving (Show, Eq)

instance FromJSON BinConfigs where
    parseJSON = withObject "BinConfigs" $ \o -> BinConfigs <$> o .: "configs"

readBinConfig :: MonadIO m => Path Abs File -> m (Either String BinConfig)
readBinConfig = readYamlSafe

data BinConfig = BinConfig
    { binConfigName :: Path Rel File
    , binConfigArgs :: [String]
    } deriving (Show, Eq)

instance FromJSON BinConfig where
    parseJSON =
        withObject "BinConfig" $ \o ->
            BinConfig <$> o .: "executable" <*> o .: "args"

readYamlSafe :: (MonadIO m, FromJSON a) => Path Abs File -> m (Either String a)
readYamlSafe path = do
    mbc <- liftIO $ forgivingAbsence $ SB.readFile (toFilePath path)
    case mbc of
        Nothing ->
            pure $
            Left $ unwords ["WARNING: No yaml file found:", toFilePath path]
        Just contents ->
            case Yaml.decodeEither contents of
                Left err ->
                    pure $
                    Left $
                    unwords
                        [ "WARNING: unable to parse YAML in file"
                        , toFilePath path
                        , "with error:"
                        , err
                        ]
                Right r -> pure $ Right r

startBinary :: MonadIO m => BinConfig -> m ()
startBinary BinConfig {..} = do
    let cmd = unwords $ toFilePath binConfigName : binConfigArgs
    let cp = shell cmd
    (_, _, _, _) <- liftIO $ createProcess_ cmd cp
    pure ()
