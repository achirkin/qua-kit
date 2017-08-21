{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Helen.Core.Service.Startup where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import System.IO.Error
import System.Process

import Path
import Path.IO

import Helen.Core.Types
import Helen.Core.Utils

startupServices :: HelenWorld ()
startupServices = do
    bcf <- liftIO $ resolveFile' "binaries-config.yaml"
    errOrBc <- readBinConfigs bcf
    case errOrBc of
        Left err -> logErrorNS "Service Startup" (T.pack err)
        Right bc -> do
            cd <- liftIO getCurrentDir
            errOrBcss <- mapM (readBinConfig . (cd </>)) $ binConfigsFiles bc
            forM_ errOrBcss $ \errOrBcs ->
                case errOrBcs of
                    Left err -> logErrorNS "Service Startup" (T.pack err)
                    Right bcs -> void $ startBinary bcs

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
    { binConfigName :: Text
    , binConfigPath :: Path Rel File
    , binConfigArgs :: [String]
    } deriving (Show, Eq)

instance FromJSON BinConfig where
    parseJSON =
        withObject "BinConfig" $ \o ->
            BinConfig <$> o .: "name" <*> o .: "executable" <*>
            ((o .: "args") <|> (words <$> o .: "args"))

data BinState = BinState
    { binStateConfig :: BinConfig
    , binStateProcessHandle :: ProcessHandle
    }

startBinary :: BinConfig -> HelenWorld BinState
startBinary bc@BinConfig {..} = do
    let cmd = unwords $ toFilePath binConfigPath : binConfigArgs
    let cp = (shell cmd) {std_out = CreatePipe, std_err = CreatePipe}
    (_, mouth, merrh, ph) <- liftIO $ createProcess_ cmd cp
    setupOutputHandler LevelInfo mouth
    setupOutputHandler LevelError merrh
    pure BinState {binStateConfig = bc, binStateProcessHandle = ph}
  where
    setupOutputHandler level mh =
        case mh of
            Nothing -> pure () -- Should not happen, but let's not crash if it does.
            Just h ->
                forkHelen $
                let loop = do
                        ml <-
                            liftIO
                                ((Just <$> T.hGetLine h) `catch`
                                 (\e ->
                                      if isEOFError e
                                          then pure Nothing
                                          else throwIO e))
                        case ml of
                            Nothing -> pure ()
                            Just line -> do
                                logOtherNS binConfigName level line
                                loop
                in loop
