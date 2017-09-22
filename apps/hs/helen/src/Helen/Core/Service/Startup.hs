{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Helen.Core.Service.Startup where

import qualified Data.Text.IO as T

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State

import System.IO.Error
import System.Process

import Path

import Helen.Core.OptParse.Types
import Helen.Core.Types

startupServices :: HelenWorld ()
startupServices = do
    bcs <- gets $ setBins . helenSettings
    mapM_ startBinary bcs

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
