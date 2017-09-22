{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Helen.Core.Service.Startup where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State

import System.Exit
import System.IO.Error
import System.Process

import Path

import Helen.Core.OptParse.Types
import Helen.Core.Types

startupServices :: HelenWorld ()
startupServices = do
    bcs <- gets $ setBins . helenSettings
    mapM_ startBinaryWithRestarts bcs

data BinState = BinState
    { binStateProcessHandle :: ProcessHandle
    , binStateRestartsLeft :: Int
    }

startBinaryWithRestarts :: BinConfig -> HelenWorld ()
startBinaryWithRestarts bc@BinConfig {..} = do
    ph <- startBinary bc
    nras <- gets $ setBinRestartAttempts . helenSettings
    let bs = BinState {binStateProcessHandle = ph, binStateRestartsLeft = nras}
    forkHelen $ go bs
  where
    go BinState {..} = do
        ec <- liftIO $ waitForProcess binStateProcessHandle
        case ec of
            ExitSuccess -> pure ()
            ExitFailure c -> do
                logOtherNS binConfigName LevelError $
                    T.unwords
                        [ binConfigName
                        , "failed with exit code"
                        , T.pack (show c) <> ","
                        , T.pack (show binStateRestartsLeft)
                        , "restarts left."
                        ]
                when (binStateRestartsLeft > 0) $ do
                    logOtherNS binConfigName LevelInfo $
                        T.unwords ["restarting", binConfigName]
                    ph <- startBinary bc
                    go
                        BinState
                        { binStateProcessHandle = ph
                        , binStateRestartsLeft = binStateRestartsLeft - 1
                        }

startBinary :: BinConfig -> HelenWorld ProcessHandle
startBinary BinConfig {..} = do
    let cmd = unwords $ toFilePath binConfigPath : binConfigArgs
    let cp = (shell cmd) {std_out = CreatePipe, std_err = CreatePipe}
    (_, mouth, merrh, ph) <- liftIO $ createProcess_ cmd cp
    setupOutputHandler LevelInfo mouth
    setupOutputHandler LevelError merrh
    pure ph
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
