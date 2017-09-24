{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helen.Core.OptParse
    ( getSettings
    , Settings(..)
    ) where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON)
import Data.List (intercalate, union)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import qualified Data.ByteString as SB
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Path
import Path.IO (resolveFile', forgivingAbsence)
import System.Environment (getArgs, getEnvironment)
import Text.Read

import Options.Applicative

import Helen.Core.OptParse.Types

getSettings :: IO Settings
getSettings = do
    flags <- getFlags
    env <- getEnv
    config <- getConfiguration flags env
    combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mconf = do
    let c func = mconf >>= func
    mlf <-
        case flagLogFile `mplus` envLogFile `mplus` c confLogFile of
            Nothing -> pure Nothing
            Just lf -> Just <$> resolveFile' lf
    pure
        Settings
        { settingsHost =
              fromMaybe "localhost" $
              flagHost `mplus` envHost `mplus` c confHost
        , settingsPort = fromMaybe 7654 $ flagPort `mplus` envPort `mplus` c confPort
        , settingsLogFile = mlf
        , settingsLogLevel =
              fromMaybe LevelInfo $
              (flagLogLevel `mplus` envLogLevel `mplus` c confLogLevel) >>=
              (`lookup` logLevelOptions)
        , settingsTrustedClients =
              S.fromList
                  (flagTrustedClients `union` envTrustedClients `union`
                   fromMaybe [] (c confTrustedClients))
        , settingsBins = fromMaybe [] $ c confBins
        , settingsBinRestartAttempts =
              fromMaybe 3 $
              flagRestartAttempts `mplus` envRestartAttempts `mplus`
              c confRestartAttempts
        }

logLevelOptions :: [(String, LogLevel)]
logLevelOptions =
    [ ("debug", LevelDebug)
    , ("info", LevelInfo)
    , ("warn", LevelWarn)
    , ("warning", LevelWarn)
    , ("error", LevelError)
    ]

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
    cf <-
        case flagConfigFile `mplus` envConfigFile of
            Nothing -> defaultConfigFile
            Just fcf -> resolveFile' fcf
    errOrConfig <- readYamlSafe cf
    case errOrConfig of
        Left err -> do
            let runWithLog =
                    (case flagLogFile `mplus` envLogFile of
                         Nothing -> runStdoutLoggingT
                         Just lf -> runFileLoggingT lf) .
                    filterLogger
                         (\_ l ->
                              l >=
                              fromMaybe
                                  LevelInfo
                                  (flagLogLevel `mplus` envLogLevel >>=
                                    (`lookup` logLevelOptions)))
            runWithLog $
                logWarnNS "ConfigParser" $
                T.unwords
                    [ "WARNING: Could not read YAML file:"
                    , T.pack (toFilePath cf)
                    , "because of error:"
                    , T.pack err
                    , "; using defaults."
                    ]
            pure Nothing
        Right c -> pure $ Just c

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "helen-config.yaml"

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    let v k = lookup ("HELEN_" ++ k) env
    pure
        Environment
        { envConfigFile = v "CONFIG_FILE"
        , envHost = v "HOST"
        , envPort = v "PORT" >>= readMaybe
        , envLogFile = v "LOGFILE"
        , envLogLevel = v "LOGLEVEL"
        , envTrustedClients =
              case v "TRUSTED_CLIENTS" of
                  Nothing -> []
                  Just clientsStr -> mapMaybe readMaybe $ words clientsStr
        , envRestartAttempts = v "RESTARTS" >>= readMaybe
        }

getFlags :: IO Flags
getFlags = do
    args <- getArgs
    let result = runFlagParser args
    handleParseResult result

runFlagParser :: [String] -> ParserResult Flags
runFlagParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Flags
argParser = info (helper <*> parseFlags) (fullDesc <> progDesc "Helen")

parseFlags :: Parser Flags
parseFlags =
    Flags <$>
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILE"
             , long "config-file"
             , short 'c'
             , help "Config file"
             , value Nothing
             ]) <*>
    option
        (Just <$> auto)
        (mconcat
             [ metavar "HOST"
             , long "host"
             , short 'h'
             , help "Host"
             , value Nothing
             ]) <*>
    option
        (Just <$> auto)
        (mconcat
             [ metavar "PORT"
             , long "port"
             , short 'p'
             , help "Port"
             , value Nothing
             ]) <*>
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILE"
             , long "logfile"
             , short 'o'
             , help "Log file"
             , value Nothing
             ]) <*>
    option
        (Just <$> str)
        (mconcat
             [ metavar "LOGLEVEL"
             , long "loglevel"
             , short 'l'
             , help $
               "Log level; (options: " ++
               intercalate ", " (map fst logLevelOptions) ++ ")"
             , value Nothing
             ]) <*>
    many
        (option
             auto
             (mconcat
                  [ metavar "IP"
                  , long "trusted"
                  , short 't'
                  , help "Trusted client"
                  ])) <*>
    option
        (Just <$> auto)
        (mconcat
             [ metavar "INT"
             , long "restarts"
             , short 'r'
             , help "Number of restart attempts for services"
             , value Nothing
             ])

readYamlSafe :: (MonadIO m, FromJSON a) => Path Abs File -> m (Either String a)
readYamlSafe path =
    runExceptT $ do
        contents <-
            maybeToExceptT (unwords ["No yaml file found:", toFilePath path]) .
            MaybeT . liftIO . forgivingAbsence $
            SB.readFile (toFilePath path)
        withExceptT
            (\err ->
                 unwords
                     [ "Unable to parse YAML in file"
                     , toFilePath path
                     , "with error:"
                     , err
                     ]) .
            ExceptT . pure $
            Yaml.decodeEither contents
