{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Helen.Core.OptParse
    ( getSettings
    , Settings(..)
    ) where

import Control.Monad
import Data.List (intercalate, union)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import qualified Data.Set as S
import Luci.Connect
import Path
import Path.IO (resolveFile')
import System.Environment (getArgs, getEnvironment)
import Text.Read

import Options.Applicative

import Helen.Core.OptParse.Types
import Helen.Core.Utils

getSettings :: IO Settings
getSettings = do
    flags <- getFlags
    env <- getEnv
    config <- getConfiguration flags env
    combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mconf = do
    let c func = mconf >>= func
    pure
        Settings
        { setHost =
              fromMaybe "localhost" $
              flagHost `mplus` envHost `mplus` c confHost
        , setPort = fromMaybe 7654 $ flagPort `mplus` envPort `mplus` c confPort
        , setLogLevel =
              fromMaybe LevelInfo $
              (flagLogLevel `mplus` envLogLevel `mplus` c confLogLevel) >>=
              (`lookup` logLevelOptions)
        , setTrustedClients =
              S.fromList
                  (flagTrustedClients `union` envTrustedClients `union`
                   fromMaybe [] (c confTrustedClients))
        , setBins = fromMaybe [] $ c confBins
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
    pure $
        case errOrConfig of
            Left _ -> Nothing
            Right c -> Just c

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
        , envLogLevel = v "LOGLEVEL"
        , envTrustedClients =
              case v "TRUSTED_CLIENTS" of
                  Nothing -> []
                  Just clientsStr -> mapMaybe readMaybe $ words clientsStr
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
                  ]))
