{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Helen.Core.Utils where

import Data.Aeson
import qualified Data.ByteString as SB
import Data.Yaml as Yaml

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe


import Path
import Path.IO

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
