-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Preview
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Preview where

import Control.Exception hiding (Handler)
--import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import Text.Blaze
import Yesod
import Yesod.Default.Util

import Foundation
import Model

getPreviewR :: Key StoredFile -> Handler Html
getPreviewR ident = do
    StoredFile filename contentType bytes <- getById ident
    defaultLayout $ do
        setTitle . toMarkup $ "File Processor - " `Text.append` filename
        previewBlock <- liftIO $ preview ident contentType bytes
        $(widgetFileNoReload def "preview")

preview :: Key StoredFile -> Text -> SB.ByteString -> IO Widget
preview ident contentType bytes
  | "image/" `Text.isPrefixOf` contentType =
    return [whamlet|<img src=@{DownloadR ident} style="width: 100%;">|]
  | otherwise = do
    eText <- try . evaluate $ Text.decodeUtf8 bytes :: IO (Either SomeException Text)
    return $ case eText of
      Left _ -> errorMessage
      Right textval -> [whamlet|<pre>#{textval}|]
  where
    errorMessage = [whamlet|<pre>Unable to display file contents.|]
