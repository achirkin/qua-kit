-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Download
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Download where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Yesod

import Foundation
import Model

getDownloadR :: Key StoredFile -> Handler TypedContent
getDownloadR ident = do
    StoredFile filename contentType bytes <- getById ident
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", filename, "\""]
    sendResponse (Text.encodeUtf8 contentType, toContent bytes)
