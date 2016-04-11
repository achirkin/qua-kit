-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Image
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Handler.Image
    ( getImageR
    ) where

import qualified Data.Text.Encoding as Text

import Yesod

import Foundation
import Model

getImageR :: Key Story -> Handler TypedContent
getImageR ident = do
    img <- runDB $ do
      ustory <- get404 ident
      upreview <- get404 $ storyImage ustory
      get404 $ imagePreviewFullVersion upreview
    addHeader "Content-Disposition" "inline"
    sendResponse (Text.encodeUtf8 $ imageContentType img, toContent $ imageData img)


