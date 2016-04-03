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

getDownloadR :: Key UserStory -> Handler TypedContent
getDownloadR ident = do
    ustory <- getById ident
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", userStoryImageName ustory, "\""]
    sendResponse (Text.encodeUtf8 $ userStoryImageType ustory, toContent $ userStoryImageData ustory)
