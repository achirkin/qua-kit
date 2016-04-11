-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.ImgPreview
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

module Handler.ImgPreview
    ( getImgPreviewR
    ) where

import qualified Data.ByteString as SB

import Yesod

import Foundation
import Model

getImgPreviewR :: Key Story -> Handler TypedContent
getImgPreviewR ident = do
    img <- runDB $ do
      ustory <- get404 ident
      get404 $ storyImage ustory
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: SB.ByteString), toContent $ imagePreviewContent img)


