-----------------------------------------------------------------------------
--
-- Module      :  Handler.ImageUpload
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ImageUpload where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Data.Text (Text)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
import Yesod.Auth

import Foundation
import Model


postImageUploadR :: Handler Html
postImageUploadR = getImageUploadR

getImageUploadR :: Handler Html
getImageUploadR = do
    maid <- maybeAuthId
    (formWidget, formEncType) <- generateFormPost uploadForm
    storedFiles <- getList
    defaultLayout $ do
        setTitle "Share your story"
        $(widgetFileNoReload def "imageUpload")

uploadForm :: Html -> MForm Handler (FormResult
        ( Maybe Text
        , FileInfo
        , Text
        , Text
        , Textarea
        , Bool), Widget)
uploadForm = renderDivs $ (,,,,,)
  <$> aopt textField "You may type your name, if you want" Nothing
  <*> fileAFormReq FieldSettings
        { fsLabel   = "Select an image file for upload"
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("required","true")]
        }
  <*> areq textField "Country" Nothing
  <*> areq textField "City" Nothing
  <*> areq textareaField FieldSettings
        { fsLabel   = "Commentary"
        , fsTooltip = Nothing
        , fsId      = Just "imageCommentArea"
        , fsName    = Nothing
        , fsAttrs   = [("required","true")]
        } Nothing
  <*> areq checkBoxField FieldSettings
        { fsLabel   = "I confirm that I own the uploaded image\
                      \ and allow the Chair for Information Architecture to use the image\
                      \ and expose it to public."
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("required","true")]
        } Nothing

