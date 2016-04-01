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
{-# LANGUAGE QuasiQuotes #-}

module Handler.ImageUpload where

--import Control.Monad.Trans.Resource (runResourceT)
--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
import Data.Text (Text)
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
--import Yesod.Auth

import Foundation
--import Model


data Story = Story
    { storyAuthor  :: Maybe Text
    , storyImage   :: FileInfo
    , storyCountry :: Text
    , storyCity    :: Text
    , storyComment :: Textarea
    }

instance Show Story where
    show s = "Your story:\n"
        ++ "Author:  " ++ show (storyAuthor s) ++ "\n"
        ++ "Country: " ++ show (storyCountry s) ++ "\n"
        ++ "City:    " ++ show (storyCity s) ++ "\n"
        ++ "Comment: " ++ show (storyComment s)

getImageUploadR :: Handler Html
getImageUploadR = postImageUploadR

postImageUploadR :: Handler Html
postImageUploadR = do
    ((res, widget), formEncType) <- runFormPost uploadForm
    defaultLayout $ do
        setTitle "Share your story"
        [whamlet|
            <p>Result: #{show res}
            <form method=post action=@{ImageUploadR}  enctype=#{formEncType}>
                ^{widget}
        |]


uploadForm :: Html -> MForm Handler (FormResult Story, Widget)
uploadForm extra = do
    (authorRes, authorView ) <- mopt textField     opts Nothing
    (imageRes,  imageView  ) <- mreq fileField     reqs
       { fsAttrs = ("accept","image/*") : fsAttrs reqs} Nothing
    (countryRes,countryView) <- mreq textField     reqs Nothing
    (cityRes,   cityView   ) <- mreq textField     reqs Nothing
    (commentRes,commentView) <- mreq textareaField reqs Nothing
    (agreeRes,  agreeView  ) <- mreq checkBoxField reqs Nothing
    storyDataDiv  <- newIdent
    imgPreviewDiv <- newIdent
    topDiv    <- newIdent
    bottomDiv <- newIdent
    let mustAgree s = case agreeRes of
                FormSuccess False ->
                    FormFailure ["You must agree the terms of use (check the checkbox)."] *> s
                x -> x *> s
        storyRes = mustAgree $ Story
            <$> authorRes
            <*> imageRes
            <*> countryRes
            <*> cityRes
            <*> commentRes
    return (storyRes, $(widgetFileNoReload def "imageUpload"))
  where
    reqs = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [("required","true")]
        }
    opts = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = []
        }

