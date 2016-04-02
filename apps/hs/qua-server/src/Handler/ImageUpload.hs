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

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary

import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
import Yesod
import Yesod.Default.Util
import Yesod.Core.Types

import qualified Text.Blaze as Blaze
--import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lazy as Base64L

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
getImageUploadR = do
    ((res, widget), formEncType) <- runFormPost uploadForm
    defaultLayout $ do
      setTitle "Share your story"
      case res of
        FormFailure msgs -> showFormError msgs
        _ -> return ()
      showFormWidget widget formEncType
  where
    showFormError :: [Text] -> Widget
    showFormError msgs = do
      errorWrapper <- newIdent
      toWidget [cassius|
        ##{errorWrapper} div
            background-color: #fcc;
            padding: 3px 5%;
            margin: 3px;
            font-variant: all-small-caps;
      |]
      [whamlet|
        <div ##{errorWrapper}>
          $forall msg <- msgs
            <div>#{msg}
      |]
    showFormWidget :: Widget -> Enctype -> Widget
    showFormWidget widget formEncType = [whamlet|
        <form method=post action=@{ImageUploadR} enctype=#{formEncType}>
          ^{widget}
      |]

postImageUploadR :: Handler Html
postImageUploadR = do
    ((res, widget), formEncType) <- runFormPost uploadForm
    defaultLayout $ do
      setTitle "Share your story"
      case res of
        FormFailure msgs -> showFormError msgs
        _ -> return ()
      showFormWidget widget formEncType
  where
    showFormError :: [Text] -> Widget
    showFormError msgs = do
      errorWrapper <- newIdent
      toWidget [cassius|
        ##{errorWrapper} div
            background-color: #fcc;
            padding: 3px 5%;
            margin: 3px;
            font-variant: all-small-caps;
      |]
      [whamlet|
        <div ##{errorWrapper}>
          $forall msg <- msgs
            <div>#{msg}
      |]
    showFormWidget :: Widget -> Enctype -> Widget
    showFormWidget widget formEncType = [whamlet|
        <form method=post action=@{ImageUploadR} enctype=#{formEncType}>
          ^{widget}
      |]


uploadForm :: Html -> MForm Handler (FormResult Story, Widget)
uploadForm extra = do
    storyDataDiv  <- newIdent
    imgPreviewDiv <- newIdent
    topDiv    <- newIdent
    bottomDiv <- newIdent

    (authorRes, authorView ) <- mopt textField     opts Nothing
    (mimageRes, imageView  ) <- mopt fileField     reqs
       { fsAttrs = ("accept","image/*") : fsAttrs reqs} Nothing
    (countryRes,countryView) <- mreq textField     reqs Nothing
    (cityRes,   cityView   ) <- mreq textField     reqs Nothing
    (commentRes,commentView) <- mreq textareaField reqs Nothing
    (agreeRes,  agreeView  ) <- mreq checkBoxField reqs Nothing

    (imgFName, imgFType, imgBase64) <- case mimageRes of
        FormSuccess (Just fi) -> do
            liftIO $ print $ fileName fi
            fb <- runResourceT $ fileSource fi $$ sinkLbs
            return ( Just $ fileName fi
                   , Just $ fileContentType fi
                   , Just . LText.decodeUtf8 $ Base64L.encode fb
                   )
        _ -> return (Nothing, Nothing, Nothing)


    (inameRes, inameView) <- addHiddenValueHolder imgFName
    (itypeRes, itypeView) <- addHiddenValueHolder imgFType
    (idataRes, idataView) <- addHiddenValueHolder imgBase64

    let mLastImage = formToMaybe $ mTriple
            <$> inameRes
            <*> itypeRes
            <*> idataRes
        mTriple (Just a) (Just b) (Just c) = Just (FileInfo
                { fileName = a
                , fileContentType = b
                , fileSourceRaw = sourceLbs . Base64L.decodeLenient
                                            $ LText.encodeUtf8 c
                , fileMove = \_ -> return ()
                }, c)
        mTriple _ _ _ = Nothing

        imageRes = case (mimageRes, fst <$> mLastImage) of
            (FormSuccess (Just x), _)       -> FormSuccess x
            (FormSuccess Nothing , Just x ) -> FormSuccess x
            (FormSuccess Nothing , Nothing) -> FormFailure ["Value is required"]
            (FormMissing         , Just x ) -> FormSuccess x
            (FormMissing         , Nothing) -> FormFailure ["Value is required"]
            (FormFailure _       , Just x ) -> FormSuccess x
            (FormFailure errs    , Nothing) -> FormFailure errs

        setErrMsg s v = case v of
                FormFailure (m:ms) ->
                    FormFailure ((s `Text.append` ": " `Text.append` m):ms)
                x -> x

        mustAgree s = case agreeRes of
                FormSuccess False ->
                    FormFailure ["You must agree the terms of use (check the checkbox)"] *> s
                x -> x *> s

        storyRes = mustAgree $ Story
            <$> setErrMsg "Error in the field \"author\"" authorRes
            <*> setErrMsg "Error in the field \"image file\"" imageRes
            <*> setErrMsg "Error in the field \"country\"" countryRes
            <*> setErrMsg "Error in the field \"city\"" cityRes
            <*> setErrMsg "Error in the field \"commentary\"" commentRes
    imageBytes <- case imageRes of
      FormSuccess fi -> do
        fb <- runResourceT $ fileSource fi $$ sinkLbs
        return . Just $ "data:"
            `LText.append` (LText.fromStrict $ fileContentType fi)
            `LText.append` ";base64,"
            `LText.append` (LText.decodeLatin1 $ Base64L.encode fb)
      _ -> return Nothing
    return (storyRes, $(widgetFileNoReload def "imageUpload"))
  where
    formToMaybe (FormSuccess (Just x)) = Just x
    formToMaybe _ = Nothing
    reqs = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = [] -- [("required","required")]
        }
    opts = FieldSettings
        { fsLabel   = ""
        , fsTooltip = Nothing
        , fsId      = Nothing
        , fsName    = Nothing
        , fsAttrs   = []
        }
addHiddenValueHolder :: (PathPiece p, Blaze.ToMarkup p)
                     => Maybe p
                     -> MForm Handler (FormResult (Maybe p), Widget)
addHiddenValueHolder Nothing = return (FormMissing, [whamlet||])
addHiddenValueHolder mx = do
  xId <- newIdent
  xName <- newIdent
  (xRes, _) <- mopt hiddenField FieldSettings
    { fsLabel   = ""
    , fsTooltip = Nothing
    , fsId      = Just xId
    , fsName    = Just xName
    , fsAttrs   = []
    }  Nothing
  return $ case mx of
    Nothing -> ( xRes, [whamlet||])
    Just x  -> ( xRes
               , [whamlet|<input type="hidden" ##{xId} name="#{xName}" value="#{x}">|])


