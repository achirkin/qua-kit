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
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
import Yesod
import Yesod.Default.Util
import Yesod.Core.Types

import qualified Text.Blaze as Blaze
--import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lazy as Base64L

import Foundation
--import Model


import Web.LTI
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as Map

t_oauth_consumer_key :: BS.ByteString
t_oauth_consumer_key = "test_lti_key"

t_oauth_secret :: BS.ByteString
t_oauth_secret = "test_lti_secret"

t_lti :: LTIProvider
t_lti = newLTIProvider t_oauth_consumer_key t_oauth_secret

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
postImageUploadR = setupSession $ \userId -> do
    ((res, widget), formEncType) <- runFormPost uploadForm
    defaultLayout $ do
      setTitle "Share your story"
      case res of
        FormFailure msgs -> showFormError msgs
        _ -> return ()
      [whamlet|<p>#{userId}|]
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

setupSession :: (Text -> Handler Html) -> Handler Html
setupSession h = do
    muid <- lookupSession "lis_result_sourcedid"
    case muid of
      Nothing -> do
        wreq <- waiRequest
        eltiRequest <- runExceptT $ processWaiRequest t_lti wreq
        case eltiRequest of
          Left err -> defaultLayout $ do
            setTitle "Error"
            [whamlet|<p>#{show err}|]
          Right pams -> do
            let murl = Text.decodeUtf8 <$> Map.lookup "lis_outcome_service_url" pams
                msid = Text.decodeUtf8 <$> Map.lookup "lis_result_sourcedid" pams
            Prelude.mapM_ (setSession "lis_outcome_service_url") murl
            Prelude.mapM_ (setSession "lis_result_sourcedid") msid
            case msid of
                Nothing -> defaultLayout $ do
                    setTitle "Error"
                    [whamlet|<p>Failed to lookup lis_result_sourcedid|]
                Just uid -> h uid
      Just uid -> h uid

uploadForm :: Html -> MForm Handler (FormResult Story, Widget)
uploadForm extra = do
    -- set up ids of main div elements
    storyDataDiv  <- newIdent
    imgPreviewDiv <- newIdent
    topDiv    <- newIdent
    bottomDiv <- newIdent

    -- set up all input
    (authorRes, authorView ) <- mopt textField     opts Nothing
    (mimageRes, imageView  ) <- mopt fileField     reqs
       { fsAttrs = ("accept","image/*") : fsAttrs reqs} Nothing
    (countryRes,countryView) <- mreq textField     reqs Nothing
    (cityRes,   cityView   ) <- mreq textField     reqs Nothing
    (commentRes,commentView) <- mreq textareaField reqs Nothing
    (agreeRes,  agreeView  ) <- mreq checkBoxField reqs Nothing

    -- set up hidden fields for keeping image
    (imgFName, imgFType, imgBase64) <- case mimageRes of
        FormSuccess (Just fi) -> do
            fb <- runResourceT $ fileSource fi $$ sinkLbs
            return ( Just $ fileName fi
                   , Just $ fileContentType fi
                   , Just . LText.decodeUtf8 $ Base64L.encode fb
                   )
        _ -> return (Nothing, Nothing, Nothing)
    (inameRes, inameView) <- addHiddenValueHolder imgFName
    (itypeRes, itypeView) <- addHiddenValueHolder imgFType
    (idataRes, idataView) <- addHiddenValueHolder imgBase64

    let -- take either current image or previous (if any)
        imageRes = case ( mimageRes
                        , makeFileInfo inameRes itypeRes idataRes) of
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

        -- insert the image encoded data into the page later
        imageBytes = case (itypeRes, idataRes) of
          (Just typ, Just dat) -> Just $ "data:"
                `Text.append` typ
                `Text.append` ";base64,"
                `Text.append` (LText.toStrict dat)
          _ -> Nothing
    return (storyRes, $(widgetFileNoReload def "imageUpload"))
  where
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
    makeFileInfo (Just iname)
                 (Just itype)
                 (Just idata) = Just $ FileInfo
        { fileName = iname
        , fileContentType = itype
        , fileSourceRaw = sourceLbs . Base64L.decodeLenient
                                    $ LText.encodeUtf8 idata
        , fileMove = \_ -> return ()
        }
    makeFileInfo _ _ _ = Nothing


-- | Returns default value if it is given,
--   otherwise returns input value
addHiddenValueHolder :: (PathPiece p, Blaze.ToMarkup p)
                     => Maybe p
                     -> MForm Handler (Maybe p, Widget)
addHiddenValueHolder mx = do
  xName <- newIdent
  (xRes', xView) <- mopt hiddenField FieldSettings
    { fsLabel   = ""
    , fsTooltip = Nothing
    , fsId      = Nothing
    , fsName    = Just xName
    , fsAttrs   = []
    }  Nothing
  let xRes = case mx of
        Nothing -> case xRes' of
           FormSuccess (Just x) -> Just x
           _                    -> Nothing
        Just x  -> Just x
  return . (,) xRes $ case xRes of
    Just x ->
        [whamlet|<input type="hidden" ##{fvId xView} name="#{xName}" value="#{x}">|]
    _ -> return ()

