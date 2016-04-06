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
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import Data.Default
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
--import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import Text.Blaze
import Yesod
import Yesod.Default.Util

import Network.HTTP.Client.Conduit
import qualified Network.Wai as Wai

import Foundation
import Model

import Web.LTI
import Control.Monad.Trans.Except (runExceptT)

getPreviewR :: Key Story -> Handler Html
getPreviewR ident = do
    ustory <- getById ident
    upreview <- getById $ storyImage ustory
    img <- getById $ imagePreviewFullVersion upreview
    defaultLayout $ do
        setTitle . toMarkup $ "File Processor - " `Text.append` imageName img
        previewBlock <- liftIO $ preview ident (imageContentType img)
                                               (imageData img)
        $(widgetFileNoReload def "preview")

preview :: Key Story -> Text -> SB.ByteString -> IO Widget
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

postLtiR :: Handler Html
postLtiR = getLtiR

getLtiR :: Handler Html
getLtiR = do
  wreq <- waiRequest
  ltiConstruct <- runExceptT $ gradeRequest
          (newLTIProvider t_oauth_consumer_key t_oauth_secret)
          processWaiRequest wreq
  case ltiConstruct of
   Left err -> do
    body <- liftIO $ Wai.strictRequestBody wreq
    defaultLayout $ do
      setTitle "LTI test -- error"
      [whamlet|
            There was an error during LTI procedures:
            <pre>#{show err}
            Page request headers:
            <pre>
              $forall (pamn,pamv) <- Wai.requestHeaders wreq
                #{show pamn}: #{SBC.unpack pamv ++ "\n"}
            Page request query string:
            <pre>
              #{SBC.unpack $ Wai.rawQueryString wreq}
            Page request body:
            <pre>
              #{SBC.unpack $ LB.toStrict body}
        |]
   Right (grade, pams) -> do
    app <- getYesod
    response <- grade 0.75 $ httpManager app
    defaultLayout $ do
      setTitle "LTI test"
      [whamlet|
            Page request parameters:
            <pre>
              $forall (pamn,pamv) <- Map.toList pams
                #{SBC.unpack pamn}: #{SBC.unpack pamv ++ "\n"}
            EdX response headers:
            <pre>
              $forall (pamn,pamv) <- responseHeaders response
                #{show pamn}: #{SBC.unpack pamv ++ "\n"}
            EdX response body:
            <pre>#{LT.decodeUtf8 $ responseBody response}
        |]



t_oauth_consumer_key :: SB.ByteString
t_oauth_consumer_key = "test_lti_key"

t_oauth_secret :: SB.ByteString
t_oauth_secret = "test_lti_secret"

