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
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
--import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import qualified Blaze.ByteString.Builder as Builder
import Text.Blaze
import Text.Hamlet.XML
import Text.XML
import Yesod
import Yesod.Default.Util
import qualified Data.IORef as I

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Client.Conduit
--import Network.HTTP.Types.URI as URI
--import Text.Blaze (unsafeByteString)
import Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C

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



getLtiR :: Handler Html
getLtiR = do
    srequest <- signOAuthWithBodyHash ltiOAuth emptyCredential request
    response <- withManager $ httpLbs srequest
    defaultLayout $ do
        setTitle "LTI test"
        [whamlet|
            Request URI:
            <pre>#{show $ getUri srequest}
            Request headers:
            <pre>#{showHeaders $ requestHeaders srequest}
            Request parameters:
            <pre>#{show $ queryString srequest}
            Request body:
            <pre>#{Text.decodeUtf8 $ t_msg_content}
            <br>
            Response headers:
            <pre>#{showHeaders $ responseHeaders response}
            Response body:
            <pre>#{LT.decodeUtf8 $ responseBody response}
        |]
    where ltiOAuth = newOAuth
            { oauthServerName      = "test LTI oauth"
            , oauthSignatureMethod = HMACSHA1
            , oauthConsumerKey     = t_oauth_consumer_key
            , oauthConsumerSecret  = t_oauth_secret
            , oauthVersion         = OAuth10
            }
          request = def
            { method = "POST"
            , secure = True
            , host = "courses.edx.org"
            , port = 443
            , path = "/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@2fc0ef710f2c4e9ca6d2c31fc5731ff5/handler_noauth/outcome_service_handler"

            , requestBody = RequestBodyBS t_msg_content
            , requestHeaders = [ ("Content-Type", "application/xml")
                               ]
            }
          t_msg_content = replaceResultTemplate
                            "161346234634"
                            "course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-2fc0ef710f2c4e9ca6d2c31fc5731ff5:a9f4ca11f3b686a7bf12812326dfcb1c"
                            0.86
                            "This is a text result data. Nothing interesting, just data"
          showHeaders ((n,v):hs) = show n ++ ": " ++ show v ++ "\n" ++ showHeaders hs
          showHeaders [] = ""


signOAuthWithBodyHash :: MonadIO m
                      => OAuth      -- ^ OAuth Application
                      -> Credential -- ^ Credential
                      -> Request    -- ^ Original Request
                      -> m Request  -- ^ Signed OAuth Request
signOAuthWithBodyHash auth creds req = do
    content <- toBS $ requestBody req
    let oauth_body_hash = paramEncode . Base64.encode $ SHA1.hash content
        qstring = queryString req
        mqstring = (if SB.null qstring
                    then ""
                    else qstring `SB.append` "&")
                   `SB.append` "oauth_body_hash=" `SB.append` oauth_body_hash
    sreq <- signOAuth auth creds req{queryString = mqstring}
    let rhs = appendHeader <$> requestHeaders sreq
        appendHeader (hn, hv) | hn == "Authorization" = (hn, hv
                                                            `SB.append` ",oauth_body_hash=\""
                                                            `SB.append` oauth_body_hash
                                                            `SB.append` "\"")
                              | otherwise = (hn,hv)
    return sreq {queryString = qstring, requestHeaders = rhs}

-- COPIED
toBS :: MonadIO m => RequestBody -> m SB.ByteString
toBS (RequestBodyLBS l) = return $ LB.toStrict l
toBS (RequestBodyBS s) = return s
toBS (RequestBodyBuilder _ b) = return $ Builder.toByteString b
toBS (RequestBodyStream _ givesPopper) = toBS' givesPopper
toBS (RequestBodyStreamChunked givesPopper) = toBS' givesPopper

-- COPIED
toBS' :: MonadIO m => GivesPopper () -> m SB.ByteString
toBS' gp = liftIO $ do
    ref <- I.newIORef SB.empty
    gp (go ref)
    I.readIORef ref
  where
    go ref popper =
        loop id
      where
        loop front = do
            bs <- popper
            if SB.null bs
                then I.writeIORef ref $ SB.concat $ front []
                else loop (front . (bs:))

encodeTemplate :: [Node] -> SB.ByteString
encodeTemplate (NodeElement el :_) = LB.toStrict . renderLBS def{rsPretty = False} $ Document (Prologue [] Nothing []) el []
encodeTemplate (_:xs)              = encodeTemplate xs
encodeTemplate []                  = SB.empty


replaceResultTemplate :: Text -> Text -> Double -> Text -> SB.ByteString
replaceResultTemplate imsxMessageId sourceId resultScore resultData = encodeTemplate
  [xml|<imsx_POXEnvelopeRequest xmlns="http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">
         <imsx_POXHeader>
           <imsx_POXRequestHeaderInfo>
             <imsx_version>V1.0
             <imsx_messageIdentifier>#{imsxMessageId}
           <imsx_POXBody>
             <replaceResultRequest>
               <resultRecord>
                 <sourcedGUID>
                   <sourcedId>#{sourceId}
                 <result>
                   <resultScore>
                     <textString>#{Text.pack $ show resultScore}
                   <resultData>
                     <text>#{resultData}
  |]

-- answer:
--    <imsx_POXHeader>
--        <imsx_POXResponseHeaderInfo>
--            <imsx_version>V1.0</imsx_version>
--            <imsx_messageIdentifier>335349992332</imsx_messageIdentifier>
--            <imsx_statusInfo>
--                <imsx_codeMajor>success</imsx_codeMajor>
--                <imsx_severity>status</imsx_severity>
--                <imsx_description>Score for course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-2fc0ef710f2c4e9ca6d2c31fc5731ff5:a9f4ca11f3b686a7bf12812326dfcb1c is now 0.7</imsx_description>
--                <imsx_messageRefIdentifier>
--                </imsx_messageRefIdentifier>
--            </imsx_statusInfo>
--        </imsx_POXResponseHeaderInfo>
--    </imsx_POXHeader>
--    <imsx_POXBody><replaceResultResponse/></imsx_POXBody>

readResultTemplate :: Text -> Text -> SB.ByteString
readResultTemplate imsxMessageId sourceId = encodeTemplate
  [xml|<imsx_POXEnvelopeRequest xmlns="http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">
         <imsx_POXHeader>
           <imsx_POXRequestHeaderInfo>
             <imsx_version>V1.0
             <imsx_messageIdentifier>#{imsxMessageId}
           <imsx_POXBody>
             <readResultRequest>
               <resultRecord>
                 <sourcedGUID>
                   <sourcedId>#{sourceId}
  |]




t_oauth_consumer_key :: SB.ByteString
t_oauth_consumer_key = "test_lti_key"

t_url :: String
t_url = "https://courses.edx.org/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@2fc0ef710f2c4e9ca6d2c31fc5731ff5/handler_noauth/outcome_service_handler"


t_oauth_secret :: SB.ByteString
t_oauth_secret = "test_lti_secret"

