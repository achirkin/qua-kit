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
import qualified Data.Foldable as F
import Data.Default
import Data.Maybe (fromJust)
import Data.Text (Text)
import           Data.List                    as List (find)
import qualified Data.Text as Text
--import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import Text.Blaze
import Text.Hamlet.XML
import Text.XML
import Yesod
import Yesod.Default.Util

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Client.Conduit
import Network.HTTP.Types           (parseSimpleQuery)
import qualified Network.Wai as Wai

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

postLtiR :: Handler Html
postLtiR = getLtiR

getLtiR :: Handler Html
getLtiR = do
    (curReq, curBody) <- waiRequest >>= convert
    checkedReq <- checkOAuth ltiOAuth emptyCredential curReq
    (renderReq, medxRequest, medxRB) <- case checkedReq of
      Left err -> return ([whamlet| Incoming msg error: #{show $ err} |], Nothing, Nothing)
      Right req -> let pams = parseSimpleQuery (LB.toStrict curBody)
                       outcome_url = fmap (SBC.unpack . snd)
                                   . List.find ( ("lis_outcome_service_url" ==) . fst) $  pams
                       lis_result_sourcedid = fmap (SBC.unpack . snd)
                                   . List.find ( ("lis_result_sourcedid" ==) . fst) $  pams
                       msg_content = replaceResultTemplate
                                           "161346234345"
                                           (Text.pack $ fromJust lis_result_sourcedid)
                                           0.86
                                           "This is a text result data. Nothing interesting, just data"
            in if outcome_url == Nothing || lis_result_sourcedid == Nothing
            then return ([whamlet| Incoming msg error: failed to get some params |], Nothing, Nothing)
            else do
        ereq0 <- parseUrl $ fromJust outcome_url
        let ereq = ereq0 { method = "POST"
                         , requestBody = RequestBodyBS msg_content
                         , requestHeaders = [ ("Content-Type", "application/xml")]
                         }
        return ([whamlet|
            <h1>Incoming request
            Request URI:
            <pre>#{show $ getUri req}
            Request headers:
            <pre>
              $forall (pamn,pamv) <- requestHeaders req
                #{show pamn}: #{SBC.unpack pamv ++ "\n"}
            Request query parameters:
            <pre>
              $forall (pamn,pamv) <- parseSimpleQuery (queryString req)
                #{SBC.unpack pamn}: #{SBC.unpack pamv ++ "\n"}
            Response body:
            <pre>
              $forall (pamn,pamv) <- parseSimpleQuery (LB.toStrict curBody)
                #{SBC.unpack pamn}: #{SBC.unpack pamv ++ "\n"}
            |], Just ereq, Just msg_content)
    renderAns <- case (medxRequest, medxRB) of
     (Nothing, _) -> return [whamlet| Incoming msg error: no request |]
     (_, Nothing) -> return [whamlet| Incoming msg error: no request body |]
     (Just edxRequest, Just edxRB) -> do
      srequest <- signOAuth ltiOAuth emptyCredential edxRequest
      erequest <-checkOAuth ltiOAuth emptyCredential srequest
      case erequest of
       Left err -> return [whamlet| Outcoming msg error: #{show $ err} |]
       Right req -> do
        response <- withManager $ httpLbs srequest
        return [whamlet|
            <h1>EdX request
            Request URI:
            <pre>#{show $ getUri req}
            Request headers:
            <pre>
              $forall (pamn,pamv) <- requestHeaders req
                #{show pamn}: #{SBC.unpack pamv ++ "\n"}
            Request query parameters:
            <pre>
              $forall (pamn,pamv) <- parseSimpleQuery (queryString req)
                #{SBC.unpack pamn}: #{SBC.unpack pamv ++ "\n"}
            Request body:
            <pre>#{Text.decodeUtf8 $ edxRB}
            <br>
            Response headers:
            <pre>
              $forall (pamn,pamv) <- responseHeaders response
                #{show pamn}: #{SBC.unpack pamv ++ "\n"}
            Response body:
            <pre>#{LT.decodeUtf8 $ responseBody response}
        |]
    defaultLayout $ do
        setTitle "LTI test"
        renderReq
        renderAns
    where ltiOAuth = newOAuth
            { oauthServerName      = "test LTI oauth"
            , oauthSignatureMethod = HMACSHA1
            , oauthConsumerKey     = t_oauth_consumer_key
            , oauthConsumerSecret  = t_oauth_secret
            , oauthVersion         = OAuth10
            }
          convert wr = do
            body <- liftIO $ Wai.strictRequestBody wr
            return $ (def
                { method = Wai.requestMethod wr
                , secure = Wai.isSecure wr
                , host = whost
                , port = wport
                , path = Wai.rawPathInfo wr
                , queryString = Wai.rawQueryString wr
                , requestBody = RequestBodyLBS body
                , requestHeaders = Wai.requestHeaders wr
                }, body)
            where
              (whost,wport) = case SBC.span (':' /=) <$> Wai.requestHeaderHost wr of
                 Just (h, "") -> (h, if Wai.isSecure wr then 443 else 80)
                 Just (h, p)  -> (h, read . SBC.unpack $ SB.drop 1 p)
                 _ -> ("localhost", 80)


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

t_oauth_secret :: SB.ByteString
t_oauth_secret = "test_lti_secret"

