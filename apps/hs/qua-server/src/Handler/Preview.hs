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
--import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import Text.Blaze
import Yesod
import Yesod.Default.Util


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
    srequest <- signOAuth ltiOAuth ltiCred request
    response <- withManager $ httpLbs (addHash srequest){queryString=""}
    defaultLayout $ do
        setTitle "LTI test"
        toWidgetBody (unsafeLazyByteString $ responseBody response)
    where ltiOAuth = newOAuth
            { oauthServerName      = "test LTI oauth"
            , oauthSignatureMethod = HMACSHA1
            , oauthConsumerKey     = t_oauth_consumer_key
            , oauthConsumerSecret  = t_oauth_secret
            , oauthVersion         = OAuth10
            }
         -- ltiCred = newCredential "empty_credential"
         --                         t_oauth_secret
          ltiCred = OAuth.insert "oauth_body_hash" (Base64.encode $ hash t_msg_content) emptyCredential
          request = setQueryString [("oauth_body_hash", Just . Base64.encode $ SHA1.hash t_msg_content)]
                                   $ def
            { method = "POST"
            , secure = True
            , host = "courses.edx.org"
            , port = 443
            , path = "/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@2fc0ef710f2c4e9ca6d2c31fc5731ff5/handler_noauth/outcome_service_handler"

            , requestBody = RequestBodyBS t_msg_content
            , requestHeaders = [ ("Content-Type", "application/xml")
                               ]
            }
          encodedHash = last . C.split '=' $ queryString request
          addHash req = req { requestHeaders = rhs }
            where rhs = appendHeader <$> requestHeaders req
                  appendHeader (hn, hv) | hn == "Authorization" = (hn, hv
                                                                      `SB.append` ",oauth_body_hash=\""
                                                                      `SB.append` encodedHash
                                                                      `SB.append` "\"")
                                        | otherwise = (hn,hv)


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

--("Authorization", "OAuth oauth_body_hash=\""
--                                      `SB.append` Base64.encode (hash t_msg_content)
--                                      `SB.append` "\"")


--Consumer key
--The oauth_consumer_key identifies which application is making the request. Obtain this value from checking the settings page for your application on dev.twitter.com/apps.
--
--oauth_consumer_key	xvz1evFS4wEEPTGEFPHBog
--Nonce
--The oauth_nonce parameter is a unique token your application should generate for each unique request. Twitter will use this value to determine whether a request has been submitted multiple times. The value for this request was generated by base64 encoding 32 bytes of random data, and stripping out all non-word characters, but any approach which produces a relatively random alphanumeric string should be OK here.

t_oauth_consumer_key :: SB.ByteString
t_oauth_consumer_key = "test_lti_key"

--oauth_nonce	kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg
--Signature
--The oauth_signature parameter contains a value which is generated by running all of the other request parameters and two secret values through a signing algorithm. The purpose of the signature is so that Twitter can verify that the request has not been modified in transit, verify the application sending the request, and verify that the application has authorization to interact with the user’s account.
--
--The process for calculating the oauth_signature for this request is described in Creating a signature.
-- t_oauth_nonce = "3616602192130518246"

--oauth_signature	tnnArxj06cWHq44gCs1OSKk/jLY=
--Signature method
--The oauth_signature_method used by Twitter is HMAC-SHA1. This value should be used for any authorized request sent to Twitter’s API.
--
--oauth_signature_method	HMAC-SHA1
--Timestamp
--The oauth_timestamp parameter indicates when the request was created. This value should be the number of seconds since the Unix epoch at the point the request is generated, and should be easily generated in most programming languages. Twitter will reject requests which were created too far in the past, so it is important to keep the clock of the computer generating requests in sync with NTP.
--
--oauth_timestamp	1318622958
--Token
--The oauth_token parameter typically represents a user’s permission to share access to their account with your application. There are a few authentication requests where this value is not passed or is a different form of token, but those are covered in detail in Obtaining access tokens. For most general-purpose requests, you will use what is referred to as an access token. You can generate a valid access token for your account on the settings page for your application at dev.twitter.com/apps.
--
--oauth_token	370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb
-- Since we are using OAuth in a signing-only scenario (i.e., we are not using OAuth to transfer third-party identity), there is no need for an oauth_token.

t_url :: String
t_url = "https://courses.edx.org/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@2fc0ef710f2c4e9ca6d2c31fc5731ff5/handler_noauth/outcome_service_handler"



t_oauth_secret :: SB.ByteString
t_oauth_secret = "test_lti_secret"

t_msg_content :: SB.ByteString
t_msg_content =
    "<?xml version = \"1.0\" encoding = \"UTF-8\"?>\
    \<imsx_POXEnvelopeRequest xmlns=\"http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0\">\
    \  <imsx_POXHeader>\
    \    <imsx_POXRequestHeaderInfo>\
    \      <imsx_version>V1.0</imsx_version>\
    \      <imsx_messageIdentifier>335349992332</imsx_messageIdentifier>\
    \    </imsx_POXRequestHeaderInfo>\
    \  </imsx_POXHeader>\
    \  <imsx_POXBody>\
    \    <replaceResultRequest>\
    \      <resultRecord>\
    \        <sourcedGUID>\
    \          <sourcedId>course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-2fc0ef710f2c4e9ca6d2c31fc5731ff5:a9f4ca11f3b686a7bf12812326dfcb1c</sourcedId>\
    \        </sourcedGUID>\
    \        <result>\
    \          <resultScore>\
    \            <textString>0.7</textString>\
    \          </resultScore>\
    \          <resultData>\
    \            <text>Here is some result data text. I am not sure, how much of data can I keep here though...</text>\
    \          </resultData>\
    \        </result>\
    \      </resultRecord>\
    \    </replaceResultRequest>\
    \  </imsx_POXBody>\
    \</imsx_POXEnvelopeRequest>"
