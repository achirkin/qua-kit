-----------------------------------------------------------------------------
-- |
-- Module      :  Web.LTI
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.LTI
    ( -- * Data types
      LTIProvider, ltiOAuth, newLTIProvider
    , LTIException (..)
      -- * simplifiers
    , gradeRequest
      -- * Incoming requests
    , processRequest, processWaiRequest
      -- * Outgoing requests
    , replaceResultRequest
    ) where

import           Blaze.ByteString.Builder             (toByteString)
import           Control.Exception
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString             as SB
import qualified Data.ByteString.Char8       as SBC
import qualified Data.ByteString.Lazy        as LB
import           Data.Data                            (Data, Typeable)
import           Data.Default
import qualified Data.IORef                  as I
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                            (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import           Web.Authenticate.OAuth               (OAuth)
import qualified Web.Authenticate.OAuth      as OAuth
import           Text.Hamlet.XML
import           Text.XML
import           Network.HTTP.Client                  ( Request (..)
                                                      , RequestBody (..)
                                                      , GivesPopper
                                                      , parseUrl)
import           Network.HTTP.Types                   (parseSimpleQuery)
import qualified Network.Wai                 as Wai
import           System.Random                        (Random(..))


-- | LTI 1.1 Service provider
newtype LTIProvider = LTIProvider
    { ltiOAuth :: OAuth
    }

instance Default LTIProvider where
  def = LTIProvider
    { ltiOAuth = def
      { OAuth.oauthServerName      = "LTI 1.1 consumer"
      , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
      , OAuth.oauthVersion         = OAuth.OAuth10
      }
    }

-- | Create a new default LTIProvider with given provider token and secret
newLTIProvider :: ByteString
               -> ByteString
               -> LTIProvider
newLTIProvider token secret = p
    { ltiOAuth = (ltiOAuth p)
      { OAuth.oauthConsumerKey     = token
      , OAuth.oauthConsumerSecret  = secret
      }
    } where p = def

newtype LTIException = LTIException String
                    deriving (Show, Eq, Data, Typeable)

instance Exception LTIException

----------------------------------------------------------------------------------------------------

-- | Get url encoded data from Network.HTTP.Client.Request
processRequest :: MonadIO m
               => LTIProvider
               -> Request
               -> m (Map ByteString ByteString)
processRequest prov req = toBS (requestBody req)
                        >>= processRequest' prov req

-- | Get url encoded data from Network.Wai.Request
processWaiRequest :: MonadIO m
                  => LTIProvider
                  -> Wai.Request
                  -> m (Map ByteString ByteString)
processWaiRequest prov wreq = do
    (req, rbody) <- convert wreq
    processRequest' prov req $ LB.toStrict rbody
  where
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
           Just (h, po)  -> (h, read . SBC.unpack $ SB.drop 1 po)
           _ -> ("localhost", 80)


processRequest' :: MonadIO m
                => LTIProvider
                -> Request
                -> ByteString
                -> m (Map ByteString ByteString)
processRequest' prov req rbody = do
    echeckedReq  <- OAuth.checkOAuth (ltiOAuth prov) OAuth.emptyCredential req
    case echeckedReq of
      Left err -> liftIO $ throwIO err
      Right _  -> return . Map.fromList $ parseSimpleQuery rbody


----------------------------------------------------------------------------------------------------

-- | Being supplied with processRequest function
--   creates a proper grade response to the service consumer.
gradeRequest :: MonadIO m
             => LTIProvider
             -> (LTIProvider -> request -> m (Map ByteString ByteString))
             -- ^ processRequest or processWaiRequest
             -> request
             -- ^ request value (e.g. from Network.HTTP.Client or Network.Wai)
             -> m ( Double -> m Request
                  , Map ByteString ByteString
                  )
             -- ^ returns a grading function and a request parameter map
gradeRequest prov proc req = do
    pams <- proc prov req
    let murl = SBC.unpack      <$> Map.lookup "lis_outcome_service_url" pams
        msid = Text.decodeUtf8 <$> Map.lookup "lis_result_sourcedid" pams
    case (murl,msid) of
      (Nothing, _) -> liftIO . throwIO $ LTIException
            "Client request does not contain lis_outcome_service_url."
      (_, Nothing) -> liftIO . throwIO $ LTIException
            "Client request does not contain lis_result_sourcedid."
      (Just url, Just sid) ->
        return (\r -> replaceResultRequest prov url sid r Nothing, pams)


----------------------------------------------------------------------------------------------------

-- | Create a replaceResult request to send it to an LTI consumer
replaceResultRequest :: MonadIO m
                     => LTIProvider
                     -> String       -- ^ url of service consumer
                     -> Text         -- ^ sourcedId (student id)
                     -> Double       -- ^ resultScore (grade between 0.0 and 1.0)
                     -> Maybe Text   -- ^ resultData (not sure if edX stores it though)
                     -> m Request
replaceResultRequest prov url sourceId resultScore resultData = do
  ctime <- liftIO $ getPOSIXTime
  crand <- liftIO (randomIO :: IO Word)
  req0 <- liftIO $ parseUrl url
  let req = req0 { method = "POST"
                 , requestBody = RequestBodyBS rbody
                 , requestHeaders = [ ("Content-Type", "application/xml")]
                 }
      imsxMessageId = Text.pack $ show ctime ++ show crand
      rbody = replaceResultTemplate imsxMessageId sourceId resultScore
                (case resultData of
                    Nothing -> ""
                    Just da -> da)
  OAuth.signOAuth (ltiOAuth prov) OAuth.emptyCredential req


----------------------------------------------------------------------------------------------------

encodeTemplate :: [Node] -> ByteString
encodeTemplate (NodeElement el :_) = LB.toStrict
                                   . renderLBS def{rsPretty = False}
                                   $ Document (Prologue [] Nothing []) el []
encodeTemplate (_:xs)              = encodeTemplate xs
encodeTemplate []                  = SB.empty


replaceResultTemplate :: Text -> Text -> Double -> Text -> ByteString
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
--                <imsx_description>Score for course-v1%3...c is now 0.7</imsx_description>
--                <imsx_messageRefIdentifier>
--                </imsx_messageRefIdentifier>
--            </imsx_statusInfo>
--        </imsx_POXResponseHeaderInfo>
--    </imsx_POXHeader>
--    <imsx_POXBody><replaceResultResponse/></imsx_POXBody>

--readResultTemplate :: Text -> Text -> ByteString
--readResultTemplate imsxMessageId sourceId = encodeTemplate
--  [xml|<imsx_POXEnvelopeRequest xmlns="http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">
--         <imsx_POXHeader>
--           <imsx_POXRequestHeaderInfo>
--             <imsx_version>V1.0
--             <imsx_messageIdentifier>#{imsxMessageId}
--           <imsx_POXBody>
--             <readResultRequest>
--               <resultRecord>
--                 <sourcedGUID>
--                   <sourcedId>#{sourceId}
--  |]

----------------------------------------------------------------------------------------------------
-- Copied from Web.Authenticate.OAuth
toBS :: MonadIO m => RequestBody -> m ByteString
toBS (RequestBodyLBS l) = return $ LB.toStrict l
toBS (RequestBodyBS s) = return s
toBS (RequestBodyBuilder _ b) = return $ toByteString b
toBS (RequestBodyStream _ givesPopper) = toBS' givesPopper
toBS (RequestBodyStreamChunked givesPopper) = toBS' givesPopper

toBS' :: MonadIO m => GivesPopper () -> m ByteString
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
