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
      LTIProvider, ltiOAuth
    , LTIException (..)
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
--import qualified Data.Text.Encoding          as Text
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
data LTIProvider = LTIProvider
    { ltiOAuth :: OAuth
    }

instance Default LTIProvider where
  def = LTIProvider
    { ltiOAuth = def
    }

newtype LTIException = LTIException String
                    deriving (Show, Eq, Data, Typeable)

instance Exception LTIException

-- | Get url encoded data from Network.HTTP.Client.Request
processRequest :: MonadIO m
               => LTIProvider
               -> Request
               -> m (Map ByteString ByteString)
processRequest prov req = do
    echeckedReq <- OAuth.checkOAuth (ltiOAuth prov) OAuth.emptyCredential req
    case echeckedReq of
      Left  err        -> liftIO $ throwIO err
      Right checkedReq -> do
        rbody <- toBS $ requestBody checkedReq
        return . Map.fromList $ parseSimpleQuery rbody

-- | Get url encoded data from Network.Wai.Request
processWaiRequest :: MonadIO m
                  => LTIProvider
                  -> Wai.Request
                  -> m (Map ByteString ByteString)
processWaiRequest prov wreq = do
    (req, rbody) <- convert wreq
    echeckedReq  <- OAuth.checkOAuth (ltiOAuth prov) OAuth.emptyCredential req
    case echeckedReq of
      Left err -> liftIO $ throwIO err
      Right _  -> return . Map.fromList . parseSimpleQuery $ LB.toStrict rbody
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
--                <imsx_description>Score for course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-2fc0ef710f2c4e9ca6d2c31fc5731ff5:a9f4ca11f3b686a7bf12812326dfcb1c is now 0.7</imsx_description>
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

-------------------------------------------------------
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
