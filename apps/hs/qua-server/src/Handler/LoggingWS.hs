{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.LoggingWS
  ( getQVLoggingR
  ) where



import Import

import Yesod.WebSockets
import Handler.Mooc.User (maybeFetchExerciseId)
import Network.Wai (remoteHost)
import Network.Socket
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Char (isSpace)

loggingApp :: Maybe UserId -> Maybe ExerciseId -> Maybe Text -> WebSocketsT Handler ()
loggingApp uId exId ipAddr = sourceWS $$ mapM_C (\msg -> do
       t <- liftIO getCurrentTime
       lift . runDB . insert_ $ QuaViewWebLogging uId exId ipAddr t msg
     )


getQVLoggingR :: Handler Html
getQVLoggingR = do
    mUId  <- maybeAuthId
    mExId <- case mUId of
      Just uid -> maybeFetchExerciseId uid
      Nothing  -> return Nothing
    mHeadRealIp <- lookupHeader "X-Real-IP"
    mHeadForwardedFor <- lookupHeader "X-Forwarded-For"
    mReqIp <- getAddr . remoteHost <$> waiRequest
    webSockets $ loggingApp mUId mExId (findIp mHeadRealIp mHeadForwardedFor mReqIp)
    notFound
  where
    toT = fmap Text.decodeUtf8
    mh [] = Nothing
    mh (x:_) = Just x
    getFst = join
           . fmap ( mh
                  . filter (not . Text.null)
                  . map (Text.filter (not . isSpace))
                  . Text.split (','==))
    findIp op1 op2 op3 = toT op1 <|> getFst (toT op2) <|> op3
    getAddr (SockAddrInet _ ha) = case hostAddressToTuple ha of
      (a,b,c,d) -> Just $
        tshow a <> "." <> tshow b <> "." <> tshow c <> "." <> tshow d
    getAddr (SockAddrInet6 _ _ ha6 _) = case hostAddress6ToTuple ha6 of
      (a,b,c,d,e,f,g,h) -> Just . toStrict . TLB.toLazyText $
        TLB.hexadecimal a <> ":" <>
        TLB.hexadecimal b <> ":" <>
        TLB.hexadecimal c <> ":" <>
        TLB.hexadecimal d <> ":" <>
        TLB.hexadecimal e <> ":" <>
        TLB.hexadecimal f <> ":" <>
        TLB.hexadecimal g <> ":" <>
        TLB.hexadecimal h
    getAddr _ = Nothing
