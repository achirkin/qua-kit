-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.LoggingWS
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.LoggingWS
  ( getQVLoggingR
  ) where



import Import

import Yesod.WebSockets
import Data.Conduit.Network as Network
import qualified Data.Conduit.List as CList
--import qualified Data.ByteString.Char8 as BSC


loggingApp :: WebSocketsT Handler ()
loggingApp = do
    sourceWS $$ mapM_C (\msg -> print (msg :: Text))
--            atomically $ writeTChan writingChan $ name <> ": " <> msg
--
--    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
--    name <- receiveData
--    sendTextData $ "Welcome, " <> name
--    writingChan <- appWSChan <$> getYesod
--    readingChan <- atomically $ do
--        writeTChan writingChan $ name <> " has joined the chat"
--        dupTChan writingChan
--    race_
--        (forever $ atomically (readTChan readingChan) >>= sendTextData)
--        (sourceWS $$ mapM_C (\msg ->
--            atomically $ writeTChan writingChan $ name <> ": " <> msg))

--  mluciAddr <- lift $ appLuciAddress . appSettings <$> getYesod
--  case mluciAddr of
--    Nothing -> return () -- sendTextData ("No default Luci host specified in settings." :: Text)
--    Just luciAddr -> runGeneralTCPClient luciAddr $ \appData -> do
--      -- sendTextData ("Connected to Luci on " <> getHost luciAddr <> ":" <> BSC.pack (show $ getPort luciAddr))
--      let toLuci :: Consumer ByteString (WebSocketsT Handler) ()
--          toLuci = Network.appSink appData
--          fromLuci :: Producer (WebSocketsT Handler) ByteString
--          fromLuci = Network.appSource appData
--          toClient :: Consumer ByteString (WebSocketsT Handler) ()
--          toClient = sinkWSBinary
--          fromClient :: Producer (WebSocketsT Handler) ByteString
--          fromClient = sourceWS
--      race_
--         (runConduit $ fromClient =$= CList.mapM (\x -> (liftIO . print $ "CLIENT: " <> x) >> return x) =$= toLuci)
--         (runConduit $ fromLuci =$= CList.mapM (\x -> (liftIO . print $ "LUCI: " <> x) >> return x) =$= toClient)


getQVLoggingR :: Handler Html
getQVLoggingR = do
    webSockets loggingApp
    notFound

