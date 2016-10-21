-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.LuciProxy
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Handler.LuciProxy where


import Import

import Yesod.WebSockets
import Data.Conduit.Network as Network
import qualified Data.Conduit.List as CList
--import qualified Data.ByteString.Char8 as BSC

import Data.Bits (shiftR, (.&.))

--chatApp :: WebSocketsT Handler ()
--chatApp = do
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
--
--getChatR :: Handler Html
--getChatR = do
--    webSockets chatApp
--    defaultLayout $
--      [whamlet|
--        <textarea id="tinput" rows="2" cols="50">
--        <button onclick="sendText()">send!
--        <div id="dout">
--        <script type="text/javascript">
--          var chatSocket = new WebSocket("ws" + "@{LuciR}".substr(4));
--          var inField = document.getElementById("tinput");
--          var outField = document.getElementById("dout");
--          function sendText() {
--            chatSocket.send(inField.value);
--            inField.value = "";
--          };
--          chatSocket.onmessage = function (event) {
--            outField.innerHTML = event.data + "<br/>" + outField.innerHTML;
--          };
--      |]

luciApp :: WebSocketsT Handler ()
luciApp = do
  mluciAddr <- lift $ appLuciAddress . appSettings <$> getYesod
  case mluciAddr of
    Nothing -> sendBinaryData (makeSimpleLuciMessage $ "{\"wsError\":\"Luci connection is not set up.\"}")
    Just luciAddr -> do
      let toClient :: Consumer ByteString (WebSocketsT Handler) ()
          toClient = sinkWSBinary
          fromClient :: Producer (WebSocketsT Handler) ByteString
          fromClient = sourceWS
      runGeneralTCPClient luciAddr $ \appData -> do
        sendBinaryData (makeSimpleLuciMessage $
                "{\"wsSuccess\":\""
                <> "Connected to Luci on " <> decodeUtf8 (getHost luciAddr) <> ":" <> (pack . show $ getPort luciAddr)
                <> "\"}"
                )
        -- sendTextData ("Connected to Luci on " <> getHost luciAddr <> ":" <> BSC.pack (show $ getPort luciAddr))
        let toLuci :: Consumer ByteString (WebSocketsT Handler) ()
            toLuci = Network.appSink appData
            fromLuci :: Producer (WebSocketsT Handler) ByteString
            fromLuci = Network.appSource appData
        race_
           (runConduit $ fromClient =$= CList.mapM (\x -> ($(logDebug) $ "CLIENT: " <> decodeUtf8 x) >> return x) =$= toLuci)
           (runConduit $ fromLuci =$= CList.mapM (\x -> ($(logDebug) $ "LUCI: " <> decodeUtf8 x) >> return x) =$= toClient)
      sendBinaryData (makeSimpleLuciMessage $
              "{\"wsTerminate\":\""
              <> "Connection to Luci has been closed."
              <> "\"}"
              )


makeSimpleLuciMessage :: Text -> ByteString
makeSimpleLuciMessage s = bsL <> replicate 7 0 <> replicate 1 8 <> msg <> replicate 8 0
  where
    msg = encodeUtf8 s
    l = fromIntegral (length msg) :: Int64
    bsL = pack $ map (\d -> fromIntegral $ 0xFF .&. shiftR l d) [56,48..0]

getLuciR :: Handler Html
getLuciR = do
    webSockets luciApp
    defaultLayout $ do
      [whamlet|
        Connect to Luci
      |]
--    defaultLayout $ do
--      toWidgetHead $
--        [hamlet|
--          <script src="@{StaticR js_LuciClient_js}" type="text/javascript">
--        |]
--      [whamlet|
--        <textarea id="tinput" rows="2" cols="50">
--        <button onclick="sendText()">send!
--        <div id="dout">
--        <script type="text/javascript">
--          var inField = document.getElementById("tinput");
--          var outField = document.getElementById("dout");
--          var lc = new Luci.Client("ws" + "@{LuciR}".substr(4), function(msghead, atts) {
--            console.log("header", JSON.parse(msghead));
--            console.log("attachments", atts);
--            outField.innerHTML = msghead + "<br/>" + outField.innerHTML;
--          });
--
--          function sendText() {
--            lc.sendMessage(inField.value,
--               [ new Uint8Array("Hello world Attachment!!!".split('').map(function(c) {return c.charCodeAt(0);}))
--               , new Uint8Array("+hell+".split('').map(function(c) {return c.charCodeAt(0);}))
--               ]);
--            inField.value = "";
--          };
--      |]


--mouseDebugApp :: WebSocketsT Handler ()
--mouseDebugApp = do
--  sourceWS $$ mapM_C $(logInfo)
--
--getMouseDebugR :: Handler Html
--getMouseDebugR = do
--    webSockets mouseDebugApp
--    defaultLayout $ do
--      [whamlet|
--        Debug mouse
--      |]
