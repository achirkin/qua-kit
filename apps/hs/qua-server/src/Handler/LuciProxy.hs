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


chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    writingChan <- appWSChan <$> getYesod
    readingChan <- atomically $ do
        writeTChan writingChan $ name <> " has joined the chat"
        dupTChan writingChan
    race_
        (forever $ atomically (readTChan readingChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writingChan $ name <> ": " <> msg))

getLuciR :: Handler Html
getLuciR = do
    webSockets chatApp
    defaultLayout $
      [whamlet|
        <textarea id="tinput" rows="2" cols="50">
        <button onclick="sendText()">send!
        <div id="dout">
        <script type="text/javascript">
          var chatSocket = new WebSocket("ws" + "@{LuciR}".substr(4));
          var inField = document.getElementById("tinput");
          var outField = document.getElementById("dout");
          function sendText() {
            chatSocket.send(inField.value);
            inField.value = "";
          };
          chatSocket.onmessage = function (event) {
            outField.innerHTML = event.data + "<br/>" + outField.innerHTML;
          };
      |]
