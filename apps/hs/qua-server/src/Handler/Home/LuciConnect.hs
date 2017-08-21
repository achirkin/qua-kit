{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Home.LuciConnect
  ( luciConnectPane
  ) where

import Import
import Text.Julius

luciConnectPane :: Handler (Text,Text, Widget)
luciConnectPane = do
  luciConnectedClass <- newIdent
  luciConnectingClass <- newIdent
  luciDisconnectedClass <- newIdent
  muser <- maybeAuth
  let urole = muserRole muser
  return . (,,) luciConnectedClass luciDisconnectedClass $ do
    connectButton <- newIdent
    luciProxyUrl <- newIdent
    luciConnectedInfo <- newIdent
    luciConnectingInfo <- newIdent

    hiddenPaneClass  <- newIdent
    visiblePaneClass <- newIdent

    let connectForm =
          [hamlet|
           <div style="margin: 10px 0 0 20px;">
            $if urole /= UR_NOBODY
              <div>
                Connect to Luci
              <div>
                <a.btn.btn-red.waves-attach.waves-light.waves-effect ##{connectButton}">
                  connect
                <div.form-group.form-group-label.control-highlight style="display: inline-block; margin: 12px 0px -12px 4px;">
                  <label.floating-label for="#{luciProxyUrl}">Host address
                  <input.form-control ##{luciProxyUrl} type="url" value="ws://localhost/luci">
            $else
              <div style="display: none;" ##{connectButton}">
                <input type="hidden" style="display: none;" ##{luciProxyUrl} type="url" value="ws://localhost/luci">
          |]
        connectedInfo =
          [hamlet|
            <div style="margin: 10px 0 0 20px;">
              Connected to Luci at
              <p ##{luciConnectedInfo} style="display: inline;">
           |]
        connectingInfo =
          [hamlet|
            <div style="margin: 10px 0 0 20px;">
              Connecting to Luci at
              <p ##{luciConnectingInfo} style="display: inline;">
              ...
           |]

    toWidgetHead
      [cassius|
       .#{hiddenPaneClass}
         display: none

       .#{visiblePaneClass}
         display: block
         margin: 0
         padding: 0
      |]
    toWidgetHead
      [julius|
        /** Registers one callback; comes from Handler.Home.PanelServices.
         *  onClick :: JSString -> IO () -- address of websocket host
         *  return :: IO ()
         */
        function registerUserConnectToLuci(onClick){
          document.getElementById('#{rawJS connectButton}').onclick = function(){
            var url = document.getElementById('#{rawJS luciProxyUrl}').value;
            if (url) {onClick(url);}
          };
        }

        /** Display "luci connected message"; comes from Handler.Home.PanelServices.
         *  connectedHost :: JSString -- address of websocket host
         *  return :: IO ()
         */
        function showLuciConnected(connectedHost){
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciDisconnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciDisconnectedClass} #{rawJS hiddenPaneClass}";
             });
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectingClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectingClass} #{rawJS hiddenPaneClass}";
             });
          document.getElementById('#{rawJS luciConnectedInfo}').innerText = connectedHost;
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectedClass} #{rawJS visiblePaneClass}";
             });
        }

        /** Display "luci connected message"; comes from Handler.Home.PanelServices.
         *  connectedHost :: JSString -- address of websocket host
         *  return :: IO ()
         */
        function showLuciConnecting(connectedHost){
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciDisconnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciDisconnectedClass} #{rawJS hiddenPaneClass}";
             });
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectedClass} #{rawJS hiddenPaneClass}";
             });
          document.getElementById('#{rawJS luciConnectingInfo}').innerText = connectedHost;
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectingClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectingClass} #{rawJS visiblePaneClass}";
             });
        }

        /** Display "connect to luci" form; comes from Handler.Home.PanelServices.
         *  defaultHost :: JSString -- default address of websocket host
         *  return :: IO ()
         */
        function showLuciConnectForm(defaultHost){
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectedClass} #{rawJS hiddenPaneClass}";
             });
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciConnectingClass}')).forEach(function(e){
               e.className = "#{rawJS luciConnectingClass} #{rawJS hiddenPaneClass}";
             });
          if (defaultHost) {
            document.getElementById('#{rawJS luciProxyUrl}').value = defaultHost;
          } else {
            document.getElementById('#{rawJS luciProxyUrl}').value = "";
          }
          Array.prototype.slice.call(
            document.getElementsByClassName('#{rawJS luciDisconnectedClass}')).forEach(function(e){
               e.className = "#{rawJS luciDisconnectedClass} #{rawJS visiblePaneClass}";
             });
        }
      |]
    toWidgetBody
      [hamlet|
        <div .#{luciDisconnectedClass}>
          ^{connectForm}
        <div .#{luciConnectedClass}>
          ^{connectedInfo}
        <div .#{luciConnectingClass} .#{hiddenPaneClass}>
          ^{connectingInfo}
      |]
