-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.PanelServices
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Home.PanelServices
  ( panelServices
  ) where

import Import
import Text.Julius

-- | Rotating circular progress bar;
--   exposes one dom id '#loadingSplash'.
panelServices :: Widget
panelServices = do
  luciConnectForm <- newIdent
  connectButton <- newIdent
  luciProxyUrl <- newIdent

  luciConnectedPane <- newIdent
  luciConnectedInfo <- newIdent

  hiddenPaneClass  <- newIdent
  visiblePaneClass <- newIdent

  let connectForm =
        [hamlet|
          <div .pheading>
            Connect to Luci
          <div style="display: table; width: 100%; margin: 20px auto 10px 0px; z-index: 5;">
            <div style="display: table-cell; width: 6em; margin: 0; padding 0; vertical-align: middle;">
              <button .button ##{connectButton} type="button" style="width: 100%; margin: 8px 0px 0px 0px; padding: 4px 12px 2px 12px;">
                Connect
            <div style="display: table-cell; margin: 0; padding 0;">
              <div .group style="">
                <input .pinput ##{luciProxyUrl} onblur="checkIfUsed('#{luciProxyUrl}')" type="url">
                <span .phighlight>
                <span .pbar>
                <label .plabel>
                  Host address
        |]
      connectedInfo =
        [hamlet|
          <div .pheading>
            Connected to Luci at
            <p ##{luciConnectedInfo} style="display: inline;">
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
      // helper for showing input fields correctly
      function checkIfUsed(id) {
              var el = document.getElementById(id);
              if (el.value==null || el.value=="") { el.className = "pinput"; }
              else { el.className = "pinput used"; }
      }
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
        document.getElementById('#{rawJS luciConnectForm}').className = "#{rawJS hiddenPaneClass}";
        document.getElementById('#{rawJS luciConnectedInfo}').innerText = connectedHost;
        document.getElementById('#{rawJS luciConnectedPane}').className = "#{rawJS visiblePaneClass}";
      }

      /** Display "connect to luci" form; comes from Handler.Home.PanelServices.
       *  defaultHost :: JSString -- default address of websocket host
       *  return :: IO ()
       */
      function showLuciConnectForm(defaultHost){
        document.getElementById('#{rawJS luciConnectedPane}').className = "#{rawJS hiddenPaneClass}";
        if (defaultHost) {
          document.getElementById('#{rawJS luciProxyUrl}').value = defaultHost;
        } else {
          document.getElementById('#{rawJS luciProxyUrl}').value = "";
        }
        document.getElementById('#{rawJS luciConnectForm}').className = "#{rawJS visiblePaneClass}";
        checkIfUsed('#{rawJS luciProxyUrl}');
      }
    |]
  toWidgetBody
    [hamlet|
      <div ##{luciConnectForm} .#{hiddenPaneClass}>
        ^{connectForm}
      <div ##{luciConnectedPane} .#{hiddenPaneClass}>
        ^{connectedInfo}
    |]
