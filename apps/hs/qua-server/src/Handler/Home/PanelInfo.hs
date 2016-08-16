-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.PanelInfo
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Home.PanelInfo
  ( panelInfo
  ) where


import Import
import Text.Julius

panelInfo :: Widget
panelInfo = do
    infoPane <- newIdent
    infoRow <- newIdent
    infoRowOdd <- newIdent
    infoCellLeft <- newIdent
    infoCellRight <- newIdent
    toWidgetHead
      [cassius|
        .#{infoRow}
          display: table-row
          cursor: pointer
          font-size: 12px
          padding: 0px
          margin: 0px
          -webkit-touch-callout: none /* iOS Safari */
          -webkit-user-select: none   /* Chrome/Safari/Opera */
          -khtml-user-select: none    /* Konqueror */
          -moz-user-select: none      /* Firefox */
          -ms-user-select: none       /* Internet Explorer/Edge */
          user-select: none

        .#{infoRow}:hover
          background-color: #FF5722
          color: #FFFFFF

        .#{infoRow}:active
          background-color: #BF360C
          color: #FFFFFF

        .#{infoRowOdd}
          background-color: #FFF5EE

        .#{infoCellLeft}
          display: table-cell
          text-align: right
          padding: 3px 5px 3px 5px

        .#{infoCellRight}
          display: table-cell
          text-align: left
          padding: 3px 5px 3px 5px
          width: 100%
      |]
    toWidgetHead
      [julius|
        var colorizeProperty = function(n){};
        /** Registers one callback; comes from Handler.Home.PanelInfo.
         *  h :: JSString -> IO ()
         *  return :: IO ()
         */
        function registerColorizeProperty(f) {
          colorizeProperty = function(n){f(n);};
        }
        /** Show info (pairs of key-value); comes from Handler.Home.PanelInfo.
         *  obj :: Object -- all property names and values inside an object
         *  return :: IO ()
         */
        function showInfo(obj) {
          document.getElementById('#{rawJS infoPane}').innerHTML = Object.keys(obj).reduce(function(text, n, i){
                    if (obj[n] != null && (obj[n].constructor == Number || obj[n].constructor == String)) {
                      var classnames = i % 2 == 1 ? "#{rawJS infoRow}" : "#{rawJS infoRowOdd} #{rawJS infoRow}"
                      return text
                             + "<div class=\"" + classnames + "\" onclick=\"colorizeProperty('" + n + "')\" >"
                             + " <div class=\"#{rawJS infoCellLeft}\">" + n + "</div>"
                             + " <div class=\"#{rawJS infoCellRight}\">" + obj[n] + "</div>"
                             + "</div>\n";
                    } else {
                      return text;
                    }
                  }, "");
        }
      |]
    toWidgetBody
      [hamlet|
         <div ##{infoPane}>
      |]
