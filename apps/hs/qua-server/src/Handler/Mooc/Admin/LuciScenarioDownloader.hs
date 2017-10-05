{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Admin.LuciScenarioDownloader
    ( getLuciScenarioDownloaderR
    ) where

import Import hiding ((==.), on)
import Text.Julius

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Text.Encoding as TE
import qualified Text.Blaze as Blaze

import Yesod.Form.Bootstrap3

import Handler.Mooc.Admin

getLuciScenarioDownloaderR :: Handler Html
getLuciScenarioDownloaderR = do
    requireAdmin
    app <- getYesod
    req <- waiRequest
    let appr = getApprootText guessApproot app req
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
    adminLayout "Luci scenarios" $ do
      toWidgetHead
        [hamlet|
          <script src="@{StaticR js_LuciClient_js}" type="text/javascript">
        |]
      scenariosRow <- newIdent
      scenariosRowOdd <- newIdent
      scenariosCell <- newIdent
      toWidgetHead
        [cassius|
          .#{scenariosRow}
            display: table-row
            cursor: pointer
            -webkit-touch-callout: none /* iOS Safari */
            -webkit-user-select: none   /* Chrome/Safari/Opera */
            -khtml-user-select: none    /* Konqueror */
            -moz-user-select: none      /* Firefox */
            -ms-user-select: none       /* Internet Explorer/Edge */
            user-select: none

          .#{scenariosRow}:hover
            background-color: #FF5722
            color: #FFFFFF

          .#{scenariosRow}:active
            background-color: #BF360C
            color: #FFFFFF

          .#{scenariosRowOdd}
            background-color: #FFF5EE

          .#{scenariosCell}
            display: table-cell
        |]
      setTitle "qua-kit - luci scenarios"
      [whamlet|
        <div .row>
          <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-6>
            <div .card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no.text-brand-accent>
                        Parameters
                  <div.card-inner>
                    <div>
                      <input type="checkbox" id="doPrettyPrint" name="Enable pretty printing" value="on" checked>
                      <label for="doPrettyPrint">
                        Enable pretty printing
                    <div>
                      <input type="checkbox" id="forceSrid" name="Force SRID value" value="on">
                      <label for="forceSrid">
                          Force SRID value
                      <input type="number" id="forceSridN" value="4326">

          <div .col-lg-12 .col-md-12 .col-sm-12 .col-xs-12>
            <div .card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no.text-brand-accent>
                        Scenarios
                  <div.card-inner>
                    <div #scenarioListTable style="display: table; width: 100%;">

      |]
      toWidgetBody
        [julius|
          function displayScenarios(xs) {
            document.getElementById('scenarioListTable').innerHTML = xs.reduce(function(text, scDesc, i){
                var classnames = i % 2 == 0 ? "#{rawJS scenariosRow}" : "#{rawJS scenariosRowOdd} #{rawJS scenariosRow}";
                return  "<div class=\"" + classnames + "\" onclick='askLuciForScenario(" + scDesc.ScID + ")'>"
                         + "<div class=\"#{rawJS scenariosCell}\">" + scDesc.ScID + "</div>"
                         + "<div class=\"#{rawJS scenariosCell}\">" + scDesc.name + "</div>"
                         + "<div class=\"#{rawJS scenariosCell}\">" + formatDate(scDesc.created) + "</div>"
                         + "<div class=\"#{rawJS scenariosCell}\">" + formatDate(scDesc.lastmodified)  + "</div>"
                         + "</div>\n" + text;
              }, "");
          }
          function formatDate(t) {
            var d = new Date(t*1000);
            return d.getFullYear() + "." + ('0' + (d.getMonth() + 1)).slice(-2) + "." + ('0' + d.getDate()).slice(-2) + " "
                   + d.getHours() + ":" + ('0' + d.getMinutes()).slice(-2) + ":" + ('0' + d.getSeconds()).slice(-2);
          }
          function askLuciForScenario(scID) {
            var c =
              { run: "scenario.geojson.Get"
              , ScID: scID
              };
            var sridN = parseInt(document.getElementById("forceSridN").value);
            if (document.getElementById("forceSrid").checked && !isNaN(sridN)) {
              c.srid = sridN;
            }
            console.log("sending:", c);
            luci.sendMessage(JSON.stringify(c));
          }
          /*! @source http://purl.eligrey.com/github/FileSaver.js/blob/master/FileSaver.js */
          var saveAs=saveAs||function(e){"use strict";if(typeof e==="undefined"||typeof navigator!=="undefined"&&/MSIE [1-9]\./.test(navigator.userAgent)){return}var t=e.document,n=function(){return e.URL||e.webkitURL||e},r=t.createElementNS("http://www.w3.org/1999/xhtml","a"),o="download"in r,a=function(e){var t=new MouseEvent("click");e.dispatchEvent(t)},i=/constructor/i.test(e.HTMLElement)||e.safari,f=/CriOS\/[\d]+/.test(navigator.userAgent),u=function(t){(e.setImmediate||e.setTimeout)(function(){throw t},0)},s="application/octet-stream",d=1e3*40,c=function(e){var t=function(){if(typeof e==="string"){n().revokeObjectURL(e)}else{e.remove()}};setTimeout(t,d)},l=function(e,t,n){t=[].concat(t);var r=t.length;while(r--){var o=e["on"+t[r]];if(typeof o==="function"){try{o.call(e,n||e)}catch(a){u(a)}}}},p=function(e){if(/^\s*(?:text\/\S*|application\/xml|\S*\/\S*\+xml)\s*;.*charset\s*=\s*utf-8/i.test(e.type)){return new Blob([String.fromCharCode(65279),e],{type:e.type})}return e},v=function(t,u,d){if(!d){t=p(t)}var v=this,w=t.type,m=w===s,y,h=function(){l(v,"writestart progress write writeend".split(" "))},S=function(){if((f||m&&i)&&e.FileReader){var r=new FileReader;r.onloadend=function(){var t=f?r.result:r.result.replace(/^data:[^;]*;/,"data:attachment/file;");var n=e.open(t,"_blank");if(!n)e.location.href=t;t=undefined;v.readyState=v.DONE;h()};r.readAsDataURL(t);v.readyState=v.INIT;return}if(!y){y=n().createObjectURL(t)}if(m){e.location.href=y}else{var o=e.open(y,"_blank");if(!o){e.location.href=y}}v.readyState=v.DONE;h();c(y)};v.readyState=v.INIT;if(o){y=n().createObjectURL(t);setTimeout(function(){r.href=y;r.download=u;a(r);h();c(y);v.readyState=v.DONE});return}S()},w=v.prototype,m=function(e,t,n){return new v(e,t||e.name||"download",n)};if(typeof navigator!=="undefined"&&navigator.msSaveOrOpenBlob){return function(e,t,n){t=t||e.name||"download";if(!n){e=p(e)}return navigator.msSaveOrOpenBlob(e,t)}}w.abort=function(){};w.readyState=w.INIT=0;w.WRITING=1;w.DONE=2;w.error=w.onwritestart=w.onprogress=w.onwrite=w.onabort=w.onerror=w.onwriteend=null;return m}(typeof self!=="undefined"&&self||typeof window!=="undefined"&&window||this.content);if(typeof module!=="undefined"&&module.exports){module.exports.saveAs=saveAs}else if(typeof define!=="undefined"&&define!==null&&define.amd!==null){define("FileSaver.js",function(){return saveAs})};
          function downloadScenario(geomOutput) {
             var text = document.getElementById("doPrettyPrint").checked
               ? JSON.stringify(geomOutput, null, 2)
               : JSON.stringify(geomOutput);
             var filename = geomOutput.name + ".scenario";
             var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
             saveAs(blob, filename);
          }
          var onMsg = function(msg) {
            var j = JSON.parse(msg);
            if(!j.hasOwnProperty("result")) {
              console.log(j);
            } else {
              var result = j.result;
              if(result.hasOwnProperty("scenarios")){
                displayScenarios(result.scenarios);
              } else if (result.hasOwnProperty("geometry_output")){
                downloadScenario(result.geometry_output);
              } else {
                console.log(result);
              }
            }
          };
          var onError = function(err) {console.log(err);};
          var onOpen  = function(){
            console.log("Connection open!");
            luci.sendMessage(JSON.stringify({
                run: "scenario.GetList"
              }));
          };
          var onClose = function(){console.log("Luci connection closed!");};
          var luci = new Luci.Client("#{rawJS luciProxyWS}", onMsg, onOpen, onClose, onError);
        |]
