-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.PanelGeometry
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Home.PanelGeometry
  ( panelGeometry
  ) where

import Import
import Text.Julius

-- | Rotating circular progress bar;
--   exposes one dom id '#loadingSplash'.
panelGeometry :: Widget
panelGeometry = do
  dynamicstaticswitcher <- newIdent
  jsonfileinput <- newIdent
  toWidgetHead
    [cassius|

      ##{dynamicstaticswitcher} + .label:after
        content: "Static geometry"
        display: block
        white-space: nowrap
        overflow: visible
        position: absolute
        margin: 10px 5px 5px 5px
        padding: 0px
        left: 60px
        top: 2px

      ##{dynamicstaticswitcher}:checked + .label:after
        content: "Dynamic geometry"
    |]
  toWidgetHead
    [julius|
        "use strict"
        /** Registers two callbacks; comes from Handler.Home.PanelGeometry.
         *  onSuccess :: JSON -> IO ()
         *  onFailure :: JSString -> IO ()
         *  return :: IO ()
         */
        function registerLoadingFile(onSuccess, onFailure) {
          var jsonFileInputButton = document.getElementById('#{rawJS jsonfileinput}');
          jsonFileInputButton.onchange = function() {
            var r = new FileReader();
            r.onloadend = function() {
              if (r.readyState != FileReader.EMPTY ) {
                var json = null;
                try {
                  json = JSON.parse(r.result);
                } catch (err) {onFailure('Your browser does not like JSON file you have chosen: ' + err);}
                onSuccess(json);
              }
            };
            r.onerror = function() {onFailure('Your browser cannot open the file chosen.');};
            r.readAsText(jsonFileInputButton.files[0]);
          };
        }
        function displayUploadedFileName() {
          var jsonFileInputButton = document.getElementById('#{rawJS jsonfileinput}');
          document.getElementById('filenameIndicator').innerText = jsonFileInputButton.value.split(/(\\|\/)/g).pop();
        }
        function clearUploadedFileName() {
          var jsonFileInputButton = document.getElementById('#{rawJS jsonfileinput}');
          document.getElementById('filenameIndicator').innerText = '';
          jsonFileInputButton.value = '';
        }
    |]
  toWidgetBody
    [hamlet|
      $#  GeoJSON file reading
      <div .pheading>
        Read GeoJSON from file
      <div .pnormal .pleftbutdiv>
        <input checked="" ##{dynamicstaticswitcher} type="checkbox">
        <label .label for="#{dynamicstaticswitcher}">
      <div .pleftbutdiv>
        <button .button onclick="document.getElementById('#{jsonfileinput}').click()" style="display: inline;" type="button">
          Browse
        <div .pnormal #filenameIndicator style="display: inline;">
        <input ##{jsonfileinput} onchange="displayUploadedFileName()" style="display:none" type="file">
      $#  GeoJSON clear button
      <div .pleftbutdiv>
        <button .button #cleargeombutton onclick="clearUploadedFileName()" style="display: inline;" type="button">
          Clear
    |]
