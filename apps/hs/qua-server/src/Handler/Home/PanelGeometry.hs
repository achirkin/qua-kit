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
  ( fileUploadGeometry, luciScenarios
  ) where

import Import
import Text.Julius


fileUploadGeometry :: Widget
fileUploadGeometry = do
  dynamicstaticswitcher <- newIdent
  jsonfileinput <- newIdent
  fileNameIndicator <- newIdent
  clearGeometry <- newIdent
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
          jsonFileInputButton.addEventListener("change", function() {
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
          });
        }
        /** Registers one callback; comes from Handler.Home.PanelGeometry.
         *  onClick :: IO ()
         *  return :: IO ()
         */
        function registerClearGeometry(onClick) {
          document.getElementById('#{rawJS clearGeometry}').addEventListener("click", onClick);
        }
        function displayUploadedFileName() {
          var jsonFileInputButton = document.getElementById('#{rawJS jsonfileinput}');
          document.getElementById('#{rawJS fileNameIndicator}').innerText = jsonFileInputButton.value.split(/(\\|\/)/g).pop();
        }
        function clearUploadedFileName() {
          var jsonFileInputButton = document.getElementById('#{rawJS jsonfileinput}');
          document.getElementById('#{rawJS fileNameIndicator}').innerText = '';
          jsonFileInputButton.value = '';
        }
    |]
  toWidgetBody
    [hamlet|
      $#  GeoJSON file reading
      <div .pheading>
        Read GeoJSON from file
      <div style="display: table; width: 100%; z-index: 5;">
        <div style="display: table-cell; width: 80px; margin: 0; padding 0; vertical-align: middle;">
          <button .button ##{clearGeometry} onclick="clearUploadedFileName()" type="button" style="width: 100%; margin: 8px 0px 0px 0px; padding: 4px 12px 2px 12px;">
            clear
        <div style="display: table-cell; width: 80px; margin: 0; padding 0; vertical-align: middle;">
          <button .button onclick="document.getElementById('#{jsonfileinput}').click()" type="button" style="width: 100%; margin: 8px 0px 0px 0px; padding: 4px 12px 2px 12px;">
            files
        <div style="display: table-cell; margin: 0; padding 0; vertical-align: middle;">
          <div .pnormal ##{fileNameIndicator}>
          <input ##{jsonfileinput} onchange="displayUploadedFileName()" style="display:none" type="file">
      $# <!-- <div> -->
      $# <!--   <input checked="" ##{dynamicstaticswitcher} type="checkbox"> -->
      $# <!--   <label .label for="#{dynamicstaticswitcher}"> -->
    |]

luciScenarios :: Handler (Widget, Widget)
luciScenarios = do
  popupScenarioListId <- newIdent
  popupScenarioListTable <- newIdent
  popupScenarioSaveId <- newIdent
  popupScenarioSave <- newIdent
  scenarioNameField <- newIdent
  let popupScenario = do
        toWidgetHead
          [julius|
              "use strict"
              var saveLuciScenario = function(scname){console.log("save scenario: " + scname); hidePopups();};
              /** Registers one callback; comes from Handler.Home.PanelGeometry.
               *  sendMsg :: JSString -> IO ()
               *  return :: IO ()
               */
              function registerSaveScenario(sendMsg) {
                saveLuciScenario = function(scname){sendMsg(scname);hidePopups();};
              }
          |]
        toWidgetBody
              [hamlet|
                <div .popupdiv ##{popupScenarioListId} style="display: none;">
                  <div ##{popupScenarioListTable} style="display: table; width: 100%;">
                <div .popupdiv ##{popupScenarioSaveId} style="display: none;">
                  <div ##{popupScenarioSave} style="display: table; width: 100%;">
                    Enter a name for a new scenario to save it on Luci server
                    <center>
                      <div .group style="">
                        <input .pinput ##{scenarioNameField} onblur="checkIfUsed('#{scenarioNameField}')" type="text">
                        <span .phighlight>
                        <span .pbar>
                        <label .plabel>
                          Scenario name
                    <center>
                      <button .button onclick="saveLuciScenario(document.getElementById('#{scenarioNameField}').value);" type="button">
                        Save!
              |]
      luciScenariosPane = do
        browseScenarios <- newIdent
        saveScenario <- newIdent
        scenariosRow <- newIdent
        scenariosRowOdd <- newIdent
        scenariosCell <- newIdent
        fileNameIndicator <- newIdent
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
        toWidgetHead
          [julius|
              "use strict"
              var askLuciForScenario = function(id,scname){hidePopups();};
              /** Registers one callback; comes from Handler.Home.PanelGeometry.
               *  h :: ScID -> JSString -> IO ()
               *  return :: IO ()
               */
              function registerAskLuciForScenario(sendMsg) {
                askLuciForScenario = function(id,scname){sendMsg(id,scname);hidePopups();};
              }
              /** Registers one callback; comes from Handler.Home.PanelGeometry.
               *  onClick :: IO ()
               *  return :: IO ()
               */
              function registerGetScenarioList(onClick) {
                document.getElementById('#{rawJS browseScenarios}').addEventListener("click", onClick);
              }
               /** Call this when scenarios are parsed; comes from Handler.Home.PanelGeometry.
                *  xs :: [{ScenarioDescription, as-is}]
                *  return :: IO ()
                */
              function displayScenarios(xs) {
                showPopup('#{rawJS popupScenarioListId}');
                document.getElementById('#{rawJS popupScenarioListTable}').innerHTML = xs.reduce(function(text, scDesc, i){
                    var classnames = i % 2 == 0 ? "#{rawJS scenariosRow}" : "#{rawJS scenariosRowOdd} #{rawJS scenariosRow}"
                    return  "<div class=\"" + classnames + "\" onclick=\"askLuciForScenario(" + scDesc.ScID + ",'" + scDesc.name + "')\" >"
                             + "<div class=\"#{rawJS scenariosCell}\">" + scDesc.name + "</div>"
                             + "<div class=\"#{rawJS scenariosCell}\">" + formatDate(scDesc.created) + "</div>"
                             + "<div class=\"#{rawJS scenariosCell}\">" + formatDate(scDesc.lastmodified)  + "</div>"
                             + "</div>\n" + text;
                  }, "");
              }
              function formatDate(t) {
                var d = new Date(t*1000);
                return d.getFullYear() + "." + ((d.getMonth() + 101) + ".").substr(1) + ((d.getDate()+100) + " ").substr(1)
                       + d.getHours() + ":" + d.getMinutes() + ":" + d.getSeconds();
              }
              /** call it to setup scenario buttons state; comes from Handler.Home.PanelGeometry.
               *  showButton :: Bool -- whether to show "save scenario" button
               *  scName :: JSString -- name of the scenario displayed on a panel
               *  return :: IO ()
               */
              function toggleSaveScenarioButton(showButton, scName) {
                var scn = scName ? scName : "";
                document.getElementById('#{rawJS saveScenario}').style.display = showButton ? 'table-cell' : 'none';
                document.getElementById('#{rawJS fileNameIndicator}').innerText = scn;
              }
          |]
        toWidgetBody
          [hamlet|
            <div .pheading>
              Remote (Luci) scenarios
            <div style="display: table; width: 100%; 0px; z-index: 5;">
              <div style="display: table-cell; width: 97px; margin: 0; padding 0; vertical-align: middle;">
                <button .button ##{browseScenarios} type="button" style="width: 100%; max-width: 97px; margin: 8px 0px 0px 0px; padding: 4px 12px 2px 12px;">
                  scenarios
              <div ##{saveScenario} style="display: none; width: 63px; margin: 0; padding 0; vertical-align: middle;">
                <button .button type="button" onclick="document.getElementById('#{scenarioNameField}').value = '';showPopup('#{popupScenarioSaveId}');" style="width: 100%; max-width: 63px; margin: 8px 0px 0px 0px; padding: 4px 12px 2px 12px;">
                  save
              <div style="display: table-cell; margin: 0; padding 0; vertical-align: middle;">
                <div .pnormal ##{fileNameIndicator}>

            $# <!-- <div> -->
            $# <!--   <input checked="" ##{dynamicstaticswitcher} type="checkbox"> -->
            $# <!--   <label .label for="#{dynamicstaticswitcher}"> -->
          |]
  return (popupScenario, luciScenariosPane)


