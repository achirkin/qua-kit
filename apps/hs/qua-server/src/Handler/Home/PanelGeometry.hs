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
  jsonfileinput <- newIdent
  fileNameIndicator <- newIdent
  clearGeometry <- newIdent
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
      <div>
        Read GeoJSON from file
      <div>
        <a.btn.btn-red.waves-attach.waves-light.waves-effect ##{clearGeometry} onclick="clearUploadedFileName()">
          clear
        <a.btn.btn-red.waves-attach.waves-light.waves-effect onclick="document.getElementById('#{jsonfileinput}').click()">
          files
        <div style="display:inline; font-size: 0.9em;" ##{fileNameIndicator}>
        <input ##{jsonfileinput} onchange="displayUploadedFileName()" style="display:none" type="file">
    |]

luciScenarios :: Handler (Widget, Widget)
luciScenarios = do
  popupScenarioListId <- newIdent
  popupScenarioListTable <- newIdent
  popupScenarioSaveId <- newIdent
  scenarioNameField <- newIdent
  let popupScenario = do
        toWidgetHead
          [julius|
              "use strict"
              var saveLuciScenario = function(scname){console.log("save scenario: " + scname);$('##{rawJS popupScenarioSaveId}').modal('hide');};
              /** Registers one callback; comes from Handler.Home.PanelGeometry.
               *  sendMsg :: JSString -> IO ()
               *  return :: IO ()
               */
              function registerSaveScenario(sendMsg) {
                saveLuciScenario = function(scname){sendMsg(scname.trim());$('##{rawJS popupScenarioSaveId}').modal('hide');};
              }
          |]
        toWidgetBody
              [hamlet|
                <div style="display: none;" aria-hidden="true" class="modal modal-va-middle fade" ##{popupScenarioListId} role="dialog" tabindex="-1" style="max-height: 100%">
                  <div class="modal-dialog" style="max-height: 100%">
                    <div class="modal-content" style="max-height: 100%">
                      <div class="modal-heading" style="max-height: 10%">
                        <p class="modal-title">
                          Select scenario
                      <div class="modal-inner" style="max-height: 80%">
                        <div ##{popupScenarioListTable} style="display: table; width: 100%; max-height: 80%; overflow-y: scroll">
                      <div class="modal-footer" style="max-height: 10%">
                        <p class="text-right">
                          <a class="btn btn-flat btn-brand-accent waves-attach waves-effect" data-dismiss="modal">
                            Cancel
                <div style="display: none;" aria-hidden="true" class="modal modal-va-middle fade" ##{popupScenarioSaveId} role="dialog" tabindex="-1">
                  <div class="modal-dialog">
                    <div class="modal-content">
                      <div class="modal-heading">
                        <p class="modal-title">
                          Enter a name for a new scenario to save it on a server
                      <div class="modal-inner">
                        <div class="form-group form-group-label">
                          <label class="floating-label" for="#{scenarioNameField}">Scenario name
                          <input class="form-control" ##{scenarioNameField} type="text">
                      <div class="modal-footer">
                        <p class="text-right">
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal">
                            Cancel
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal" onclick="saveLuciScenario($('##{scenarioNameField}').val());">
                            Save
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
              var askLuciForScenario = function(id,scname){$('##{rawJS popupScenarioListId}').modal('hide');};
              /** Registers one callback; comes from Handler.Home.PanelGeometry.
               *  h :: ScID -> JSString -> IO ()
               *  return :: IO ()
               */
              function registerAskLuciForScenario(sendMsg) {
                askLuciForScenario = function(id,scname){$('##{rawJS popupScenarioListId}').modal('hide');sendMsg(id,scname);};
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
                $('##{rawJS popupScenarioListId}').modal('show');
                document.getElementById('#{rawJS popupScenarioListTable}').innerHTML = xs.reduce(function(text, scDesc, i){
                    var classnames = i % 2 == 0 ? "#{rawJS scenariosRow}" : "#{rawJS scenariosRowOdd} #{rawJS scenariosRow}";
                    var nn = scDesc.name.replace(/"|'/g, "").trim();
                    return  "<div class=\"" + classnames + "\" onclick='askLuciForScenario(" + scDesc.ScID + ",\"" + nn + "\")'>"
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
              /** call it to setup scenario buttons state; comes from Handler.Home.PanelGeometry.
               *  showButton :: Bool -- whether to show "save scenario" button
               *  scName :: JSString -- name of the scenario displayed on a panel
               *  return :: IO ()
               */
              function toggleSaveScenarioButton(showButton, scName) {
                var scn = scName ? scName : "";
                document.getElementById('#{rawJS saveScenario}').style.display = showButton ? 'inline' : 'none';
                document.getElementById('#{rawJS fileNameIndicator}').innerText = scn;
              }
          |]
        toWidgetBody
          [hamlet|
            <div>
              Remote (Luci) scenarios
            <div>
              <a.btn.btn-red.waves-attach.waves-light.waves-effect ##{browseScenarios}">
                Scenarios
              <a.btn.btn-red.waves-attach.waves-light.waves-effect ##{saveScenario}" style="display: none;" onclick="$('##{scenarioNameField}').val('');$('##{popupScenarioSaveId}').modal('show');">
                Save
              <div style="display:inline; font-size: 0.9em;" ##{fileNameIndicator}>
          |]
  return (popupScenario, luciScenariosPane)


