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
--import Text.Julius

panelServices :: Widget
panelServices = do
  toWidgetHead
    [cassius|
      .spKey
        padding: 2px
        text-align: right
      .spVal
        padding: 2px
    |]
  toWidgetHead
    [julius|
    /** Registers one callback; comes from Handler.Home.PanelServices.
     *  onClick :: IO ()
     *  return :: IO ()
     */
    function registerRefreshServiceList(onClick) {
      document.getElementById('refreshServicesBtn').addEventListener("click", onClick);
    }
    /** Updates visible service list; comes from Handler.Home.PanelServices.
     *  xs :: [ServiceName]
     *  return :: IO ()
     */
    function updateServiceNames(xs) {
      var e = document.getElementById("serviceListControlMenu");
      while (e.length > 0) {
        e.remove(e[0]);
      }
      var option, i;
      for (i = 0; i < xs.length; i++) {
        option = document.createElement("option");
        option.text = xs[i];
        option.value = xs[i];
        e.add(option);
        if (i == 0) {
          option.selected = true;
          activeVisService(xs[i]);
        } else {
          option.selected = false;
        }
      }
    }
    var activeVisService = function(s){console.log("Set active service: ", s)};
    /** Registers one callback; comes from Handler.Home.PanelServices.
     *  setActiveService :: String -> IO ()
     *  return :: IO ()
     */
    function registerSetActiveService(setActiveService) {
      activeVisService = setActiveService;
    }
    var updateValSCB = function(s, v){console.log("Update parameter: ", s, v)};
    /** Registers one callback; comes from Handler.Home.PanelServices.
     *  updateParam :: String -> JSVal -> IO ()
     *  return :: IO ()
     */
    function registerUpdateSParamValue(updateParam) {
      updateValSCB = updateParam;
    }
    |]
  toWidgetBody
    [hamlet|
      <div>
        Select a remote service to run
          <table style="width: 98%">
            <tr>
              <td style="width: 5%">
                <a.btn.btn-flat.btn-red.waves-attach title="Refresh list of available services" #refreshServicesBtn>
                  <span.icon.icon-lg>refresh
              <td style="width: 95%" onchange="activeVisService($(this).find(':selected').val())">
                <select.form-control #serviceListControlMenu>

      <div.form-group #guiServiceParams>

    |]

--        <div.checkbox.switch>
--          <label for="booleanPamId01">
--            <input.access-hide id="booleanPamId01" name="booleanPamName01" type="checkbox">
--            <span.switch-toggle>
--            Boolean parameter 1
--
--        <div.checkbox.switch>
--          <label for="booleanPamId02">
--            <input.access-hide id="booleanPamId02" name="booleanPamName02" type="checkbox" checked="">
--            <span.switch-toggle>
--            Boolean parameter 2
--
--        <div style="margin: 2px;">
--          <label for="integerPamId01">
--            <input id="integerPamId01" name="integerPamName01" type="number" value="42" min="10" max="70" style="width: 6em;">
--            [10..70] (m<sup>2</sup>) Integer parameter 1 - constrained
--
--        <div style="margin: 2px;">
--          <label for="floatPamId01">
--            <input id="floatPamId01" name="floatPamName01" type="number" value="-10.5" style="width: 6em;">
--            (m<sup>3</sup>) Float parameter 1
--
--        <div style="margin: 2px;">
--          <label for="floatPamId02">
--            <input id="floatPamId02" name="floatPamName02" type="number" value="0.3" min="0" style="width: 6em;">
--            [0..] (kg/(m c)) Float parameter 2 - lower bound only

