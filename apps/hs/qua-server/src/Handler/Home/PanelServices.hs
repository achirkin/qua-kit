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
    |]
  toWidgetBody
    [hamlet|
      <div>
        Select a remote service to run
          <table style="width: 100%">
            <tr>
              <td style="width: 5%">
                <a.btn.btn-flat.btn-red.waves-attach title="Refresh list of available services" #refreshServicesBtn>
                  <span.icon.icon-lg>refresh
              <td style="width: 95%">
                <select.form-control>
                  <option value="iso">Isovist
                  <option value="sol">Solar analysis
                  <option value="dis">Distance to walls

      <div.form-group>

        <div.checkbox.switch>
          <label for="booleanPamId01">
            <input.access-hide id="booleanPamId01" name="booleanPamName01" type="checkbox">
            <span.switch-toggle>
            Boolean parameter 1

        <div.checkbox.switch>
          <label for="booleanPamId02">
            <input.access-hide id="booleanPamId02" name="booleanPamName02" type="checkbox" checked="">
            <span.switch-toggle>
            Boolean parameter 2

        <div style="margin: 2px;">
          <label for="integerPamId01">
            <input id="integerPamId01" name="integerPamName01" type="number" checked="true" value="42" min="10" max="70" style="width: 6em;">
            [10..70] (m<sup>2</sup>) Integer parameter 1 - constrained

        <div style="margin: 2px;">
          <label for="floatPamId01">
            <input id="floatPamId01" name="floatPamName01" type="number" checked="true" value="-10.5" style="width: 6em;">
            (m<sup>3</sup>) Float parameter 1

        <div style="margin: 2px;">
          <label for="floatPamId02">
            <input id="floatPamId02" name="floatPamName02" type="number" checked="true" value="0.3" min="0" style="width: 6em;">
            [0..] (kg/(m c)) Float parameter 2 - lower bound only
    |]
