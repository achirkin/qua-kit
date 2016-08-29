-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.UIButtons
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Home.UIButtons
  ( uiButtons
  ) where

import Import
import Text.Julius

-- | Rotating circular progress bar.
--   Exposes:
--     * #helpbutton
--     * #fullscreenbutton
--     * #submitbutton
--     * #toolboxbutton
--     * #clearbutton
--     * #evaluatebutton
uiButtons :: Handler (Widget, Widget)
uiButtons = do

  popupSubmitId <- newIdent
  sfGeometry <- newIdent
  sfPreview <- newIdent
  submitForm <- newIdent
  previewcontainer <- newIdent

  textAreaDesc <- newIdent

  let popupSubmit = do
        toWidgetBody
              [hamlet|
                <div style="display: none;" aria-hidden="true" class="modal modal-va-middle fade" ##{popupSubmitId} role="dialog" tabindex="-1">
                  <div class="modal-dialog modal-xs">
                    <div class="modal-content">
                      <div class="modal-heading">
                        <p class="modal-title">
                          Submit your design
                      <div class="modal-inner">
                        <div ##{previewcontainer}>
                        <form ##{submitForm} method="post">
                          <input type="hidden" ##{sfGeometry} name="geometry">
                          <input type="hidden" ##{sfPreview} name="preview">
                          <div class="form-group form-group-label">
                            <label.floating-label for="#{textAreaDesc}">
                            <textarea.form-control.textarea-autosize form="#{submitForm}" id="#{textAreaDesc}" name="description">
                      <div class="modal-footer">
                        <p class="text-right">
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal">
                            Cancel
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect data-dismiss="modal" onclick="$('##{submitForm}').submit();">
                            Save
              |]
      uiButtonsG = do
        uiButton <- newIdent
        guiElement <- newIdent
        guiplaceholder <- newIdent
        idleplaceholder <- newIdent
        activeplaceholder <- newIdent
        btn <- newIdent

        serviceClear <- newIdent
        serviceRun <- newIdent
        submitbutton <- newIdent

        toWidgetHead
          [cassius|
            /* GUI icons */

            .#{guiElement}
                fill: #FF8A65 /* #FFAB91 */
                stroke: #FFFFFF
                stroke-width: 0.0

            .#{uiButton}:hover
                cursor: pointer

            .#{uiButton}:hover .#{guiElement}
                fill: #FF5722

            .#{uiButton}:active .#{guiElement}
                fill: #FF5722

            .#{uiButton}
                position: relative
                padding: 0
                margin: 0
                z-index: 10
                overflow: hidden
                width: 64px
                height: 64px

            rect.#{btn}
                stroke: #FFFFFF
                fill: #FFFFFF
                fill-opacity: 0
                stroke-opacity: 0

            ##{guiplaceholder}
                position: absolute
                bottom: 0
                padding: 0
                margin: 0
                z-index: 4
                overflow: visible
            /*    height: 256px */
                width: 64px
                -webkit-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -moz-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -o-transition: width 300ms ease-in-out, left 300ms ease-in-out
                transition: width 300ms ease-in-out, left 300ms ease-in-out

            .#{activeplaceholder}
                left: -32px

            .#{idleplaceholder}
                left: -64px
          |]
--            sfGeometry <- newIdent
--  sfPreview <- newIdent
--  submitButton <- newIdent
--  previewcontainer <- newIdent
        toWidgetHead
          [julius|
            "use strict"
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: (submitUrl -> FeatureCollection -> Image -> IO ()) -> IO ()
             *  return :: IO ()
             */
            function registerSubmit(onClick) {
              document.getElementById('#{rawJS submitbutton}').style.display = "block"
              document.getElementById('#{rawJS submitbutton}').addEventListener("click", function() {
                  onClick( function(submitUrl, fc, img) {
                    document.getElementById('#{rawJS sfGeometry}').value = JSON.stringify(fc);
                    document.getElementById('#{rawJS sfPreview}').value = img;
                    var eimg = document.createElement("img");
                    eimg.src = img;
                    eimg.style.width = "100%";
                    document.getElementById('#{rawJS previewcontainer}').innerHTML = "";
                    document.getElementById('#{rawJS previewcontainer}').appendChild(eimg);
                    document.getElementById('#{rawJS submitForm}').action = submitUrl;
                    $('##{rawJS popupSubmitId}').modal('show');
                  });
                });
            }
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: IO ()
             *  return :: IO ()
             */
            function registerServiceClear(onClick) {
              document.getElementById('#{rawJS serviceClear}').addEventListener("click", onClick);
            }
            /** Registers one callback; comes from Handler.Home.UIButtons.
             *  onClick :: IO ()
             *  return :: IO ()
             */
            function registerServiceRun(onClick) {
              document.getElementById('#{rawJS serviceRun}').style.display = "block"
              document.getElementById('#{rawJS serviceRun}').addEventListener("click", onClick);
            }
            /** Shows or hides button "clear"; comes from Handler.Home.UIButtons.
             *  state :: Bool
             *  return :: IO ()
             */
            function toggleServiceClear(state) {
              document.getElementById('#{rawJS serviceClear}').style.display = state ? "block" : "none";
              document.getElementById('#{rawJS serviceRun}').style.display = state ? "none" : "block";
            }
            // Toggle fullscreen and change the fullscreen button shape
            function toggleFullScreen() {
                if (!document['fullscreenElement'] && !document['mozFullScreenElement']
                 && !document['webkitFullscreenElement'] && !document['msFullscreenElement'] && !document['fullScreen']) {
                  if (document.documentElement['requestFullscreen']) {
                    document.documentElement.requestFullscreen();
                  } else if (document.documentElement['msRequestFullscreen']) {
                    document.documentElement.msRequestFullscreen();
                  } else if (document.documentElement['mozRequestFullScreen']) {
                    document.documentElement.mozRequestFullScreen();
                  } else if (document.documentElement['webkitRequestFullscreen']) {
                    document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
                  }
                } else {
                  if (document['exitFullscreen']) {
                    document.exitFullscreen();
                  } else if (document['msExitFullscreen']) {
                    document.msExitFullscreen();
                  } else if (document['mozCancelFullScreen']) {
                    document.mozCancelFullScreen();
                  } else if (document['webkitExitFullscreen']) {
                    document.webkitExitFullscreen();
                  } else {
                    document.cancelFullScreen();
                    document.exitFullscreen();
                  }
                }
            };

            // change fullscreen button
            function checkfullscreen() {
                if (document['fullscreenElement'] || document['webkitFullscreenElement'] || document['mozFullScreenElement']) {
                    document.getElementById('fullscreenbicon').innerText = "fullscreen_exit";
                } else {
                    document.getElementById('fullscreenbicon').innerText = "fullscreen";
                }
            };

            document.addEventListener('webkitfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('mozfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('msfullscreenchange', function(e) {checkfullscreen();}, false);
            document.addEventListener('fullscreenchange', function(e) {checkfullscreen();}, false);

            // Toggle GUI panel on the right side of window
            function toggleGUIPanel(){
                var panel = document.getElementById('guipanel');
                if (panel.className == 'idleguipanel') {
                    panel.className = 'activeguipanel';
                    document.getElementById('#{rawJS guiplaceholder}').className = '#{rawJS activeplaceholder}';
                } else {
                    panel.className = 'idleguipanel';
                    document.getElementById('#{rawJS guiplaceholder}').className = '#{rawJS idleplaceholder}';
                }
            };
          |]
        toWidgetBody
          [hamlet|
            <div .#{idleplaceholder} ##{guiplaceholder}>
              <div class="fbtn-inner open">
                <a aria-expanded="true" class="fbtn fbtn-lg fbtn-red waves-attach waves-circle waves-light waves-effect" data-toggle="dropdown">
                  <span class="fbtn-text fbtn-text-left">Tools
                  <span class="fbtn-ori icon">apps
                  <span class="fbtn-sub icon">close
                <div class="fbtn-dropup">
                  <a class="fbtn waves-attach waves-circle waves-effect" #helpbutton onclick="$('#popuphelp').modal('show')">
                    <span class="fbtn-text fbtn-text-left">Help
                    <span class="icon icon-lg">help_outline
                  <a class="fbtn waves-attach waves-circle waves-effect" #fullscreenbutton onclick="toggleFullScreen()">
                    <span class="fbtn-text fbtn-text-left">Toggle fullscreen
                    <span class="icon icon-lg" #fullscreenbicon>fullscreen
                  <a class="fbtn waves-attach waves-circle waves-effect" #toolboxbutton onclick="toggleGUIPanel()">
                    <span class="fbtn-text fbtn-text-left">Control panel
                    <span class="icon icon-lg">settings
                  <a class="fbtn fbtn-brand waves-attach waves-circle waves-effect" style="display: none;" ##{submitbutton}>
                    <span class="fbtn-text fbtn-text-left">Submit proposal
                    <span class="icon icon-lg">save
                  <a class="fbtn fbtn-brand-accent waves-attach waves-circle waves-light waves-effect" style="display: none;" ##{serviceClear}>
                    <span class="fbtn-text fbtn-text-left">Clear service results
                    <span class="icon icon-lg">visibility_off
                  <a class="fbtn fbtn-green waves-attach waves-circle waves-effect" style="display: none;" ##{serviceRun}>
                    <span class="fbtn-text fbtn-text-left">Run evaluation service
                    <span class="icon icon-lg">play_arrow
          |]
  return (uiButtonsG, popupSubmit)
