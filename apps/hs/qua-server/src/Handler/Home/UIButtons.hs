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
uiButtons :: Widget
uiButtons = do
  uiButton <- newIdent
  guiElement <- newIdent
  guiplaceholder <- newIdent
  idleplaceholder <- newIdent
  activeplaceholder <- newIdent
  btn <- newIdent
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
  toWidgetHead
    [julius|
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
              document.getElementById('fullscreenbshape').setAttribute('d','M14,14H19V16H16V19H14V14M5,14H10V19H8V16H5V14M8,5H10V10H5V8H8V5M19,8V10H14V5H16V8H19Z');
          } else {
              document.getElementById('fullscreenbshape').setAttribute('d','M5,5H10V7H7V10H5V5M14,5H19V10H17V7H14V5M17,14H19V19H14V17H17V14M10,17V19H5V14H7V17H10Z');
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
      $#  help popup button
      <svg .#{uiButton} height="64" style="left:0px;" version="1.1" viewBox="0 0 64 64" width="64" xmlns="http://www.w3.org/2000/svg">
        <text .#{guiElement} style="font-size: 60px; font-weight: bold;" x="16" y="60">
          ?
        <rect .#{btn} height="64" #helpbutton onclick="showPopup('popuphelp')" width="64" x="0" y="0">
      $#  fullscreen button
      <svg .#{uiButton} height="64" style="left:0px;" version="1.1" viewBox="2 2 20 20" width="64" xmlns="http://www.w3.org/2000/svg">
        <path .#{guiElement} d="M5,5H10V7H7V10H5V5M14,5H19V10H17V7H14V5M17,14H19V19H14V17H17V14M10,17V19H5V14H7V17H10Z" #fullscreenbshape>
        <rect .#{btn} height="24" #fullscreenbutton onclick="toggleFullScreen()" width="24" x="0" y="0">
      $#  submit button
      <svg .#{uiButton} height="64" style="left:0px;" version="1.1" viewBox="0 0 24 24" width="64" xmlns="http://www.w3.org/2000/svg">
        <path .#{guiElement} d="M15,9H5V5H15M12,19A3,3 0 0,1 9,16A3,3 0 0,1 12,13A3,3 0 0,1 15,16A3,3 0 0,1 12,19M17,3H5C3.89,3 3,3.9 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V7L17,3Z" #submitshape>
        <rect .#{btn} height="24" #submitbutton width="24" x="0" y="0">
      $#  toolbox button
      <svg .#{uiButton} height="64" style="left:0px;" version="1.1" viewBox="0 0 24 24" width="64" xmlns="http://www.w3.org/2000/svg">
        <path .#{guiElement} d="M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.34 19.43,11L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.21,8.95 2.27,9.22 2.46,9.37L4.57,11C4.53,11.34 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.21,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.67 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z" #toolboxshape>
        <rect .#{btn} height="24" #toolboxbutton onclick="toggleGUIPanel()" width="24" x="0" y="0">
      $#  clear button
      <svg .#{uiButton} height="64" style="position: absolute; left:-64px; display: none;" version="1.1" viewBox="0 0 24 24" width="64" xmlns="http://www.w3.org/2000/svg">
        <path .#{guiElement} d="M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,2C6.47,2 2,6.47 2,12C2,17.53 6.47,22 12,22C17.53,22 22,17.53 22,12C22,6.47 17.53,2 12,2M14.59,8L12,10.59L9.41,8L8,9.41L10.59,12L8,14.59L9.41,16L12,13.41L14.59,16L16,14.59L13.41,12L16,9.41L14.59,8Z" #clearshape>
        <rect .#{btn} height="24" #clearbutton width="24" x="0" y="0">
      $#  evaluate button
      <svg .#{uiButton} height="64" style="left:0px;" version="1.1" viewBox="0 0 24 24" width="64" xmlns="http://www.w3.org/2000/svg">
        <path .#{guiElement} d="M12,20.14C7.59,20.14 4,16.55 4,12.14C4,7.73 7.59,4.14 12,4.14C16.41,4.14 20,7.73 20,12.14C20,16.55 16.41,20.14 12,20.14M12,2.14A10,10 0 0,0 2,12.14A10,10 0 0,0 12,22.14A10,10 0 0,0 22,12.14C22,6.61 17.5,2.14 12,2.14M10,16.64L16,12.14L10,7.64V16.64Z" #evaluateshape>
        <rect .#{btn} height="24" #evaluatebutton width="24" x="0" y="0">
    |]
