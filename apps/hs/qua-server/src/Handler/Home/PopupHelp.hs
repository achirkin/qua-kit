-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.PopupHelp
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Home.PopupHelp
  ( popupHelp
  ) where

import Import

-- | Greetings and help popup;
--   exposes dom id '#popuphelp' and some others.
popupHelp :: Widget
popupHelp = do
  helpIcons <- newIdent
  toWidgetHead
    [cassius|
      .#{helpIcons}
          display: inline
          height: 32px
          padding: 0
          margin: 0
          vertical-align: middle
    |]
  toWidgetBody
    [hamlet|
      <div .popupdiv #popuphelp>
        <center .pheading #greetings>
          Welcome to the web-based geometry modeler
        <hr>
        <table>
          <tr>
            <td>
            <td>
              <div .pheading>
                You can use mouse to move camera and buildings:
          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #mouseLeftIcon timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <path d="m 60,66 0,-8.233239 c -3.690856,-1.401015 -6.258857,-4.504114 -6.258857,-8.109972 l 0,-15.313352 c 0,-3.605859 2.567755,-6.708958 6.258857,-8.109972 L 60,10 55.836735,10 C 38.253796,10 24,22.692958 24,38.350648 L 24,66 60,66 Z" #leftButton style="fill:#FF5722">
                <path d="M 71,47.54717 C 71,51.110943 67.865965,54 64.000001,54 l 0,0 C 60.134035,54 57,51.110943 57,47.54717 L 57,36.45283 C 57,32.889056 60.134035,30 64.000001,30 l 0,0 C 67.865965,30 71,32.889056 71,36.45283 l 0,11.09434 z" #middleButton style="fill:#020202">
                <path d="m 68,26.233239 c 3.690857,1.401015 6.258857,4.504113 6.258857,8.109973 l 0,15.313351 c 0,3.605859 -2.567755,6.708958 -6.258857,8.109972 L 68,66 l 36,0 0,-27.649352 C 104,22.692958 89.746203,10 72.163265,10 L 68,10 68,26.233239 Z" #rightButton style="fill:#020202">
                <path d="m 24,69 0,21.536878 C 24,106.25679 38.109818,119 55.515151,119 l 16.969697,0 C 89.890181,119 104,106.25679 104,90.536878 L 104,69 24,69 Z" #mouseBody style="fill:#020202">
            <td>
              Click on a building using left mouse button to select it, or click on empty space to unselect a building.

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #mouseLeftIcon timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <path d="m 60,66 0,-8.233239 c -3.690856,-1.401015 -6.258857,-4.504114 -6.258857,-8.109972 l 0,-15.313352 c 0,-3.605859 2.567755,-6.708958 6.258857,-8.109972 L 60,10 55.836735,10 C 38.253796,10 24,22.692958 24,38.350648 L 24,66 60,66 Z" #leftButton style="fill:#FF5722">
                <path d="M 71,47.54717 C 71,51.110943 67.865965,54 64.000001,54 l 0,0 C 60.134035,54 57,51.110943 57,47.54717 L 57,36.45283 C 57,32.889056 60.134035,30 64.000001,30 l 0,0 C 67.865965,30 71,32.889056 71,36.45283 l 0,11.09434 z" #middleButton style="fill:#020202">
                <path d="m 68,26.233239 c 3.690857,1.401015 6.258857,4.504113 6.258857,8.109973 l 0,15.313351 c 0,3.605859 -2.567755,6.708958 -6.258857,8.109972 L 68,66 l 36,0 0,-27.649352 C 104,22.692958 89.746203,10 72.163265,10 L 68,10 68,26.233239 Z" #rightButton style="fill:#020202">
                <path d="m 24,69 0,21.536878 C 24,106.25679 38.109818,119 55.515151,119 l 16.969697,0 C 89.890181,119 104,106.25679 104,90.536878 L 104,69 24,69 Z" #mouseBody style="fill:#020202">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Press left mouse button to move camera or selected building horizontally

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #mouseLeftIcon timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <path d="m 60,66 0,-8.233239 c -3.690856,-1.401015 -6.258857,-4.504114 -6.258857,-8.109972 l 0,-15.313352 c 0,-3.605859 2.567755,-6.708958 6.258857,-8.109972 L 60,10 55.836735,10 C 38.253796,10 24,22.692958 24,38.350648 L 24,66 60,66 Z" #leftButton style="fill:#020202">
                <path d="M 71,47.54717 C 71,51.110943 67.865965,54 64.000001,54 l 0,0 C 60.134035,54 57,51.110943 57,47.54717 L 57,36.45283 C 57,32.889056 60.134035,30 64.000001,30 l 0,0 C 67.865965,30 71,32.889056 71,36.45283 l 0,11.09434 z" #middleButton style="fill:#FF5722">
                <path d="m 68,26.233239 c 3.690857,1.401015 6.258857,4.504113 6.258857,8.109973 l 0,15.313351 c 0,3.605859 -2.567755,6.708958 -6.258857,8.109972 L 68,66 l 36,0 0,-27.649352 C 104,22.692958 89.746203,10 72.163265,10 L 68,10 68,26.233239 Z" #rightButton style="fill:#020202">
                <path d="m 24,69 0,21.536878 C 24,106.25679 38.109818,119 55.515151,119 l 16.969697,0 C 89.890181,119 104,106.25679 104,90.536878 L 104,69 24,69 Z" #mouseBody style="fill:#020202">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Press middle mouse button to move camera vertically

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #mouseLeftIcon timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <path d="m 60,66 0,-8.233239 c -3.690856,-1.401015 -6.258857,-4.504114 -6.258857,-8.109972 l 0,-15.313352 c 0,-3.605859 2.567755,-6.708958 6.258857,-8.109972 L 60,10 55.836735,10 C 38.253796,10 24,22.692958 24,38.350648 L 24,66 60,66 Z" #leftButton style="fill:#020202">
                <path d="M 71,47.54717 C 71,51.110943 67.865965,54 64.000001,54 l 0,0 C 60.134035,54 57,51.110943 57,47.54717 L 57,36.45283 C 57,32.889056 60.134035,30 64.000001,30 l 0,0 C 67.865965,30 71,32.889056 71,36.45283 l 0,11.09434 z" #middleButton style="fill:#020202">
                <path d="m 68,26.233239 c 3.690857,1.401015 6.258857,4.504113 6.258857,8.109973 l 0,15.313351 c 0,3.605859 -2.567755,6.708958 -6.258857,8.109972 L 68,66 l 36,0 0,-27.649352 C 104,22.692958 89.746203,10 72.163265,10 L 68,10 68,26.233239 Z" #rightButton style="fill:#FF5722">
                <path d="m 24,69 0,21.536878 C 24,106.25679 38.109818,119 55.515151,119 l 16.969697,0 C 89.890181,119 104,106.25679 104,90.536878 L 104,69 24,69 Z" #mouseBody style="fill:#020202">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Press right mouse button to rotate camera or selected building
          <tr>
            <td>
            <td>
              <div .pheading>
                If your device has a multitouch screen, you can use touch controls:
          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #singleTouch timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <circle cx="64" cy="64" fill="none" #outerCircle r="40" stroke="#FF5722" stroke-width="8">
                <circle cx="64" cy="64" fill="#999999" #innerCircle r="30">
            <td>
              Touch a building using left mouse button to select it, or touch on empty space to unselect a building.

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #singleDrag timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <circle cx="64" cy="64" fill="none" #outerCircle r="40" stroke="#FF5722" stroke-width="8">
                <circle cx="64" cy="64" fill="#999999" #innerCircle r="30">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Move camera or selected building horizontally with one finger

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #twinDrag timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <circle cx="32" cy="75" fill="none" #outerCircle r="29" stroke="#FF5722" stroke-width="6">
                <circle cx="32" cy="75" fill="#999999" #innerCircle r="22">
                <circle cx="91" cy="40" fill="none" #outerCircle r="29" stroke="#FF5722" stroke-width="6">
                <circle cx="91" cy="40" fill="#999999" #innerCircle r="22">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Use two fingers to move, scale, and rotate naturally

          <tr>
            <td>
              <svg .#{helpIcons} fill="none" #twinDrag timelinebegin="loadbegin" version="1.1" viewBox="0 0 128 128" xmlns="http://www.w3.org/2000/svg">
                <circle cx="29" cy="60" fill="none" #outerCircle r="26" stroke="#FF5722" stroke-width="6">
                <circle cx="29" cy="60" fill="#999999" #innerCircle r="20">
                <circle cx="91" cy="29" fill="none" #outerCircle r="26" stroke="#FF5722" stroke-width="6">
                <circle cx="91" cy="29" fill="#999999" #innerCircle r="20">
                <circle cx="80" cy="96" fill="none" #outerCircle r="26" stroke="#FF5722" stroke-width="6">
                <circle cx="80" cy="96" fill="#999999" #innerCircle r="20">
                <path d="m 32,0 -12,13.886719 4.697266,0 0,10.810547 -10.810547,0 0,-4.697266 L 0,32 l 13.886719,12 0,-4.697266 10.810547,0 0,10.808594 -4.697266,0 L 32,64 44,50.111328 l -4.697266,0 0,-10.808594 10.808594,0 0,4.697266 L 64,32 50.111328,20 l 0,4.697266 -10.808594,0 0,-10.810547 4.697266,0 L 32,0 Z" #dragArrows style="fill:#FF5722;opacity:0.5" transform="translate(64,64)">
            <td>
              Use three fingers to rotate camera in all directions

        <hr>
        <div style="text-align: justify;">
          <b>
            Note:
          for research purposes some of your actions (e.g. moving and rotating geometry objects) on this page may be anonymously recorded and sent to our servers; by proceeding you agree to share these data.

        <center>
          <button .button onclick="hidePopups()" type="button">
            Ok, let's go!
    |]
