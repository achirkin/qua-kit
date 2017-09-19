module Handler.Viewer
    ( getViewerR
    ) where

import Import

getViewerR :: Handler Html
getViewerR = do
  minimalLayout $ do
    setTitle "qua-kit"

    toWidgetHead
      [hamlet|
        <link href=@{StaticR css_qua_view_css} rel="stylesheet" type="text/css">
      |]

    [whamlet|
      <!-- A special element for showing loading splash very early -->
      <div .qua-view-loading-busy #qua-view-loading-div>
        <div #qua-view-loading-background>
        <svg #qua-view-loading-splash fill="none" version="1.1" viewBox="0 0 72 72"
            xmlns="http://www.w3.org/2000/svg">
          <circle #qua-view-loading-splash-red-circle cx="36" cy="36" r="28"
            stroke="#FF5722" stroke-dasharray="10, 5, 50, 40, 30.929188601, 40"
            stroke-opacity="1" stroke-width="16">
          <circle #qua-view-loading-splash-grey-circle cx="36" cy="36" r="28"
            stroke="#BF360C" stroke-dasharray="38, 14, 8, 14, 65.929188601, 14, 8, 14"
            stroke-opacity=".2" stroke-width="8">

      <!-- WebGL drawing canvas -->
      <canvas #qua-view-webgl-canvas>

      <!-- All dynamic widgets from qua-view.js go inside here -->
      <div #qua-view-widgets>

      <!-- All dynamic modals from qua-view.js go inside here -->
      <div #qua-view-modals>

      <!-- Qua-view generated code -->
      <script src=@{StaticR js_qua_view_js} async type="text/javascript">
    |]
