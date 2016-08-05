-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home.PopupSave
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Home.PopupSave
  ( popupSave
  ) where

import Import

-- | Greetings and help popup;
--   exposes dom id '#popupsave', '#submitform' and some others.
popupSave :: Widget
popupSave =
  toWidgetBody
    [hamlet|
      <div .popupdiv #popupsave style="display: none;">
        <div #previewcontainer>
        <form #submitform method="post">
          <input #sfSessionId name="sessionID" type="hidden">
          <input #sfGeometry name="geometry" type="hidden">
          <input #sfPreview name="preview" type="hidden">
          <center>
            <button .button onclick="hidePopups()" type="button">
              Cancel
            <button .button type="submit">
              Submit
    |]

