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
    |]
  toWidgetBody
    [hamlet|
      <div .pheading>
        Here you will see some services
    |]
