-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive.Banana.JsHs
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Reactive.Banana.JsHs
    ( -- * Event handler for an html element
      module ElementHandler
      -- * Basic types
    , module Types
    ) where


import qualified Reactive.Banana.JsHs.ElementHandler as ElementHandler
import qualified Reactive.Banana.JsHs.Types as Types
