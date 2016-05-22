-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Types.Prim
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module JsHs.Types.Prim
    ( JSVal (..)
    , jsNull, jsIsNullOrUndef
    ) where


import GHCJS.Prim (JSVal (..), jsNull)



foreign import javascript unsafe "$1 == null" jsIsNullOrUndef :: JSVal -> Bool
