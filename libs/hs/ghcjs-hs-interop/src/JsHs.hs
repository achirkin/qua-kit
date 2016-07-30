-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module JsHs
    ( -- * LikeJS
      -- | LikeJS is a unified way to transform objects between Haskell and JavaScript representation.
      LikeJS.LikeJS (..)
      -- * Common types
    , Array.Array, Array.emptyArray
    , module Import
    ) where


import qualified JsHs.LikeJS.Class as LikeJS
import JsHs.Types as Import
import qualified JsHs.Array as Array
