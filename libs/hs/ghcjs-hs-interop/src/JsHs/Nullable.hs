{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Nullable
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- A set of hackery GHCJS function often used for debugging
--
-----------------------------------------------------------------------------

module JsHs.Nullable
    ( Nullable (..)
    ) where

import Data.Coerce
import JsHs.Types (JSVal)
import JsHs.Types.Prim (jsNull)

-- | Data types that may be null.
--   I think this is potentially dangerous!
--   Use it only when null value is really ok as a function argument.
class Coercible JSVal a => Nullable a where
    -- | Create null reference and cast it to a given type
    {-# INLINE nullRef #-}
    nullRef :: a
    nullRef = coerce jsNull

instance Nullable JSVal where
    {-# INLINE nullRef #-}
    nullRef = jsNull
