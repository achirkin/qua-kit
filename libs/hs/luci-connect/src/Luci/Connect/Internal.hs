-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Connect.Internal
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- JSON helpers and etc.
--
-----------------------------------------------------------------------------

module Luci.Connect.Internal
    ( -- * JSON helpers
      (.=!), (.=?), (.=), object, objectM, (.:), (.:?), (..:)
    , ToJSON (..), FromJSON (..), Value (..)
      -- * Other useful types
    , Text, ByteString, Int64
    ) where

import Data.Text
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes)

import Data.Int (Int64)

(.=!) :: (ToJSON v, KeyValue kv) => Text -> v -> Maybe kv
(.=!) k v = Just $ k .= v

(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) k v = (k .=) <$> v

(..:) :: FromJSON a => Parser Object -> Text -> Parser a
(..:) x t = x >>= (.: t)

objectM :: [Maybe Pair] -> Value
objectM = object . catMaybes
