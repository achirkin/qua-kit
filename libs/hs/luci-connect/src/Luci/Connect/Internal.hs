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
    , ToJSON (..), FromJSON (..), Value (..), fromJSON
      -- * Other useful types
    , Text, ByteString, Int64
    ) where

import Data.Text
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes)

import Data.Int (Int64)

-- | Maybe wrapper for '(.=)'
(.=!) :: (ToJSON v, KeyValue kv) => Text -> v -> Maybe kv
(.=!) k v = Just $ k .= v

-- | Maybe wrapper for '(.=)' with Maybe values
(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) k v = (k .=) <$> v

-- | allow composition of '(.:)'
(..:) :: FromJSON a => Parser Object -> Text -> Parser a
(..:) x t = x >>= (.: t)

-- | Maybe unwrapper for 'object'
objectM :: [Maybe Pair] -> Value
objectM = object . catMaybes
