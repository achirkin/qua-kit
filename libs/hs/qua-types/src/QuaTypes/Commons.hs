{-# LANGUAGE CPP #-}

module QuaTypes.Commons
    ( Base64
    , GeoJson
    , Url
    , QuaText
    , FromJSON (..)
    , ToJSON (..)
#ifndef ghcjs_HOST_OS
    , defaultOptions
    , genericToEncoding
#endif
    ) where

#ifdef ghcjs_HOST_OS
import Data.JSString
import JavaScript.JSON.Types.Internal ()
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Generic ()

type QuaText = JSString

#else

import Data.Aeson
import Data.Text

type QuaText = Text
#endif

-- | binary data serialized as a base64-encoded string
type Base64  = QuaText
-- | to avoid parsing JSON unnecessarily, this is stored as a string
type GeoJson = QuaText
-- | a string holding a URL
type Url     = QuaText
