{-# LANGUAGE CPP #-}

module QuaTypes.Commons (
    Base64
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

type Base64  = QuaText -- ^ binary data serialized as a base64-encoded string
type GeoJson = QuaText -- ^ to avoid parsing JSON unnecessarily, this is stored as a string
type Url     = QuaText -- ^ a string holding a URL
