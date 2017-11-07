{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Define serializable data types to enable interoperability between
--   qua-server and qua-view.
--
--   This module defines the data types used by the whole qua-view.
--   Other modules in @QuaTypes.*@ hierarchy define settings and data types
--   specific to particular widgets.
module QuaTypes (
    Settings (..)
  ) where

import Control.Applicative ((<|>))
import Data.Semigroup
import GHC.Generics
import QuaTypes.Commons

-- | General qua-view settings
data Settings = Settings {
    loggingUrl               :: Maybe Url  -- ^ WebSocket URL to send user analytics to
  , luciUrl                  :: Maybe Url  -- ^ WebSocket URL to connect to Luci
  , getSubmissionGeometryUrl :: Maybe Url  -- ^ URL to GET geoJSON for current submission
  , postSubmissionUrl        :: Maybe Url  -- ^ URL for students to POST their new submission to
  , reviewSettingsUrl        :: Maybe Url  -- ^ URL to get settings related to reviews
  , viewUrl                  :: Url        -- ^ URL of current qua-viewer page
  } deriving Generic
instance FromJSON  Settings
instance ToJSON    Settings
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

-- | @(<>)@ operation is left-biased:
--   it prefers left settings fields if they exist
--   and gets right fields overwise.
--   Therefore, we can use it to overwrite default settings with ones obtained
--   from the server of in another way:
--
--   > currentSettings = reallyGoodSettings <> someFairSettings <> defaultSettings <> mempty
instance Semigroup Settings where
  (<>) s1 s2 = Settings {
      loggingUrl               = loggingUrl s1               <|> loggingUrl s2
    , luciUrl                  = luciUrl s1                  <|> luciUrl s2
    , getSubmissionGeometryUrl = getSubmissionGeometryUrl s1 <|> getSubmissionGeometryUrl s2
    , postSubmissionUrl        = postSubmissionUrl s1        <|> postSubmissionUrl s2
    , reviewSettingsUrl        = reviewSettingsUrl s1        <|> reviewSettingsUrl s2
    , viewUrl                  = if viewUrl s1 == viewUrl mempty
                                 then viewUrl s2
                                 else viewUrl s1
    }
instance Monoid Settings where
  mappend = (<>)
  mempty = Settings {
       loggingUrl               = Nothing
     , luciUrl                  = Nothing
     , getSubmissionGeometryUrl = Nothing
     , postSubmissionUrl        = Nothing
     , reviewSettingsUrl        = Nothing
     , viewUrl                  = "" -- TODO: we need to decide on a better mempty value... index.html?
     }
