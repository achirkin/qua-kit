{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QuaTypes (
    Settings (..)
  ) where

import Control.Applicative ((<|>))
import Data.Semigroup
import GHC.Generics
import QuaTypes.Commons

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
instance Semigroup Settings where
  (<>) s1 s2 = Settings {
      loggingUrl               = loggingUrl s1               <|> loggingUrl s2
    , luciUrl                  = luciUrl s1                  <|> luciUrl s2
    , getSubmissionGeometryUrl = getSubmissionGeometryUrl s1 <|> getSubmissionGeometryUrl s2
    , postSubmissionUrl        = postSubmissionUrl s1        <|> postSubmissionUrl s2
    , reviewSettingsUrl        = reviewSettingsUrl s1        <|> reviewSettingsUrl s2
    , viewUrl                  = case viewUrl s1 of
                                   "" -> viewUrl s2
                                   s  -> s
    }
instance Monoid Settings where
  mappend = (<>)
  mempty = Settings {
             loggingUrl               = Nothing
           , luciUrl                  = Nothing
           , getSubmissionGeometryUrl = Nothing
           , postSubmissionUrl        = Nothing
           , reviewSettingsUrl        = Nothing
           , viewUrl                  = ""
           }
