{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( ReviewPost (..)
    , ThumbState (..)
    , ReviewSettings (..)
    , Settings   (..)
    ) where

import Data.Semigroup
import GHC.Generics

#ifdef ghcjs_HOST_OS
import Data.JSString
import JavaScript.JSON.Types.Internal ()
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Generic ()

type QuaText = JSString
emptyQuaText = Data.JSString.null

#else

import Data.Aeson
import Data.Text

type QuaText = Text
emptyQuaText = Data.Text.null
#endif

-- | Left-biased choice on foldables
orElse :: Foldable t => t a -> t a -> t a
x `orElse` y = if Prelude.null x
                 then y
                 else x

orElseText :: QuaText -> QuaText -> QuaText
x `orElseText` y = if emptyQuaText x
                     then y
                     else x

data ThumbState = None | ThumbUp | ThumbDown deriving Generic
instance ToJSON ThumbState
instance FromJSON ThumbState

data ReviewPost = ReviewPost {
    thumb             :: !ThumbState
  , reviewPostComment :: !QuaText
  } deriving Generic
instance ToJSON   ReviewPost
instance FromJSON ReviewPost

data Settings = Settings {
    loggingUrl              :: Maybe QuaText -- ^ WebSocket URL to send user analytics to
  , luciUrl                 :: Maybe QuaText -- ^ WebSocket URL to connect to Luci
  , getSubmissionGeoJsonUrl :: Maybe QuaText -- ^ URL to GET geoJSON for current submission
  , postSubmissionUrl       :: Maybe QuaText -- ^ URL for students to POST their new submission to
  , reviewSettingsUrl       :: Maybe QuaText
  , viewMode                :: QuaText       -- TODO: use proper type
  , viewUrl                 :: QuaText       -- ^ URL of current qua-viewer page
  } deriving Generic
instance ToJSON    Settings
instance FromJSON  Settings
instance Semigroup Settings where
  (<>) s1 s2 = Settings {
                 loggingUrl              = orElse (loggingUrl s1) (loggingUrl s2)
               , luciUrl                 = orElse (luciUrl s1) (luciUrl s2)
               , getSubmissionGeoJsonUrl = orElse (getSubmissionGeoJsonUrl s1) (getSubmissionGeoJsonUrl s2)
               , postSubmissionUrl       = orElse (postSubmissionUrl s1) (postSubmissionUrl s2)
               , reviewSettingsUrl       = orElse (reviewSettingsUrl s1) (reviewSettingsUrl s2)
               , viewMode                = orElseText (viewMode s1) (viewMode s2)
               , viewUrl                 = orElseText (viewUrl s1) (viewUrl s2)
               }
instance Monoid Settings where
  mappend = (<>)
  mempty = Settings {
             loggingUrl              = Nothing
           , luciUrl                 = Nothing
           , getSubmissionGeoJsonUrl = Nothing
           , postSubmissionUrl       = Nothing
           , reviewSettingsUrl       = Nothing
           , viewMode                = ""
           , viewUrl                 = ""
           }

data ReviewSettings = ReviewSettings {
    postReviewUrl :: QuaText
  } deriving Generic
instance ToJSON    ReviewSettings
instance FromJSON  ReviewSettings
