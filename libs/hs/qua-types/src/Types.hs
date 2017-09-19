{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( ReviewPost (..)
    , ThumbState (..)
    , Settings   (..)
    ) where

import Data.Semigroup
import Data.Text
import GHC.Generics

#ifdef ghcjs_HOST_OS
import JavaScript.JSON.Types.Internal ()
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Generic ()
#else
import Data.Aeson
#endif

-- | Left-biased choice on foldables
orElse :: Foldable t => t a -> t a -> t a
x `orElse` y = if Prelude.null x
                 then y
                 else x

orElseText :: Text -> Text -> Text
x `orElseText` y = if Data.Text.null x
                     then y
                     else x

data ThumbState = None | ThumbUp | ThumbDown deriving Generic
instance ToJSON ThumbState
instance FromJSON ThumbState

data ReviewPost = ReviewPost {
    thumb             :: !ThumbState
  , reviewPostComment :: !Text
  } deriving Generic
instance ToJSON   ReviewPost
instance FromJSON ReviewPost

data Settings = Settings {
    loggingUrl              :: Maybe Text -- ^ WebSocket URL to send user analytics to
  , luciUrl                 :: Maybe Text -- ^ WebSocket URL to connect to Luci
  , getSubmissionGeoJsonUrl :: Maybe Text -- ^ URL to GET geoJSON for current submission
  , postSubmissionUrl       :: Maybe Text -- ^ URL for students to POST their new submission to
  , viewMode                :: Text       -- TODO: use proper type from qua-server
  , viewUrl                 :: Text       -- ^ URL of current qua-viewer page
  } deriving Generic
instance ToJSON    Settings
instance FromJSON  Settings
instance Semigroup Settings where
  (<>) s1 s2 = Settings {
                 loggingUrl              = orElse (loggingUrl s1) (loggingUrl s2)
               , luciUrl                 = orElse (luciUrl s1) (luciUrl s2)
               , getSubmissionGeoJsonUrl = orElse (getSubmissionGeoJsonUrl s1) (getSubmissionGeoJsonUrl s2)
               , postSubmissionUrl       = orElse (postSubmissionUrl s1) (postSubmissionUrl s2)
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
           , viewMode                = ""
           , viewUrl                 = ""
           }
