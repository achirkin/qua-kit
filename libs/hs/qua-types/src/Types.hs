{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( ReviewPost (..)
    , ReviewSettings (..)
    , Settings (..)
    , SubmissionPost (..)
    , TCriterion (..)
    , ThumbState (..)
    , TReview (..)
    ) where

import Control.Applicative ((<|>))
import Data.Semigroup
import Data.Time.Clock (UTCTime)
import GHC.Generics

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

data ReviewSettings = ReviewSettings {
    criterions :: [TCriterion] -- ^ criterions this submission can be reviewed with
  , reviews    :: [TReview]    -- ^ reviews of this submission
  , reviewsUrl :: Maybe Url    -- ^ URL to post new review to and fetch updated list of reviews from
  } deriving Generic
instance FromJSON  ReviewSettings
instance ToJSON    ReviewSettings
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif


data TCriterion = TCriterion {
    tCriterionId          :: Int
  , tCriterionName        :: QuaText
  --, criterionDescription :: QuaText
  --, criterionImage       :: ByteString
  , tCriterionIcon        :: QuaText
  } deriving Generic
instance FromJSON  TCriterion
instance ToJSON    TCriterion
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

data ThumbState = None | ThumbUp | ThumbDown deriving (Generic, Eq)
instance FromJSON ThumbState
instance ToJSON ThumbState
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

data ReviewPost = ReviewPost {
    criterionId       :: !Int
  , thumb             :: !ThumbState
  , reviewPostComment :: !QuaText
  } deriving Generic
instance FromJSON ReviewPost
instance ToJSON   ReviewPost

data TReview = TReview {
    tReviewId          :: !Int
  , tReviewUserName    :: !QuaText
  , tReviewCriterionId :: !Int
  , tReviewThumb       :: !ThumbState
  , tReviewComment     :: !QuaText
  , tReviewTimestamp   :: !UTCTime
  } deriving Generic
instance FromJSON  TReview
instance ToJSON    TReview
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

data SubmissionPost = SubmissionPost {
    tSubPostDescription  :: !QuaText
  , tSubPostGeometry     :: !GeoJson
  , tSubPostPreviewImage :: !Base64
  } deriving Generic
instance FromJSON  SubmissionPost
instance ToJSON    SubmissionPost
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
