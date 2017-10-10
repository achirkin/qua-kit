{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( ReviewPost (..)
    , ReviewSettings (..)
    , Settings (..)
    , TCriterion (..)
    , ThumbState (..)
    , TReview (..)
    ) where

import Data.Semigroup
import Data.Time.Clock (UTCTime)
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

data Settings = Settings {
    loggingUrl               :: Maybe QuaText -- ^ WebSocket URL to send user analytics to
  , luciUrl                  :: Maybe QuaText -- ^ WebSocket URL to connect to Luci
  , getSubmissionGeometryUrl :: Maybe QuaText -- ^ URL to GET geoJSON for current submission
  , postSubmissionUrl        :: Maybe QuaText -- ^ URL for students to POST their new submission to
  , reviewSettingsUrl        :: Maybe QuaText -- ^ URL to get settings related to reviews
  , editMode                 :: Bool          -- ^ Are we in edit mode?
  , viewUrl                  :: QuaText       -- ^ URL of current qua-viewer page
  } deriving Generic
instance ToJSON    Settings
instance FromJSON  Settings
instance Semigroup Settings where
  (<>) s1 s2 = Settings {
                 loggingUrl               = orElse (loggingUrl s1) (loggingUrl s2)
               , luciUrl                  = orElse (luciUrl s1) (luciUrl s2)
               , getSubmissionGeometryUrl = orElse (getSubmissionGeometryUrl s1) (getSubmissionGeometryUrl s2)
               , postSubmissionUrl        = orElse (postSubmissionUrl s1) (postSubmissionUrl s2)
               , reviewSettingsUrl        = orElse (reviewSettingsUrl s1) (reviewSettingsUrl s2)
               , editMode                 = editMode s1 || editMode s2
               , viewUrl                  = orElseText (viewUrl s1) (viewUrl s2)
               }
instance Monoid Settings where
  mappend = (<>)
  mempty = Settings {
             loggingUrl               = Nothing
           , luciUrl                  = Nothing
           , getSubmissionGeometryUrl = Nothing
           , postSubmissionUrl        = Nothing
           , reviewSettingsUrl        = Nothing
           , editMode                 = False
           , viewUrl                  = ""
           }

data ReviewSettings = ReviewSettings {
    criterions :: [TCriterion] -- ^ criterions this submission can be reviewed with
  , reviews    :: [TReview]    -- ^ reviews of this submission
  , reviewsUrl :: QuaText      -- ^ URL to fetch updated list of reviews
  } deriving Generic
instance ToJSON    ReviewSettings
instance FromJSON  ReviewSettings


data TCriterion = TCriterion {
    tCriterionId          :: Int
  , tCriterionName        :: QuaText
  --, criterionDescription :: QuaText
  --, criterionImage       :: ByteString
  , tCriterionIcon        :: QuaText
  } deriving Generic
instance ToJSON    TCriterion
instance FromJSON  TCriterion

data ThumbState = None | ThumbUp | ThumbDown deriving (Generic, Eq)
instance FromJSON ThumbState
instance ToJSON ThumbState
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions
#endif

data ReviewPost = ReviewPost {
    criterionId       :: !Int
  , thumb             :: !ThumbState
  , reviewPostComment :: !QuaText
  } deriving Generic
instance FromJSON ReviewPost
instance ToJSON   ReviewPost
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions
#endif

data TReview = TReview {
    tReviewId          :: !Int
  , tReviewUserName    :: !QuaText
  , tReviewCriterionId :: !Int
  , tReviewThumb       :: !ThumbState
  , tReviewComment     :: !QuaText
  , tReviewTimestamp   :: !UTCTime
  } deriving Generic
instance ToJSON    TReview
instance FromJSON  TReview
