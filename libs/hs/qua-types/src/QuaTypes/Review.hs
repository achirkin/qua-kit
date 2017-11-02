{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module QuaTypes.Review (
    ReviewPost (..)
  , ReviewSettings (..)
  , TCriterion (..)
  , ThumbState (..)
  , TReview (..)
  ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics
import QuaTypes.Commons

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
