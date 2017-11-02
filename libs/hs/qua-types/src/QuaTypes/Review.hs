{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module QuaTypes.Review (
    ReviewPost (..)
  , ReviewSettings (..)
  , Criterion (..)
  , ThumbState (..)
  , Review (..)
  ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics
import QuaTypes.Commons

data ReviewSettings = ReviewSettings {
    criterions :: [Criterion] -- ^ criterions this submission can be reviewed with
  , reviews    :: [Review]    -- ^ reviews of this submission
  , reviewsUrl :: Maybe Url    -- ^ URL to post new review to and fetch updated list of reviews from
  } deriving Generic
instance FromJSON  ReviewSettings
instance ToJSON    ReviewSettings
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif


data Criterion = Criterion {
    criterionId   :: !Int
  , criterionName :: !QuaText
  , criterionIcon :: !QuaText
  } deriving Generic
instance FromJSON  Criterion
instance ToJSON    Criterion
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

data ThumbState = None | ThumbUp | ThumbDown deriving (Generic, Eq)
instance FromJSON ThumbState
instance ToJSON   ThumbState
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

data ReviewPost = ReviewPost {
    reviewPostCriterionId :: !Int
  , reviewPostThumb       :: !ThumbState
  , reviewPostComment     :: !QuaText
  } deriving Generic
instance FromJSON ReviewPost
instance ToJSON   ReviewPost

data Review = Review {
    reviewId          :: !Int
  , reviewUserName    :: !QuaText
  , reviewCriterionId :: !Int
  , reviewThumb       :: !ThumbState
  , reviewComment     :: !QuaText
  , reviewTimestamp   :: !UTCTime
  } deriving Generic
instance FromJSON  Review
instance ToJSON    Review
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
