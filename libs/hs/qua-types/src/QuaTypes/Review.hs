{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict        #-}

-- | Review widget is a place where users can comment design submissions of others.
--   They have to upvote or downvote a design with respect to a single criterion
--   and (optionally) attach some textual explanation.
module QuaTypes.Review
    ( ReviewPost (..)
    , ExpertReviewPost (..)
    , ReviewSettings (..)
    , Criterion (..)
    , ThumbState (..)
    , Review (..)
    , Rating (..)
    ) where

import           Data.Time.Clock  (UTCTime)
import           GHC.Generics
import           QuaTypes.Commons

-- | User comments on a design submission.
--   Used in the workshop mode to let users comments each others' works
--   and visualize it in a widget next to qua-view canvas.
data ReviewSettings = ReviewSettings
  { criterions       :: [Criterion] -- ^ criterions this submission
                                    --   can be reviewed with
  , reviews          :: [Review]    -- ^ reviews of this submission
  , reviewsUrl       :: Maybe Url   -- ^ URL to POST new review to,
                                    --   and GET list of reviews plus expert reviews
  , expertReviewsUrl :: Maybe Url   -- ^ URL to POST new expert review to
  } deriving Generic
instance FromJSON  ReviewSettings
instance ToJSON    ReviewSettings
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

-- | Information needed to draw design criterion icon and show its name.
--   Users get reviews with a single mark upvoting or downvoting a design
--   w.r.t. this criterion.
--   Therefore we need to show the icons for available criterions in the widget.
data Criterion = Criterion
  { criterionId   :: Int
  , criterionName :: QuaText
  , criterionIcon :: QuaText
  } deriving Generic
instance FromJSON  Criterion
instance ToJSON    Criterion
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

-- | Whether student selected to upvote or downvote design
--    (or has not selected anything yet).
data ThumbState = None | ThumbUp | ThumbDown
  deriving (Generic, Eq)
instance FromJSON ThumbState
instance ToJSON   ThumbState
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

-- | User's input - a review (comment) of the design viewed
data ReviewPost = ReviewPost
  { reviewPostCriterionId :: Int
  , reviewPostThumb       :: ThumbState
  , reviewPostComment     :: QuaText
  } deriving Generic
instance FromJSON ReviewPost
instance ToJSON   ReviewPost

-- | Expert's input - a review of the design viewed
data ExpertReviewPost = ExpertReviewPost
  { expertReviewPostGrade   :: Int
  , expertReviewPostComment :: QuaText
  } deriving Generic
instance FromJSON ExpertReviewPost
instance ToJSON   ExpertReviewPost

-- | Previous reviews of the viewed design
data Review = Review
  { reviewUserName  :: QuaText
  , reviewIsMine    :: Bool -- ^ review is by currently logged in user
  , reviewRating    :: Rating
  , reviewComment   :: QuaText
  , reviewTimestamp :: UTCTime
  } deriving Generic
instance FromJSON  Review
instance ToJSON    Review
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif


data Rating
  = UserRating
  { ratingCriterionId :: Int
  , ratingThumb       :: ThumbState
  }
  | ExpertRating
  { ratingGrade       :: Int
  } deriving Generic
instance FromJSON  Rating
instance ToJSON    Rating
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
