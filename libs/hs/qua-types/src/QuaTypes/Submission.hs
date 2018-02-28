{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}

-- | The data sent to server when a student submits a scenario (in workshop mode).
module QuaTypes.Submission (
    SubmissionPost (..), SubmitResponse (..), SubmissionInfo (..)
  ) where

import GHC.Generics
import QuaTypes.Commons
import Data.Time.Clock

-- | The data sent when a student submits a design.
data SubmissionPost = SubmissionPost
  { subPostDescription  :: QuaText
    -- ^ Arbitrary description given by a student
  , subPostGeometry     :: GeoJson
    -- ^ Scenario (FeatureCollection) in JSON format
  , subPostPreviewImage :: Base64
    -- ^ A sceenshot of the current design
  } deriving Generic
instance FromJSON  SubmissionPost
instance ToJSON    SubmissionPost
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif


newtype SubmitResponse = SubmitResponse
  { srMessage :: QuaText -- ^ A pre-formatted html message that is shown to a user
                         --     after they succesfully submit their work using `putSubmissionUrl`.
                         --   For qua-view this is an opaque html text.
                         --   For qua-server this is a template with placeholders to be interpolated
                         --     with user data.
                         --     This means, qua-server has to replace such tokens before sending settings;
                         --     e.g. ${userId} should be replaced with the current id of a user.
  } deriving Generic
instance FromJSON  SubmitResponse
instance ToJSON    SubmitResponse
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif


data SubmissionInfo = SubmissionInfo
  { subInfoDescription :: QuaText
  , subInfoUserName    :: QuaText
  , subInfoTime        :: UTCTime
  } deriving Generic
instance FromJSON  SubmissionInfo
instance ToJSON    SubmissionInfo
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
