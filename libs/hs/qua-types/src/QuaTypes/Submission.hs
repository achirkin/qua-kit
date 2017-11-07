{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The data sent to server when a student submits a scenario (in workshop mode).
module QuaTypes.Submission (
    SubmissionPost (..)
  ) where

import GHC.Generics
import QuaTypes.Commons

-- | The data sent when a student submits a design.
data SubmissionPost = SubmissionPost {
    subPostDescription  :: !QuaText
    -- ^ Arbitrary description given by a student
  , subPostGeometry     :: !GeoJson
    -- ^ Scenario (FeatureCollection) in JSON format
  , subPostPreviewImage :: !Base64
    -- ^ A sceenshot of the current design
  } deriving Generic
instance FromJSON  SubmissionPost
instance ToJSON    SubmissionPost
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
