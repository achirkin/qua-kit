{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module QuaTypes.Submission (
    SubmissionPost (..)
  ) where

import GHC.Generics
import QuaTypes.Commons

data SubmissionPost = SubmissionPost {
    subPostDescription  :: !QuaText
  , subPostGeometry     :: !GeoJson
  , subPostPreviewImage :: !Base64
  } deriving Generic
instance FromJSON  SubmissionPost
instance ToJSON    SubmissionPost
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
