{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

-- | Define serializable data types to enable interoperability between
--   qua-server and qua-view.
--
--   This module defines the data types used by the whole qua-view.
--   Other modules in @QuaTypes.*@ hierarchy define settings and data types
--   specific to particular widgets.
module QuaTypes
  ( Settings (..)
  , Permissions (..)
  ) where

import           Control.Applicative ((<|>))
import           Data.Semigroup
import           GHC.Generics
import           QuaTypes.Commons

-- | General qua-view settings
data Settings = Settings
  { loggingUrl               :: Maybe Url    -- ^ WebSocket URL to send user analytics to
  , luciUrl                  :: Maybe Url    -- ^ WebSocket URL to connect to Luci
  , getSubmissionGeometryUrl :: Maybe Url    -- ^ URL to GET geoJSON for current submission
  , getSubmissionInfoUrl     :: Maybe Url    -- ^ URL to GET JSON for current submission info
                                             --   It contains such information as
                                             --     user name and description of the submission.
  , putSubmissionUrl         :: Maybe Url    -- ^ URL for students to PUT their new or updated submission to
  , reviewSettingsUrl        :: Maybe Url    -- ^ URL to get settings related to reviews
  , viewUrl                  :: Url          -- ^ URL of current qua-viewer page
  , jsRootUrl                :: Url          -- ^ URL of the root folder for js file;
                                             --   e.g. jsRootUrl </> qua-view.js is the place of
                                             --   qua-view executable.
  , permissions              :: Permissions  -- ^ contains mostly booleans
  } deriving Generic
instance FromJSON  Settings
instance ToJSON    Settings
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif

-- | @(<>)@ operation is left-biased:
--   it prefers left settings fields if they exist
--   and gets right fields overwise.
--   Therefore, we can use it to overwrite default settings with ones obtained
--   from the server or in another way:
--
--   > currentSettings = reallyGoodSettings <> someFairSettings <> defaultSettings <> mempty
instance Semigroup Settings where
  (<>) s1 s2 = Settings
    { loggingUrl               = loggingUrl s1               <|> loggingUrl s2
    , luciUrl                  = luciUrl s1                  <|> luciUrl s2
    , getSubmissionGeometryUrl = getSubmissionGeometryUrl s1 <|> getSubmissionGeometryUrl s2
    , getSubmissionInfoUrl     = getSubmissionInfoUrl s1     <|> getSubmissionInfoUrl s2
    , putSubmissionUrl         = putSubmissionUrl s1         <|> putSubmissionUrl s2
    , reviewSettingsUrl        = reviewSettingsUrl s1        <|> reviewSettingsUrl s2
    , viewUrl                  = if viewUrl s1 == viewUrl mempty
                                 then viewUrl s2
                                 else viewUrl s1
    , jsRootUrl                = if jsRootUrl s1 == jsRootUrl mempty
                                 then jsRootUrl s2
                                 else jsRootUrl s1
    , permissions              = permissions s1
    }
instance Monoid Settings where
  mappend = (<>)
  mempty = Settings
     { loggingUrl               = Nothing
     , luciUrl                  = Nothing
     , getSubmissionGeometryUrl = Nothing
     , getSubmissionInfoUrl     = Nothing
     , putSubmissionUrl         = Nothing
     , reviewSettingsUrl        = Nothing
     , viewUrl                  = "" -- TODO: we need to decide on a better mempty value... index.html?
     , jsRootUrl                = ""
     , permissions              = Permissions
        { canEditProperties      = True
        , canEraseReloadGeometry = True
        , canAddDeleteGeometry   = True
        , canDownloadGeometry    = True
        , canModifyStaticObjects = True
        , showHiddenProperties   = False
        , showShareButton        = True
        , isViewerOnly           = False
        }
     }

-- | Some Bools so we can easily toggle functionality on and off
data Permissions = Permissions
  { canEditProperties      :: Bool -- ^ make properties in info-panel editable
  , canEraseReloadGeometry :: Bool -- ^ show button to clear geometry and load geometry from file
  , canAddDeleteGeometry   :: Bool -- ^ show geometry editor pane
  , canDownloadGeometry    :: Bool -- ^ show button to download geometry as file
  , canModifyStaticObjects :: Bool -- ^ make all objects movable, even those with property static=true
  , showHiddenProperties   :: Bool -- ^ also show blacklisted properties in info-panel
  , showShareButton        :: Bool -- ^ show share to social media etc.
  , isViewerOnly           :: Bool -- ^ Override everything and only allow
                                   --   to view scenario (no editing at all)
  } deriving Generic
instance FromJSON  Permissions
instance ToJSON    Permissions
#ifndef ghcjs_HOST_OS
  where
    toEncoding = genericToEncoding defaultOptions -- see Yesod.Core.Json
#endif
