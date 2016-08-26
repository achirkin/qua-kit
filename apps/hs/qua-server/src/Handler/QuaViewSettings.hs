-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.QuaViewSettings
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.QuaViewSettings
    ( getQuaViewSettingsR
    ) where


import Import
--import Data.Aeson
--import qualified Data.Text as Text

getQuaViewSettingsR :: Handler TypedContent
getQuaViewSettingsR = do
    muser <- maybeAuth

    app <- getYesod
    req <- waiRequest
    let role = getRole muser
        appr = getApprootText guessApproot app req
        quaviewHTTP = yesodRender app appr HomeR []
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
        quaViewLoggingWS = "ws" <> drop 4 (yesodRender app appr QVLoggingR [])
        submitHTTP = yesodRender app appr SubmitProposalR []
    return . TypedContent typeJson . toContent . object $
      [ "viewRoute"  .= quaviewHTTP
      , "luciRoute"  .= luciProxyWS
      , "loggingUrl" .= quaViewLoggingWS
      , "submitUrl"  .= submitHTTP
      , "profile" .= String ( case role of
            UR_NOBODY  -> "full"
            UR_STUDENT -> "edit"
            UR_LOCAL   -> "full"
            UR_ADMIN   -> "full"
            )
      ]
  where
    getRole Nothing = UR_NOBODY
    getRole (Just (Entity _ u)) = userRole u

--  asLikeJS jsv = Settings
--   { objectScale = getProp "objectScale" jsv
--   , viewRoute   = getProp "viewRoute" jsv
--   , luciRoute   = getProp "luciRoute" jsv
--   , scenarioUrl = getProp "scenarioUrl" jsv
--   , profile     = case getProp "profile" jsv :: Maybe JSString of
--                    Just "edit" -> ExternalEditor
--                    Just "view" -> ExternalViewer
--                    _ -> Full
--   , submitUrl   = getProp "submitUrl" jsv
--   , loggingUrl  = getProp "loggingUrl" jsv
--   }
