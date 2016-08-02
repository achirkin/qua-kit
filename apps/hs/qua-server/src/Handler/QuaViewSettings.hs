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
    app <- getYesod
    req <- waiRequest
    let appr = getApprootText guessApproot app req
        quaviewHTTP = yesodRender app appr HomeR []
        luciProxyHTTP = yesodRender app appr LuciR []
        luciProxyWS = "ws" <> drop 4 luciProxyHTTP
    return . TypedContent typeJson . toContent . object $
      [ "viewRoute" .= quaviewHTTP
      , "luciRoute" .= luciProxyWS
      ]
