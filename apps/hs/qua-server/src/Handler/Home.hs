-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Home
    ( getHomeR
    , getQuaViewR
    , getQuaViewCSSR
    , getQuaViewJSR
    , getNumericMinJSR
    ) where


--import Control.Monad.Trans.Resource (runResourceT)
--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
--import Data.Time
--import Data.Text (Text)
--import Database.Persist.Sql (fromSqlKey)
--import qualified Data.Text as Text
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util

import Foundation
--import Model

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "qua-view"
        $(widgetFileNoReload def "home")


getQuaViewR :: Handler Html
getQuaViewR = sendFile "text/html" "web/qua-view.html"

getQuaViewCSSR :: Handler ()
getQuaViewCSSR = sendFile "text/css" "web/qua-view.css"

getQuaViewJSR :: Handler ()
getQuaViewJSR = sendFile "text/javascript" "web/qua-view.js"

getNumericMinJSR :: Handler ()
getNumericMinJSR = sendFile "text/javascript" "web/numeric.min.js"

