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
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
    ( getHomeR
    , getQuaViewJSR
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
        -- add javascript dependencies to header
        toWidgetHead $
          [hamlet|
            <script src="@{StaticR numeric_min_js}" type="text/javascript">
          |]
        setTitle "qua-view"
        $(widgetFileNoReload def "qua-view")

getQuaViewJSR :: Handler ()
getQuaViewJSR = sendFile "text/javascript" "web/qua-view.js"


