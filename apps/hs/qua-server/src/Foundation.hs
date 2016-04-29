{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

--import Data.Default
import Database.Persist.Sql
import Network.HTTP.Client.Conduit (Manager)
import Text.Hamlet
import Yesod
import Yesod.EmbeddedStatic
--import Yesod.Default.Util

import Config
--import Model

-- | Add all files from folder "static"
mkEmbeddedStatic True "appStatic" [embedDir "static"]

-- | Generate routes
mkYesodData "App" $(parseRoutesFile "config/routes")

--newtype MIME = MIME Text
--newtype FileName = FileName Text
--data StoredFile = StoredFile !FileName !MIME !ByteString
data App = App
    { connPool    :: ConnectionPool
    , httpManager :: Manager
    , getStatic   :: EmbeddedStatic
    }



instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/site-layout.hamlet")
  addStaticContent = embedStaticContent getStatic StaticR Right
  maximumContentLength _ _ = Just 2000000


instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB (const persistConfig) connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool




--mkEmbeddedStatic False "quaViewHtml" [embedFileAt "/" "web/qua-view.html"]
--mkEmbeddedStatic False "quaViewCss" [embedFileAt "/qua-view.css" "web/qua-view.css"]
--mkEmbeddedStatic False "quaViewJs" [embedFileAt "/qua-view.js" "web/qua-view.js"]
--mkEmbeddedStatic False "numericMinJs" [embedFileAt "/numeric.min.js" "web/numeric.min.js"]

-- staticFilesList "web" ["qua-view.html", "qua-view.css", "qua-view.js", "numeric.min.js"]



