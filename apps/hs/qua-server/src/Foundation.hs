-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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

import Control.Arrow ((***))
--import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
--import Control.Monad (liftM)
--import Data.ByteString.Lazy (ByteString)
--import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as Text
import Database.Persist.Sql
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import Network.HTTP.Client.Conduit (Manager)
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth
--import Web.Authenticate.OAuth
import Yesod.Default.Util

--import Foundation.OAuth

import Config
import Model
--import Debug.Trace (traceShow)
--import Data.Maybe (fromMaybe)

--newtype MIME = MIME Text
--newtype FileName = FileName Text
--data StoredFile = StoredFile !FileName !MIME !ByteString
data App = App
    { connPool    :: ConnectionPool
    , httpManager :: Manager
    , oauthData   :: OAuth
    }

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
--  makeSessionBackend _ = do
--        backend <- defaultClientSessionBackend 1 "keyfile.aes"
--        return $ Just backend

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB (const persistConfig) connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesodData "App" $(parseRoutesFile "config/routes")

--mkEmbeddedStatic False "quaViewHtml" [embedFileAt "/" "web/qua-view.html"]
--mkEmbeddedStatic False "quaViewCss" [embedFileAt "/qua-view.css" "web/qua-view.css"]
--mkEmbeddedStatic False "quaViewJs" [embedFileAt "/qua-view.js" "web/qua-view.js"]
--mkEmbeddedStatic False "numericMinJs" [embedFileAt "/numeric.min.js" "web/numeric.min.js"]

-- staticFilesList "web" ["qua-view.html", "qua-view.css", "qua-view.js", "numeric.min.js"]


getList :: Handler [Entity UserStory]
getList = runDB $ selectList [] []

--getList :: Handler [(Key StoredFile, StoredFile)]
--getList =
--    getYesod >>= liftIO . liftM IntMap.toList . readTVarIO . tstore

addFile :: UserStory -> Handler ()
addFile file = runDB $ insert_ file
--addFile file = do
--    app <- getYesod
--    liftIO . atomically $ do
--        nextId <- getNextId app
--        modifyTVar (tstore app) $ IntMap.insert nextId file

getById :: Key UserStory -> Handler UserStory
getById ident = do
    mfile <- runDB $ get ident
    case mfile of
      Nothing -> notFound
      Just file -> return file

--getById :: Key UserStory -> Handler UserStory
--getById ident = do
--    store <- getYesod >>= liftIO . readTVarIO . tstore
--    case IntMap.lookup ident store of
--      Nothing -> notFound
--      Just bytes -> return bytes




instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins app =
        [ authOAuth (oauthData app)
            (mkExtractCreds "testOAuth" "oauth_token")
           -- unCredential $ getAccessToken (oauthData app) cred (httpManager app))
        ]
    authHttpManager = httpManager
    maybeAuthId = lookupSession "_ID"

mkExtractCreds :: YesodAuth m => Text -> String -> Credential -> IO (Creds m)
mkExtractCreds name idName (Credential dic) = do
  let mcrId = bsToText <$> lookup (encodeUtf8 $ Text.pack idName) dic
  case mcrId of
    Just crId -> return $ Creds name crId $ map (bsToText *** bsToText) dic
    Nothing -> throwIO $ MyCredentialError ("key not found: " ++ idName) (Credential dic)
  where bsToText = decodeUtf8With lenientDecode

data MyAuthException = MyCredentialError String Credential
                         | MySessionError String
                           deriving (Show)

instance Exception MyAuthException
