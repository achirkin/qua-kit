{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.ByteString.Lazy as BSL
import Graphics.GD.ByteString.Lazy

--import Control.Arrow ((***))
--import Control.Concurrent.STM
--import Control.Exception (Exception, throwIO)
--import Control.Monad (liftM)
--import Data.ByteString.Lazy (ByteString)
--import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
--import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
--import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as Text
import Database.Persist.Sql
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import Network.HTTP.Client.Conduit (Manager)
import Text.Hamlet
import Yesod
import Yesod.EmbeddedStatic
import Yesod.Default.Util
import Data.Time.Clock

--import Foundation.OAuth

import Config
import Model
--import Debug.Trace (traceShow)
--import Data.Maybe (fromMaybe)

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
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  addStaticContent = embedStaticContent getStatic StaticR Right
--    where mini = if development then Right else minifym
--  makeSessionBackend _ = do
--        backend <- defaultClientSessionBackend 1 "keyfile.aes"
--        return $ Just backend
  maximumContentLength _ (Just ImageUploadR) = Just 20000000
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


getList :: Handler [Entity Story]
getList = runDB $ selectList [] []

--getList :: Handler [(Key StoredFile, StoredFile)]
--getList =
--    getYesod >>= liftIO . liftM IntMap.toList . readTVarIO . tstore

addFile :: Story -> Handler ()
addFile file = runDB $ insert_ file
--addFile file = do
--    app <- getYesod
--    liftIO . atomically $ do
--        nextId <- getNextId app
--        modifyTVar (tstore app) $ IntMap.insert nextId file


--getById :: (PersistEntity b, YesodPersistBackend App ~ PersistEntityBackend b)
--        =>  Key b -> Handler b
--getById ident = do
--    mfile <- runDB $ get ident
--    case mfile of
--      Nothing -> notFound
--      Just file -> return file

--getById :: Key UserStory -> Handler UserStory
--getById ident = do
--    store <- getYesod >>= liftIO . readTVarIO . tstore
--    case IntMap.lookup ident store of
--      Nothing -> notFound
--      Just bytes -> return bytes






data TStory = TStory
    { edxUserId     :: Text
    , edxContextId  :: Text
    , edxResLink    :: Text
    , edxOutcomeUrl :: Maybe Text
    , edxResultId   :: Maybe Text
    , tstoryAuthor  :: Maybe Text
    , tstoryImage   :: FileInfo
    , tstoryPlace   :: PlaceId
    , tstoryComment :: Textarea
    }


previewSize :: Int
previewSize = 400

persistStory :: TStory -> Handler (Entity Story)
persistStory story = runDB $ do
    studentId  <- saveStudent
    resourceId <- saveResource
    previewId  <- saveImgPreview
    let placeId = tstoryPlace story
    time <- liftIO getCurrentTime
    let r = Story
          { storyResource = resourceId
          , storyAuthor   = studentId
          , storyImage    = previewId
          , storyPlace    = placeId
          , storyComment  = unTextarea $ tstoryComment story
          , storyEdxOutcomeUrl = edxOutcomeUrl story
          , storyEdxResultId   = edxResultId story
          , storyCreationTime  = time
          }
    i <- insert r
    return $ Entity i r
  where
    saveStudent = do
      mEstudent <- getBy . EdxUserId $ edxUserId story
      case (mEstudent, tstoryAuthor story) of
       (Nothing, mauthor) -> insert Student
         { studentEdxUserId = edxUserId story
         , studentName      = mauthor
         }
       (Just (Entity stId _), Nothing) -> return stId
       (Just (Entity stId _), Just name) ->
            stId <$ update stId [StudentName =. Just name]

    saveCourse mEcourse = case mEcourse of
        Nothing -> insert $ EdxCourse (edxContextId story) Nothing
        Just (Entity i _) -> return i

    saveResource = do
      mEresource <- getBy . EdxResLink $ edxResLink story
      case mEresource of
        Nothing -> do
            mEcourse <- getBy . EdxContextId $ edxContextId story
            cId <- saveCourse mEcourse
            insert $ EdxResource (edxResLink story) cId Nothing
        Just (Entity i _) -> return i

    saveImage name ctype content =
        insert $ Image name ctype content

    saveImgPreview = do
        fb <- runResourceT $ fileSource (tstoryImage story) $$ sinkLbs
        let mime = fileContentType $ tstoryImage story
        preview <- liftIO $ do
          pimg <- newImage (previewSize, previewSize)
          img <- if "jpeg" `Text.isSuffixOf` mime
            then loadJpegByteString fb
            else if "png" `Text.isSuffixOf` mime
            then loadPngByteString fb
            else if "gif" `Text.isSuffixOf` mime
            then loadGifByteString fb
            else return pimg  -- TODO: make a proper error generation.
          (w,h) <- imageSize img
          let ssize  = min w h
              spoint = ((w - ssize) `div` 2, (h-ssize) `div` 2)
              dpoint = (0,0)
          copyRegionScaled spoint (ssize,ssize) img
                           dpoint (previewSize, previewSize) pimg
          savePngByteString pimg
        liftIO $ BSL.writeFile (Text.unpack (fileName $ tstoryImage story) ++ ".png") preview
        let content = BSL.toStrict fb
            ctype   = mime
            name    = fileName $ tstoryImage story
        imgId <- saveImage name ctype content
        insert $ ImagePreview (BSL.toStrict preview) imgId




