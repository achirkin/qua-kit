-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Home
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where


import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Yesod
import Yesod.Default.Util
import Yesod.Auth

import Foundation
import Model

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    (formWidget, formEncType) <- generateFormPost uploadForm
    storedFiles <- getList
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileNoReload def "home")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess fi -> do
        fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
        addFile $ StoredFile (fileName fi) (fileContentType fi)
--                              fileBytes
                             (S.pack . L.unpack $ fileBytes)
      _ -> return ()
    redirect HomeR

uploadForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadForm = renderDivs $ fileAFormReq "file"
