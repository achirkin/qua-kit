{-# RecordWildCards #-}
module Handler.Mooc.Admin.UserManager
    ( getAdminUserManagerR
    ) where

import Import hiding ((==.), on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T

import Database.Persist
import Database.Persist.Sql

import Yesod.Form.Bootstrap3

getAdminUserManagerR :: Handler Html
getAdminUserManagerR = do
    users <- runDB $ selectList [] [Asc UserId]
    fullLayout Nothing "Welcome to the user manager" $ do
        setTitle "qua-kit - user manager"
        $(widgetFile "mooc/admin/user-manager")
