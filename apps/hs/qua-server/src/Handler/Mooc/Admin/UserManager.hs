{-# RecordWildCards #-}
module Handler.Mooc.Admin.UserManager
    ( getAdminUserManagerR
    , postSetUserRoleR
    ) where

import Import hiding ((==.), on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T
import Text.Read

import Database.Persist
import Database.Persist.Sql

import Yesod.Form.Bootstrap3

getAdminUserManagerR :: Handler Html
getAdminUserManagerR = do
    users <- runDB $ selectList [] [Asc UserId]
    let roles = [minBound .. maxBound] :: [UserRole]
    fullLayout Nothing "Welcome to the user manager" $ do
        setTitle "qua-kit - user manager"
        $(widgetFile "mooc/admin/user-manager")

roleFormInput :: FormInput Handler (Maybe UserRole)
roleFormInput = (readMaybe . T.unpack) <$> ireq textField "role"

postSetUserRoleR :: UserId -> Handler Html
postSetUserRoleR userId = do
    mrole <- runInputPost roleFormInput
    case mrole of
        Nothing -> sendResponseStatus status400 ("invalid role" :: Text)
        Just role -> do
            runDB $ update userId [UserRole =. role]
            redirect AdminUserManagerR
