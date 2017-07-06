{-# RecordWildCards #-}
module Handler.Mooc.Admin.UserManager
    ( getAdminUserManagerR
    , postSetUserRoleR
    , postAdminCreateUserR
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

import Yesod.Auth.Email
import Yesod.Core.Handler
import Yesod.Form.Input

import Handler.Mooc.Admin

getAdminUserManagerR :: Handler Html
getAdminUserManagerR = do
    requireAdmin
    users <- runDB $ selectList [] [Asc UserId]
    let roles = filter (/= UR_NOBODY) [minBound .. maxBound]
    fullLayout Nothing "Welcome to the user manager" $ do
        setTitle "qua-kit - user manager"
        $(widgetFile "mooc/admin/user-manager")

roleFormInput :: FormInput Handler (Maybe UserRole)
roleFormInput = (readMaybe . T.unpack) <$> ireq textField "role"

postSetUserRoleR :: UserId -> Handler Html
postSetUserRoleR userId = do
    requireAdmin
    mrole <- runInputPost roleFormInput
    case mrole of
        Nothing -> sendResponseStatus status400 ("invalid role" :: Text)
        Just role -> do
            runDB $ update userId [UserRole =. role]
            redirect AdminUserManagerR

data CreateUserData = CreateUserData
    { createUserDataEmail :: Text
    , createUserDataRoleStr :: Text
    } deriving (Show, Eq)

userFormInput :: FormInput Handler CreateUserData
userFormInput =
    CreateUserData <$> ireq textField "email" <*> ireq textField "role"

postAdminCreateUserR :: Handler Html
postAdminCreateUserR = do
    requireAdmin
    CreateUserData {..} <- runInputPost userFormInput
    case readMaybe $ T.unpack createUserDataRoleStr of
        Nothing -> sendResponseStatus status400 ("invalid role" :: Text)
        Just role -> do
            app <- getYesod :: Handler App
            let auth = getAuth app
            verKey <- liftIO $ randomKey app
            lid <- addUnverified createUserDataEmail verKey
            render <- getUrlRender
            let verUrl = render $ AuthR $ verifyR (toPathPiece lid) verKey
            sendVerifyEmail createUserDataEmail verKey verUrl
            $(logDebug) $
                T.unlines
                    [ "Sending verification url from admin panel."
                    , "Copy/ Paste this URL in your browser: " <> verUrl
                    ]
            runDB $ update lid [UserRole =. role]
            setMessage "User added successfully"
            redirect AdminUserManagerR
