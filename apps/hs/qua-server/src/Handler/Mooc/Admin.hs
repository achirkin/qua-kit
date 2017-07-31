{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Admin
    ( getAdminR
    , requireAdmin
    ) where

import Import


getAdminR :: Handler Html
getAdminR = do
    fullLayout Nothing "Welcome to the admin page" $ do
        setTitle "qua-kit - admin page"
        $(widgetFile "mooc/admin")

requireAdmin :: Handler ()
requireAdmin = do
    role <- muserRole <$> maybeAuth
    unless (role == UR_ADMIN) $
        sendResponseStatus
            status403
            ("You must be an admin to access this page" :: Text)
