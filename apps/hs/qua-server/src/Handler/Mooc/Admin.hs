-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.Admin
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
module Handler.Mooc.Admin
    ( getAdminR
    ) where

import Import

getAdminR :: Handler Html
getAdminR =
    fullLayout Nothing "Welcome to the admin page" $ do
        setTitle "qua-kit - admin page"
        $(widgetFile "mooc/admin")
