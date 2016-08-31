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



--import qualified Data.Map as Map
import Import



getAdminR :: Handler Html
getAdminR = do
  mmsg <- getMessage
  muser <- fmap entityVal <$> maybeAuth
--  ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

  defaultLayout $ do
    setTitle "qua-kit - admin page"
    -- render all html
    $(widgetFile "mooc/admin")
