-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.RenameMe
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.RenameMe
  ( getRenameMeR
  ) where


import Import
import qualified Data.Text as Text

-- | Update a user name for a given user
getRenameMeR :: Handler Html
getRenameMeR = do
  mnewname <- fmap Text.strip <$> lookupGetParam "newname"
  muser <- maybeAuthId
  case (,) <$> muser <*> mnewname of
    Nothing -> return ()
    Just (key,newname) -> when (length newname > 2) $
                           runDB $ update key [UserName =. newname]
  redirectUltDest MoocHomeR
