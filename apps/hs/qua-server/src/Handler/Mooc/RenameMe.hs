{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.RenameMe
  ( getRenameMeR
  ) where


import Import
import qualified Data.Text as Text

-- | Update a user name for a given user
getRenameMeR :: Handler Text
getRenameMeR = do
  mnewname <- fmap Text.strip <$> lookupGetParam "newname"
  muser <- maybeAuthId
  case (,) <$> muser <*> mnewname of
    Nothing -> return "{}"
    Just (key,newname) -> if length newname > 2
                          then do
                                runDB $ update key [UserName =. newname]
                                return $ "{\"newname\":\"" <> newname <> "\",\"twochars\":\"" <> twoCharsName newname <> "\"}"
                          else return "{}"
