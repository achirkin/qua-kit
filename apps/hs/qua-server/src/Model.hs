{-# LANGUAGE FlexibleInstances #-}

module Model
  ( module Model
  , module Model.CustomTypes
  ) where

import Model.CustomTypes
import ClassyPrelude.Yesod
import Database.Persist.Quasi


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
