-----------------------------------------------------------------------------
-- |
-- Module      :  Model.CustomTypes
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Model.CustomTypes
  ( UserRole (..)
  ) where


import Prelude
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))

data UserRole
  = UR_NOBODY
  | UR_STUDENT
  | UR_LOCAL
  | UR_ADMIN
  deriving (Eq, Ord, Enum, Show)

instance PersistField UserRole where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = fmap toEnum . fromPersistValue

instance PersistFieldSql UserRole where
  sqlType _ = SqlInt64
