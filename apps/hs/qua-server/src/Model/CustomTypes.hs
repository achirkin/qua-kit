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

-- | Specify what kind of actions a user can perform in qua-kit.
--   It does not specify the way the user has logged in (i.e. via edX or LDAP),
--   but rather tells what the user is expected to do in qua-kit.
data UserRole
  = UR_NOBODY
    -- ^ This is either a not-logged in person, or a user that has no exercises to do,
    --   is not an admin, and is not allowed to use local luci services
  | UR_STUDENT
    -- ^ A user that should submit an "exercise"
    -- (submit a scenario for an exercise or do votes or etc.)
  | UR_LOCAL
    -- ^ Just use custom scenarios and luci services with little or no restrictions
  | UR_ADMIN
    -- ^ Can edit or invite users, set up exercises, design criterions, etc.
  | UR_EXPERT
    -- ^ Should grade user submissions - write "expert reviews"
  deriving (Eq, Enum, Bounded, Show, Read)

instance PersistField UserRole where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = fmap toEnum . fromPersistValue

instance PersistFieldSql UserRole where
  sqlType _ = SqlInt64
