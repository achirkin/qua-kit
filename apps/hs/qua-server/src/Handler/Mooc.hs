-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Mooc
  ( getMoocHomeR, postMoocHomeR
  ) where




import qualified Data.Map as Map
--import qualified Database.Esqueleto      as E
--import           Database.Esqueleto      ((^.))
--import qualified Database.Persist.Sql    as PSQL

import Import


postMoocHomeR :: Handler Html
postMoocHomeR = getMoocHomeR

getMoocHomeR :: Handler Html
getMoocHomeR  = do
    setUltDestCurrent

    mmsg <- getMessage
    muser <- fmap entityVal <$> maybeAuth

    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFile "mooc/home")


