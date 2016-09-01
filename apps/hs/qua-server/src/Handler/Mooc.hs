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




--import qualified Data.Map as Map

import Import
import Handler.Mooc.EdxLogin



postMoocHomeR :: Handler TypedContent
postMoocHomeR = do
  master <- getYesod
  yreq <- getRequest
  dispatchLti (appLTICredentials $ appSettings master) yreq

getMoocHomeR :: Handler TypedContent
getMoocHomeR  = toTypedContent <$> do
    setUltDestCurrent

--    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    fullLayout Nothing "Welcome to QUA-KIT!" $ do
        setTitle "qua-kit"
        $(widgetFile "mooc/home")


