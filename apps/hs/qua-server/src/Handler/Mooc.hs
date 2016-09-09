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
import Handler.Mooc.User


postMoocHomeR :: Handler TypedContent
postMoocHomeR = do
  master <- getYesod
  yreq <- getRequest
  dispatchLti (appLTICredentials $ appSettings master) yreq

getMoocHomeR :: Handler TypedContent
getMoocHomeR  = toTypedContent <$> do
    setUltDestCurrent
    muserId <- maybeAuthId
    createAccW <- case muserId of
      Nothing -> return mempty
      Just userId -> do
        unn <- runDB $ getBy $ UserProperty userId "username"
        return $ if isNothing unn
                 then setupLocalAccountW userId
                 else mempty

--    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    fullLayout Nothing "Welcome to QUA-KIT!" $ do
        setTitle "qua-kit"
        $(widgetFile "mooc/home")


