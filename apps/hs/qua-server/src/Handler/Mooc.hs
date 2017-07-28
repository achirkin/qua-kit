{-# OPTIONS_HADDOCK hide, prune #-}
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
        (Just u) <- runDB $ get userId
        return $ if isNothing $ userEmail u
                 then setupLocalAccountW userId
                 else mempty

--    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    fullLayout Nothing "Welcome to QUA-KIT!" $ do
        setTitle "qua-kit"
        toWidgetHead
          [hamlet|
            <meta property="og:url"         content="@{MoocHomeR}" />
            <meta property="og:type"        content="website" />
            <meta property="og:title"       content="Quick Urban Analysis kit" />
            <meta property="og:description" content="Qua-kit is an urban design, education, sharing, and analysis platform." />
            <meta property="og:image"       content="@{StaticR img_bgimg_png}" />
          |]
        $(widgetFile "mooc/home")
