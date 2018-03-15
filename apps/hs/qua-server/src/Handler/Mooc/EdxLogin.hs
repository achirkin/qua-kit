{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE Rank2Types#-}
module Handler.Mooc.EdxLogin
  ( authLtiPlugin, dispatchLti
  ) where

import Import.NoFoundation
import Model.Session
import Web.LTI

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map.Strict as Map

pluginName :: Text
pluginName = "lti"

authLtiPlugin :: (Yesod m, YesodAuth m) => LTIProvider -> AuthPlugin m
authLtiPlugin conf =
  AuthPlugin pluginName (dispatchAuth conf) $ \_tp -> return ()

-- | This creates a page with url
--   root.root/auth/page/lti/login
dispatchAuth :: (RenderMessage site FormMessage) => LTIProvider -> Text -> [Text] -> AuthHandler site TypedContent
dispatchAuth conf "POST" ["login"] = getRequest >>= lift . dispatchLti conf
dispatchAuth _ _ _                 = notFound



-- | This creates a page with url
--   root.root/auth/page/lti/login
dispatchLti :: (RenderMessage site FormMessage, YesodAuth site)
            => LTIProvider
            -> YesodRequest
            -> HandlerT site IO TypedContent
dispatchLti conf yreq = do
    clearSession
    eltiRequest <- runExceptT $ processYesodRequest conf yreq
    case eltiRequest of
      Left (LTIException err) -> permissionDenied $ Text.pack err
      Left (LTIOAuthException err) -> permissionDenied $ Text.pack $ show err
      Right msg -> do
        let linkIdName     = convKey userSessionEdxResourceLinkId
            contextIdName  = convKey userSessionEdxContextId
            serviceUrlName = convKey userSessionEdxLisOutcomeServiceUrl
            sourcedIdName  = convKey userSessionEdxLisResultSourcedId
        user_id                 <- lookupParam  msg "user_id"
        resource_link_id        <- lookupParam  msg linkIdName
        context_id              <- lookupParam  msg contextIdName
        lis_outcome_service_url <- lookupParamM msg serviceUrlName
        lis_result_sourcedid    <- lookupParamM msg sourcedIdName
        -- set LTI credentials
        setCredsRedirect
             . Creds pluginName user_id
             $ (linkIdName, resource_link_id)
             : (contextIdName, context_id)
             : catMaybes
                [ (,) serviceUrlName <$> lis_outcome_service_url
                , (,) sourcedIdName  <$> lis_result_sourcedid
                ]
             ++ saveCustomParams (Map.toList msg)
  where
    -- try to get essential edxParameters
    lookupParam msg p = case Map.lookup (Text.encodeUtf8 p) msg of
                        Just v -> return $ Text.decodeUtf8 v
                        Nothing -> permissionDenied $ "Cannot access request parameter " <> p
    -- try to get optional edxParameters
    lookupParamM msg p = return $ Text.decodeUtf8 <$> Map.lookup (Text.encodeUtf8 p) msg
    -- store all special parameters in user session
    saveCustomParams [] = []
    saveCustomParams ((k,v):xs) = if "custom_" `isPrefixOf` k
               then (Text.decodeUtf8 k, Text.decodeUtf8 v) : saveCustomParams xs
               else saveCustomParams xs
