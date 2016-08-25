-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.EdxLogin
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE Rank2Types#-}
module Handler.Mooc.EdxLogin
  ( authLtiPlugin
  ) where

import Import.NoFoundation
import Web.LTI

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map.Strict as Map

pluginName :: Text
pluginName = "lti"

authLtiPlugin :: (Yesod m, YesodAuth m) => LTIProvider -> AuthPlugin m
authLtiPlugin conf =
  AuthPlugin pluginName (dispatch conf) $ \_tp -> return ()

-- | This creates a page with url
--   root.root/auth/page/lti/login
dispatch :: (RenderMessage site FormMessage) => LTIProvider -> Text -> [Text] -> AuthHandler site TypedContent
dispatch conf "POST" ["login"] = do
    yreq <- getRequest
    eltiRequest <- runExceptT $ processYesodRequest conf yreq
    case eltiRequest of
      Left (LTIException err) -> permissionDenied $ Text.pack err
      Left (LTIOAuthException err) -> permissionDenied $ Text.pack $ show err
      Right msg -> do
        user_id                 <- lookupParam msg "user_id"
        resource_link_id        <- lookupParam msg "resource_link_id"
        context_id              <- lookupParam msg "context_id"
        lis_outcome_service_url <- lookupParam msg "lis_outcome_service_url"
        lis_result_sourcedid    <- lookupParam msg "lis_result_sourcedid"
        -- set LTI credentials
        lift . setCredsRedirect
             . Creds pluginName user_id
             $ ("resource_link_id"       , resource_link_id)
             : ("context_id"             , context_id)
             : ("lis_outcome_service_url", lis_outcome_service_url)
             : ("lis_result_sourcedid"   , lis_result_sourcedid)
             : saveCustomParams (Map.toList msg)
  where
    -- try to get essential edxParameters
    lookupParam msg p = case Map.lookup p msg of
                        Just v -> return $ Text.decodeUtf8 v
                        Nothing -> permissionDenied $ "Cannot access request parameter " <> Text.decodeUtf8 p
    -- store all special parameters in user session
    saveCustomParams [] = []
    saveCustomParams ((k,v):xs) = if "custom_" `isPrefixOf` k
               then (Text.decodeUtf8 k, Text.decodeUtf8 v) : saveCustomParams xs
               else saveCustomParams xs
dispatch _ _ _                 = notFound

