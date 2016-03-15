-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation.OAuth
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation.OAuth where

import Control.Arrow ((***))
--import Control.Concurrent.STM
--import Control.Exception (Exception, throwIO)
--import Control.Monad (liftM)
--import Data.ByteString.Lazy (ByteString)
--import Data.ByteString (ByteString)
--import Data.Default
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as Text
--import Database.Persist.Sql
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
--import Network.HTTP.Client.Conduit (Manager)
--import Text.Hamlet
import Yesod
import Yesod.Auth
--import Yesod.Auth.OAuth
import Web.Authenticate.OAuth
--import Yesod.Default.Util

import Data.Maybe (fromMaybe)

mkExtractCreds :: YesodAuth m => String -> Credential -> IO (Creds m)
mkExtractCreds idName (Credential dic) = do
  let mcrId = decodeUtf8With lenientDecode <$> lookup (encodeUtf8 $ Text.pack idName) dic
  case mcrId of
    Just crId -> return $ Creds "testOAuth" crId
        $ map (decodeUtf8With lenientDecode *** decodeUtf8With lenientDecode) dic
    Nothing -> return $ Creds "testOAuth" "00001111"
        $ map (decodeUtf8With lenientDecode *** decodeUtf8With lenientDecode) dic

    --throwIO $ MyCredentialError ("key not found: " ++ idName) (Credential dic)

--
--data MyAuthException = MyCredentialError String Credential
--                         | MySessionError String
--                           deriving (Show)

--instance Exception MyAuthException

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

authOAuth :: YesodAuth m
          => OAuth                        -- ^ 'OAuth' data-type for signing.
          -> (Credential -> IO (Creds m)) -- ^ How to extract ident.
          -> AuthPlugin m
authOAuth oauth mkCreds = AuthPlugin name dispatch login
  where
    name = Text.pack $ oauthServerName oauth
    url = PluginR name []
    lookupTokenSecret = decodeUtf8With lenientDecode . fromMaybe "" . lookup "oauth_token_secret" . unCredential
    oauthSessionName = "_ID1"

    dispatch "GET" ["forward"] =  do
        render <- lift getUrlRender
        tm <- getRouteToParent
--        liftIO $ print $ encodeUtf8 $ render $ tm url
        let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
        master <- lift getYesod
        tok <- lift $ getTemporaryCredential oauth' (authHttpManager master)
--        liftIO $ print $ tok
        setSession oauthSessionName $ lookupTokenSecret tok
--        liftIO $ print "finished"
        redirect $ authorizeUrl oauth' tok
    dispatch "GET" [] = lift $ do
      Just tokSec <- lookupSession oauthSessionName
      deleteSession oauthSessionName
      reqTok <-
        if oauthVersion oauth == OAuth10
          then do
            oaTok  <- runInputGet $ ireq textField "oauth_token"
            return $ Credential [ ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
          else do
            (verifier, oaTok) <-
                runInputGet $ (,) <$> ireq textField "oauth_verifier"
                                  <*> ireq textField "oauth_token"
            return $ Credential [ ("oauth_verifier", encodeUtf8 verifier)
                                , ("oauth_token", encodeUtf8 oaTok)
                                , ("oauth_token_secret", encodeUtf8 tokSec)
                                ]
      master <- getYesod
      accTok <- getAccessToken oauth reqTok (authHttpManager master)
      creds  <- liftIO $ mkCreds accTok
      setCredsRedirect creds
    dispatch _ _ = notFound

    login tm = do
        render <- getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        [whamlet| <a href=#{oaUrl}>Login via #{name} |]
