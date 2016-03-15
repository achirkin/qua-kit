-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.Trans.Resource
--import qualified Data.IntMap as IntMap
import Database.Persist.Sql
import Network.HTTP.Client.Conduit (newManager)
import Web.Authenticate.OAuth
import Yesod

import Config
import Dispatch ()
import Model (migrateAll)
import Foundation

main :: IO ()
main = do
    man <- newManager
    pool <- createPoolConfig persistConfig
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool
        $ runMigration migrateAll
    warpEnv App
        { connPool = pool
        , httpManager = man
        , oauthData = oauth
        }
    where oauth = newOAuth
            { oauthServerName      = "testOAuth"
            , oauthRequestUri      = "http://term.ie/oauth/example/request_token.php"
            , oauthAccessTokenUri  = "http://term.ie/oauth/example/access_token.php"
--            , oauthAuthorizeUri    = "http://term.ie/oauth/example/echo_api.php"
            , oauthAuthorizeUri    = "/auth/page/testOAuth"
            , oauthSignatureMethod = HMACSHA1
            , oauthConsumerKey     = "key"
            , oauthConsumerSecret  = "secret"
            , oauthVersion         = OAuth10
            , oauthCallback        = Just "/"
            }

mycred :: Credential
mycred = newCredential "your access token here"
                       "your access token secret here"

--pk = "-----BEGIN PRIVATE KEY-----\
--    \ MIICdgIBADANBgkqhkiG9w0BAQEFAASCAmAwggJcAgEAAoGBALRiMLAh9iimur8V\
--    \ A7qVvdqxevEuUkW4K+2KdMXmnQbG9Aa7k7eBjK1S+0LYmVjPKlJGNXHDGuy5Fw/d\
--    \ 7rjVJ0BLB+ubPK8iA/Tw3hLQgXMRRGRXXCn8ikfuQfjUS1uZSatdLB81mydBETlJ\
--    \ hI6GH4twrbDJCR2Bwy/XWXgqgGRzAgMBAAECgYBYWVtleUzavkbrPjy0T5FMou8H\
--    \ X9u2AC2ry8vD/l7cqedtwMPp9k7TubgNFo+NGvKsl2ynyprOZR1xjQ7WgrgVB+mm\
--    \ uScOM/5HVceFuGRDhYTCObE+y1kxRloNYXnx3ei1zbeYLPCHdhxRYW7T0qcynNmw\
--    \ rn05/KO2RLjgQNalsQJBANeA3Q4Nugqy4QBUCEC09SqylT2K9FrrItqL2QKc9v0Z\
--    \ zO2uwllCbg0dwpVuYPYXYvikNHHg+aCWF+VXsb9rpPsCQQDWR9TT4ORdzoj+Nccn\
--    \ qkMsDmzt0EfNaAOwHOmVJ2RVBspPcxt5iN4HI7HNeG6U5YsFBb+/GZbgfBT3kpNG\
--    \ WPTpAkBI+gFhjfJvRw38n3g/+UeAkwMI2TJQS4n8+hid0uus3/zOjDySH3XHCUno\
--    \ cn1xOJAyZODBo47E+67R4jV1/gzbAkEAklJaspRPXP877NssM5nAZMU0/O/NGCZ+\
--    \ 3jPgDUno6WbJn5cqm8MqWhW1xGkImgRk+fkDBquiq4gPiT898jusgQJAd5Zrr6Q8\
--    \ AO/0isr/3aa6O6NLQxISLKcPDk2NOccAfS/xOtfOz4sJYM3+Bs4Io9+dZGSDCA54\
--    \ Lw03eHTNQghS0A==\
--    \ -----END PRIVATE KEY-----"
