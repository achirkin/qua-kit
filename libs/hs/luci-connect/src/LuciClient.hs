-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Main (main) where

import qualified Data.Conduit.Network as Network
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import Data.Monoid ((<>))

import Data.Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Conduit.Binary as ConB
import Data.Conduit.ByteString.Builder
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import qualified Data.Binary.Get as Binary

import Luci.Messages

import Debug.Trace
import Data.Void (Void)

main :: IO ()
main = do
    putStrLn "Hello World"
    result <- Network.runTCPClient connSettings luciSession
    putStrLn result
  where
    connSettings = Network.clientSettings 7654 "129.132.6.33"

luciSession :: Network.AppData -> IO String
luciSession appdata = do
--    x <- runConduit $ yield "hello"
    putStrLn $ "Remote address is " ++ show (Network.appSockAddr appdata)
    case Network.appLocalAddr appdata of
      Nothing -> putStrLn "Could not get local address"
      Just ad -> putStrLn $ "Local address is " ++ show ad
    runConduit $ yield regMsg $$ builderToByteString =$= outgoing
    putStrLn "Sent request!"
    rez <- runExceptT $ runConduit $ inPipe
    case rez of
      (Left err) -> putStrLn err
      (Right (val, _)) -> putStrLn "Success!" >> print (val :: JSON.Value)
    return "Good!"
  where
    regMsg = constructMessage regMsg' []
    regMsg' = RemoteRegister
        { exampleCall = JSON.object []
        , serviceName = "CoolTestService"
        , inputs  = Nothing
        , outputs = Nothing
        }
    outgoing = Network.appSink appdata
    incoming :: Producer (ExceptT String IO) ByteString
    incoming = Network.appSource appdata
    inPipe :: ConduitM () Void (ExceptT String IO) (JSON.Value, [ByteString])
    inPipe = incoming =$= parseMessage

-- | Encode a luci protocol message.
--   Structure:
--     8 bytes: BE long int - X - bytesize of header, which is a json message;
--     8 bytes: BE long int - N - number of attachments;
--     X bytes: UTF-8 encoded json
--     repeat N times:
--       8 bytes: BE long int - Yi - bytesize of attachment
--       Y bytes: arbitrary ByteString.
constructMessage :: JSON.ToJSON a
                 => a
                 -> [ByteString]
                 -> BS.Builder
constructMessage msg atts = BS.int64BE (fromIntegral $ BSL.length mainpart)
                         <> BS.int64BE (fromIntegral $ length atts)
                         <> BS.lazyByteString mainpart
                         <> mconcat (map constrAtt atts)
  where
    mainpart = JSON.encode $ JSON.toJSON msg
    constrAtt bs = BS.int64BE (fromIntegral $ BS.length bs) <> BS.byteString bs

-- | Decode a luci protocol message.
--   Structure:
--     8 bytes: BE long int - X - bytesize of header, which is a json message;
--     8 bytes: BE long int - N - number of attachments;
--     X bytes: UTF-8 encoded json
--     repeat N times:
--       8 bytes: BE long int - Yi - byte
parseMessage :: (JSON.FromJSON a, Monad m)
             => Consumer ByteString (ExceptT String m) (a, [ByteString])
parseMessage = do
    hSize <- ConB.take 8 >>= tryGet Binary.getInt64be
    hANum <- ConB.take 8 >>= tryGet Binary.getInt64be
    emsg  <- ConB.take (fromIntegral hSize) >>= tryDecode
    atts <- mapM (const tryGetAttachment) [1..hANum]
    return (emsg, atts)
  where
    -- wrapper around Binary's runGetOrFail
    tryGet x bs = lift $ case Binary.runGetOrFail x bs of
                 Left  (_,_,msg) -> throwE msg
                 Right (_,_,val) -> return val
    -- wrapper around Aeson's decode
    tryDecode bs = lift $ case JSON.eitherDecode' bs of
                 Left  msg -> throwE msg
                 Right val -> return val
    tryGetAttachment = do
      hSize <- ConB.take 8 >>= tryGet Binary.getInt64be
      BSL.toStrict <$> ConB.take (fromIntegral hSize)
