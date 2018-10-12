{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Mooc.HangxinServiceRedirect
  ( getHSR
  , getHSFiles1R, getHSFiles2R, getHSFiles3R, getHSFiles4R, getHSFiles5R) where


import Import
import Network.HTTP.Conduit as HTTP
import Database.Persist.Sql (fromSqlKey)
import Text.Blaze
import Data.CaseInsensitive

baseAddr :: String
baseAddr = "http://192.168.137.102:5000/" -- "http://129.132.32.134/"

getHSR :: ExerciseId -> UserId -> Handler Html
getHSR exId uId = do
    er <- simpleHttp addr
    return $ unsafeLazyByteString er
  where
    addr = baseAddr
           <> "service?url=https://qua-kit.ethz.ch/exercise/"
           <> show (fromSqlKey exId) <> "/"
           <> show (fromSqlKey uId) <> "/geometry"

getHSFiles1R :: ExerciseId -> Text -> Handler TypedContent
getHSFiles1R _ t = do
    req <- parseRequest addr
    resp <- Import.httpLbs req
    let hdrs = responseHeaders resp
        ct = fromMaybe "text/html" $ snd <$> find (("Content-Type" ==) . fst) hdrs
    mapM_ (\(n, v) -> addHeader
                        (decodeUtf8 $ original n)
                        (decodeUtf8 v)
           ) hdrs
    sendResponse (ct, toContent $ responseBody resp)
  where
    addr = baseAddr <> unpack t

getHSFiles2R :: ExerciseId
             -> Text -> Text -> Handler TypedContent
getHSFiles2R e t1 t2 = getHSFiles1R e (t1 <> "/" <> t2)

getHSFiles3R :: ExerciseId
             -> Text -> Text -> Text-> Handler TypedContent
getHSFiles3R e t1 t2 t3 = getHSFiles2R e t1 (t2 <> "/" <> t3)

getHSFiles4R :: ExerciseId
             -> Text -> Text -> Text -> Text -> Handler TypedContent
getHSFiles4R e t1 t2 t3 t4 = getHSFiles3R e t1 t2 (t3 <> "/" <> t4)

getHSFiles5R :: ExerciseId
             -> Text -> Text -> Text -> Text -> Text -> Handler TypedContent
getHSFiles5R e t1 t2 t3 t4 t5 = getHSFiles4R e t1 t2 t3 (t4 <> "/" <> t5)
