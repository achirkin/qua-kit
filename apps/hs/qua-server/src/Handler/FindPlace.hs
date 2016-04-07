-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.FindPlace
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Handler.FindPlace
    ( getFindPlaceR
    ) where

import Data.Conduit
import Database.Persist.Sql
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Blaze.ByteString.Builder (Builder)
import Data.Aeson.Encode as Aeson
import Yesod

import Foundation

-- | Default number of results to return
ndef :: Int
ndef = 10

-- | Maximum number of results to return
nmax :: Int
nmax = 100

getFindPlaceR :: Handler TypedContent
getFindPlaceR = do
    -- country id
    mcid   <- maybeId <$> lookupGetParam "c"
    -- query string
    mquery <- maybeQuery <$> lookupGetParam "q"
    -- maximum number of elements to return in JSON result
    n      <- maybeLimit <$> lookupGetParam "n"
    case (,) <$> mcid <*> mquery of
      Nothing                   -> return $ TypedContent typeJson "[]"
      Just (cid, (query, qlen)) -> respondSourceDB typeJson $ do
        placeQuery cid n query qlen
                   $= CL.map (toFlushBuilder . Aeson.encodeToBuilder . toJSON . parseDBPlace)
                  =$= streamJSONArray


placeQuery :: PersistValue
           -> PersistValue
           -> PersistValue
           -> Int
           -> Source (YesodDB App) [PersistValue]
placeQuery c n q l | l <= 0    = rawQuery sql0 [c,n]
                   | l <= 3    = rawQuery sql3 [c,q,n]
                   | otherwise = rawQuery sqlx [c,q,q,q,q,n]


sql0 :: Text
sql0 = "SELECT id,ascii_name FROM place WHERE country = ? ORDER BY ascii_name ASC LIMIT ?"

sql3 :: Text
sql3 = "SELECT id,ascii_name FROM place WHERE country = ? AND ascii_name LIKE ?||'%' \
       \ORDER BY ascii_name ASC LIMIT ?"

sqlx :: Text
sqlx = "SELECT id,ascii_name FROM place WHERE country = ? AND \
       \(ascii_name LIKE ?||'%' OR ascii_name LIKE '%'||?||'%') \
       \ORDER BY case\n\
       \  WHEN ascii_name LIKE ?||'%' THEN 1\n\
       \  WHEN ascii_name LIKE '%'||?||'%' THEN 2\n\
       \  ELSE 3\n\
       \end,ascii_name ASC \
       \LIMIT ?"

-- utilities

parseDBPlace :: [PersistValue] -> (Int,Text)
parseDBPlace (PersistInt64 i : PersistText name : _)
               = (fromIntegral i, name)
parseDBPlace _ = (0, "")

maybeLimit :: Maybe Text -> PersistValue
maybeLimit Nothing = toPersistValue ndef
maybeLimit (Just m) = case Text.decimal m of
    Left _ -> toPersistValue ndef
    Right (n,_) -> toPersistValue $ min nmax n

maybeQuery :: Maybe Text -> Maybe (PersistValue, Int)
maybeQuery Nothing = Nothing
maybeQuery (Just q) = Just (PersistText q, Text.length q)

maybeId :: Maybe Text -> Maybe (PersistValue)
maybeId Nothing = Nothing
maybeId (Just m) = case Text.decimal m of
    Left _ -> Nothing
    Right (n,_) -> Just $ PersistInt64 n


streamJSONArray :: Monad m
                => Conduit (Flush Builder) m (Flush Builder)
streamJSONArray = encodeEntryStart
  where
    encodeEntryStart = await >>= sendEntryStart
    encodeEntry  = await >>= sendEntry

    sendEntryStart (Just val) = sendChunkBS "["  >> yield val >> encodeEntry
    sendEntryStart Nothing    = sendChunkBS "[]" >> sendFlush

    sendEntry (Just val) = sendChunkBS "," >> yield val >> encodeEntry
    sendEntry Nothing    = sendChunkBS "]" >> sendFlush
