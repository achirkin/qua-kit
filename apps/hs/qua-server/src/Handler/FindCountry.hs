-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.FindCountry
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Handler.FindCountry
    ( getFindCountryR
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

getFindCountryR :: Handler TypedContent
getFindCountryR = do
    -- query string
    mquery <- maybeQuery <$> lookupGetParam "q"
    -- maximum number of elements to return in JSON result
    n      <- maybeLimit <$> lookupGetParam "n"
    case mquery of
      Nothing            -> return $ TypedContent typeJson "[]"
      Just (query, qlen) -> respondSourceDB typeJson $ do
        countryQuery n query qlen
                   $= CL.map (toFlushBuilder . Aeson.encodeToBuilder . toJSON . parseDBCountry)
                  =$= streamJSONArray


countryQuery :: PersistValue
             -> PersistValue
             -> Int
             -> Source (YesodDB App) [PersistValue]
countryQuery _ _     0 = mempty
countryQuery n query 1 = rawQuery sql1 [query,n]
countryQuery n query 2 = rawQuery sql2 [query,query,n]
countryQuery n query _ = rawQuery sqlx [query,query,query,query,n]

sql1 :: Text
sql1 = "SELECT id,iso,name FROM country WHERE LOWER(iso) LIKE ?||'%' ORDER BY name ASC LIMIT ?"

sql2 :: Text
sql2 = "SELECT id,iso,name FROM country WHERE iso = ? OR LOWER(name) LIKE ?||'%' ORDER BY name ASC LIMIT ?"

sqlx :: Text
sqlx = "SELECT id,iso,name FROM country WHERE LOWER(name) LIKE ?||'%' OR LOWER(name) LIKE '%'||?||'%' \
       \ORDER BY case\n\
       \  WHEN LOWER(name) LIKE ?||'%' THEN 1\n\
       \  WHEN LOWER(name) LIKE '%'||?||'%' THEN 2\n\
       \  ELSE 3\n\
       \end,name ASC \
       \LIMIT ?"

-- utilities

parseDBCountry :: [PersistValue] -> (Int,Text,Text)
parseDBCountry (PersistInt64 i : PersistText iso : PersistText name : _)
                 = (fromIntegral i, iso, name)
parseDBCountry _ = (0, "", "")

maybeLimit :: Maybe Text -> PersistValue
maybeLimit Nothing = toPersistValue ndef
maybeLimit (Just m) = case Text.decimal m of
    Left _ -> toPersistValue ndef
    Right (n,_) -> toPersistValue $ min nmax n

maybeQuery :: Maybe Text -> Maybe (PersistValue, Int)
maybeQuery Nothing = Nothing
maybeQuery (Just q) = Just (PersistText $ Text.toLower q, Text.length q)


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
