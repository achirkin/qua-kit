-----------------------------------------------------------------------------
-- |
-- Module      :  InsertPlaces
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Import places from a tab-separated text file provided by GeoNames
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module InsertPlaces
    ( insertPlaces
    ) where



import Data.Conduit
import qualified Data.Conduit.List as CL

import Database.Persist
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

-- The data format is tab-delimited text in utf8 encoding.
--
-- I plan to load one of these:
-- allCountries.zip         : all countries combined in one file, see 'geoname' table for columns
-- cities1000.zip           : all cities with a population > 1000 or seats of adm div (ca 80.000), see 'geoname' table for columns
-- cities5000.zip           : all cities with a population > 5000 or PPLA (ca 40.000), see 'geoname' table for columns
-- cities15000.zip          : all cities with a population > 15000 or capitals (ca 20.000), see 'geoname' table for columns


import Model

type DBEnv = ReaderT (PersistEntityBackend Place) IO

insertPlaces :: Map Text CountryId
             -> Source DBEnv Text
             -> DBEnv ()
insertPlaces countries src = do
    -- clean table first
    deleteWhere ([] :: [Filter Place])
    -- run line-by-line processing, discarding all values
    src $$ dst
  where
    fetchPlace   = parsePlace (lookupCountry countries)

    react :: Either String Place -> DBEnv ()
    react (Left err) = liftIO $ putStrLn err
    react (Right place) = insert_ place

    processLine :: Text -> DBEnv ()
    processLine = react . fetchPlace

    dst :: Sink Text DBEnv ()
    dst = CL.mapM_ processLine


parsePlace :: (Text -> Either String CountryId)
           -> Text
           -> Either String Place
parsePlace fetchCountry t = if n /= 19
  then Left $ "A row has incorrect number of fields "
            ++ "(" ++ show n ++ ")"
            ++ ":\n\t" ++ Text.unpack t
  else case tcountryId of
   Right countryId -> Right Place
    { placeGeonameId      = read . Text.unpack $ fields !! 0
    , placeCountry        = countryId
    , placeName           = fields !! 1
    , placeAsciiName      = fields !! 2
    , placeAlternateNames = fields !! 3
    , placeLatitude       = read . Text.unpack $ fields !! 4
    , placeLongitude      = read . Text.unpack $ fields !! 5
    }
   Left err -> Left $ err ++ "\n\t" ++ Text.unpack t
  where fields = Text.split ('\t' ==) t
        n = length fields
        tcountryId = fetchCountry $ fields !! 8


---- | The main 'geoname' table has the following fields :
--header :: [Text]
--header =
--  [ "geonameid"         -- 0 integer id of record in geonames database
--  , "name"              -- 1 name of geographical point (utf8) varchar(200)
--  , "asciiname"         -- 2 name of geographical point in plain ascii characters, varchar(200)
--  , "alternatenames"    -- 3 alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
--  , "latitude"          -- 4 latitude in decimal degrees (wgs84)
--  , "longitude"         -- 5 longitude in decimal degrees (wgs84)
--  , "feature class"     -- 6 see http://www.geonames.org/export/codes.html, char(1)
--  , "feature code"      -- 7 see http://www.geonames.org/export/codes.html, varchar(10)
--  , "country code"      -- 8 ISO-3166 2-letter country code, 2 characters
--  , "cc2"               -- 9 alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
--  , "admin1 code"       -- 10 fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
--  , "admin2 code"       -- 11 code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
--  , "admin3 code"       -- 12 code for third level administrative division, varchar(20)
--  , "admin4 code"       -- 13 code for fourth level administrative division, varchar(20)
--  , "population"        -- 14 bigint (8 byte int)
--  , "elevation"         -- 15 in meters, integer
--  , "dem"               -- 16 digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
--  , "timezone"          -- 17 the timezone id (see file timeZone.txt) varchar(40)
--  , "modification date" -- 18 date of last modification in yyyy-MM-dd format
--  ]


lookupCountry :: Map Text CountryId -> Text -> Either String CountryId
lookupCountry m t = case Map.lookup t m of
    Nothing -> Left $ "Failed to lookup ISO country code: " ++ Text.unpack t
    Just c -> Right c

