-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- The executable is intedent for importing data from GeoNames tables.
-- Excert:
-- http://download.geonames.org/export/dump/readme.txt
-- This work is licensed under a Creative Commons Attribution 3.0 License,
-- see http://creativecommons.org/licenses/by/3.0/
-- The Data is provided "as is" without warranty or any representation of accuracy,
-- timeliness or completeness.
-----------------------------------------------------------------------------

module Main
    ( main
    ) where


import Database.Persist.Sql
import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Binary as CB
import System.IO
import System.Environment

import Config
import Model
import InsertCountries
import InsertPlaces


-- to test performance, use
-- ghc-options: -O2 -threaded -fprof-auto "-with-rtsopts=-N -p -s -h -i0.1"
-- cabal clean
-- cabal configure --enable-profiling
-- cabal build location-import
-- cabal run location-import -- locations/cities1000.txt
-- hp2ps -c -e9in location-import.hp

main :: IO ()
main = do
  fname:_ <- getArgs
  withFile fname ReadMode $ \resHandle -> do
    pool <- createPoolConfig persistConfig
    flip runSqlPool pool $ do
      -- create tables
      runMigration migrateAll
      -- import country names
      countries <- insertCountries
      -- declare a conduit source base on supplied file
      -- to read it line-by-line
      let src = CB.sourceHandleUnsafe resHandle $= CT.decode CT.utf8 $= CT.lines
      -- import places
      insertPlaces countries src
  putStrLn "Finished import of places"
