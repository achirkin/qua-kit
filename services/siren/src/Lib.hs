{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , connectPostgres
    ) where

-- import           Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Database.PostgreSQL.LibPQ

someFunc :: IO ()
someFunc = Prelude.putStrLn "someFunc"


connectPostgres :: IO ()
connectPostgres = do
  -- connect
  conn <- connectdb "postgresql://siren:sirenpass@localhost:5432/sirendb"

  -- execute one function
  Just rez <- exec conn "SELECT list_scenarios();"
  -- print results
  resultStatus rez >>= print
  resultErrorMessage rez >>= mapM_ BSC.putStrLn
  -- get row 0 column 0 from result
  getvalue rez 0 0 >>= mapM_ BSC.putStrLn

  -- execute one more function
  Just rez2 <- execParams conn "SELECT get_scenario($1);" [Just (Oid 20, "3", Text)] Binary
  -- print results
  resultStatus rez2 >>= print
  resultErrorMessage rez2 >>= mapM_ BSC.putStrLn
  -- get row 0 column 0 from result
  getvalue rez2 0 0 >>= mapM_ BSC.putStrLn

  -- disconnect
  finish conn

-- Oids:
--  bigint = int8 = 20
--  jsonb = 3802
--  json  = 114
--  text  = 25
