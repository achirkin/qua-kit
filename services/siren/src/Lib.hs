{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lib
    ( connectPostgres
    ) where

import           Control.Monad (when)
import           Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.FileEmbed
import           Database.PostgreSQL.LibPQ


connectPostgres :: IO ()
connectPostgres = do
  -- connect
  conn <- connectdb "postgresql://siren:sirenpass@localhost:5432/sirendb"

  -- make sure all functions exist
  mapM_ (exec conn) sqlFunDefs

  -- make sure database is here
  _ <- exec conn "SELECT populatedb();"

  -- execute one function
  Just rez <- exec conn "SELECT list_scenarios();"
  -- print results
  resultStatus rez >>= print
  resultErrorMessage rez >>= mapM_ BSC.putStrLn
  nrow <- ntuples rez
  ncol <- nfields rez
  -- get row 0 column 0 from result
  when (nrow > 0 && ncol > 0) $
    getvalue rez 0 0 >>= mapM_ BSC.putStrLn

  -- execute one more function
  Just rez2 <- execParams conn "SELECT get_scenario($1);" [Just (Oid 20, "3", Text)] Text
  -- print results
  resultStatus rez2 >>= print
  resultErrorMessage rez2 >>= mapM_ BSC.putStrLn
  nrow2 <- ntuples rez2
  ncol2 <- nfields rez2
  -- get row 0 column 0 from result
  when (nrow2 > 0 && ncol2 > 0) $
    getvalue rez2 0 0 >>= mapM_ BSC.putStrLn

  -- disconnect
  finish conn
  BSC.putStrLn "Execution success"

-- Oids:
--  bigint = int8 = 20
--  jsonb = 3802
--  json  = 114
--  text  = 25

-- | All sql functions definitions
sqlFunDefs :: [BS.ByteString]
sqlFunDefs =
  [ createScenarioF
  , deleteScenarioF
  , listScenariosF
  , getScenarioF
  , recoverScenarioF
  , updateScenarioF
  , populateDBF
  , dropDBF
  ]

createScenarioF :: BS.ByteString
createScenarioF = $(embedFile "sql/create_scenario.sql")

deleteScenarioF :: BS.ByteString
deleteScenarioF = $(embedFile "sql/delete_scenario.sql")

listScenariosF :: BS.ByteString
listScenariosF = $(embedFile "sql/list_scenarios.sql")

getScenarioF :: BS.ByteString
getScenarioF = $(embedFile "sql/get_scenario.sql")

recoverScenarioF :: BS.ByteString
recoverScenarioF = $(embedFile "sql/recover_scenario.sql")

updateScenarioF :: BS.ByteString
updateScenarioF = $(embedFile "sql/update_scenario.sql")



populateDBF :: BS.ByteString
populateDBF = $(embedFile "sql/populatedb.sql")

dropDBF :: BS.ByteString
dropDBF = $(embedFile "sql/dropdb.sql")
