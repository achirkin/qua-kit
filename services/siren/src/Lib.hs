{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib
    ( PSSettings (..), Connection
    , withPostgres
    , createScenario, updateScenario
    , deleteScenario, recoverScenario
    , getScenario, listScenarios
    ) where

import           Data.ByteString           (ByteString)
import           Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.Char
import           Data.FileEmbed
import           Data.Int
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Database.PostgreSQL.LibPQ

data PSSettings = PSSettings
  { uName  :: !ByteString
  , uPass  :: !ByteString
  , dbHost :: !ByteString
  , dbPort :: !Int
  , dbName :: !ByteString
  }

-- | Establish a connection with PostgreSQL and run some commands
withPostgres :: PSSettings -> (Connection -> IO ()) -> IO ()
withPostgres PSSettings {..} commands = do
  -- connect
  conn <- connectdb $
      "postgresql://" <> uName <> ":"
                      <> uPass <> "@"
                      <> dbHost <> ":"
                      <> BSC.pack (show dbPort) <> "/"
                      <> dbName
  -- make sure all functions exist
  mapM_ (\s -> exec conn s >>= flip justResult (`checkResult` id) >>= print) sqlFunDefs
  -- make sure database is here
  populateDB conn >>= \erez -> case erez of
    Right () -> return ()
    Left err -> BSC.putStrLn err

  -- do something with the connection
  commands conn

  -- disconnect
  finish conn



oidBIGINT :: Oid
oidBIGINT = Oid 20

oidJSONB :: Oid
oidJSONB = Oid 3802

-- oidJSON :: Oid
-- oidJSON = Oid 114

oidTEXT :: Oid
oidTEXT = Oid 25

oidNUMERIC :: Oid
oidNUMERIC = Oid 1700

mkNum :: (Show a, Num a) => a -> Maybe (Oid, ByteString, Format)
mkNum i = Just (oidNUMERIC, BSC.pack (show i), Text)

mkBigInt :: (Show a, Integral a) => a -> Maybe (Oid, ByteString, Format)
mkBigInt i = Just (oidBIGINT, BSC.pack (show i), Text)

mkToken :: Int64 -> Maybe (Oid, ByteString, Format)
mkToken i = Just (oidJSONB, BSC.pack (show i), Text)

createScenario :: Connection
               -> BS.ByteString -- ^ scenario name
               -> BS.ByteString -- ^ GeoJSON Feature Collection
               -> Maybe (Double, Double, Maybe Double) -- ^ lon/lat coords and altitude in metres, above sea level
               -> IO (Either BS.ByteString Int64) -- ^ Either error or ScID
createScenario conn scName scenario mlonlatalt = do
  mrez <- execParams conn "SELECT create_scenario($1,$2,$3,$4);"
    ( Just (oidTEXT, BSC.filter (\c -> isAlphaNum c || c == ' ') scName, Text)
    : Just (oidJSONB, scenario, Text)
    : case mlonlatalt of
        Nothing                   -> [Nothing, Nothing, Nothing]
        Just (lon, lat, Nothing)  -> [mkNum lon, mkNum lat, Nothing]
        Just (lon, lat, Just alt) -> [mkNum lon, mkNum lat, mkNum alt]
    )
    Text
  justResult mrez $ \rez -> checkResult rez (read . BSC.unpack)



deleteScenario :: Connection
               -> Int64 -- ^ ScID (scenario id)
               -> IO (Either BS.ByteString ()) -- ^ Either error or ()
deleteScenario conn scID = do
  mrez <- execParams conn "SELECT delete_scenario($1);" [mkBigInt scID] Text
  justResult mrez checkStatus


recoverScenario :: Connection
               -> Int64 -- ^ ScID (scenario id)
               -> IO (Either BS.ByteString ()) -- ^ Either error or ()
recoverScenario conn scID = do
  mrez <- execParams conn "SELECT recover_scenario($1);" [mkBigInt scID] Text
  justResult mrez checkStatus

updateScenario :: Connection
               -> Int64 -- ^ ScID (scenario id)
               -> BS.ByteString -- ^ GeoJSON Feature Collection
               -> IO (Either BS.ByteString ()) -- ^ Either error or ()
updateScenario conn scID scenario = do
  mrez <- execParams conn "SELECT update_scenario($1,$2);" [mkBigInt scID, Just (oidJSONB, scenario, Text)] Text
  justResult mrez checkStatus


listScenarios :: Connection
              -> Int64 -- ^ token (callID)
              -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json with a list of scenarios
listScenarios conn token = do
  mrez <- execParams conn "SELECT list_scenarios($1);" [mkToken token] Text
  justResult mrez $ \rez -> checkResult rez id

getScenario :: Connection
            -> Int64 -- ^ ScID (scenario id)
            -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or GeoJSON Feature Collection
getScenario conn scID = do
  mrez <- execParams conn "SELECT get_scenario($1);" [mkBigInt scID] Text
  justResult mrez $ \rez -> checkResult rez id

populateDB :: Connection
           -> IO (Either BS.ByteString ()) -- ^ Either error or ()
populateDB conn = do
  mrez <- exec conn "SELECT populatedb();"
  justResult mrez checkStatus



justResult :: Maybe Result -> (Result -> IO (Either ByteString a)) -> IO (Either ByteString a)
justResult Nothing _ = return $ Left "Failed to execute an SQL query for an unknown reason (result is Nothing)."
justResult (Just rez) f = f rez


checkStatus :: Result -> IO (Either ByteString ())
checkStatus rez = do
    rstatus <- resultStatus rez
    if rstatus == CommandOk || rstatus == TuplesOk
    then return $ Right ()
    else returnError rez rstatus

checkResult :: Result -> (ByteString -> a) -> IO (Either ByteString a)
checkResult rez f = do
  rstatus <- resultStatus rez
  nrow <- ntuples rez
  ncol <- nfields rez
  if nrow > 0 && ncol > 0 &&
     (rstatus == CommandOk || rstatus == TuplesOk)
  then getvalue rez 0 0 >>= \mv -> case mv of
     Just v -> return . Right $ f v
     Nothing -> return . Left $ "Could not read a value from DB, even though the status is ok."
  else returnError rez rstatus

returnError :: Result -> ExecStatus -> IO (Either ByteString a)
returnError rez rstatus = do
  statusText <- resStatus rstatus
  merror <- resultErrorMessage rez
  return . Left $ BSC.unlines
    [ statusText
    , fromMaybe "" merror
    ]


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
