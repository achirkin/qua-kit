{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Lib
    ( PSSettings (..), Connection, ScenarioId (..)
    , withPostgres
    , createScenario, updateScenario, copyScenario
    , deleteScenario, recoverScenario
    , getScenario, listScenarios
    , getLastScUpdates
    ) where

import qualified Data.Aeson                as JSON
import           Data.ByteString           (ByteString)
import           Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.Char
import           Data.FileEmbed
import           Data.Int
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Monoid               ((<>))
import           Database.PostgreSQL.LibPQ

newtype ScenarioId = ScenarioId Int64
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

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


data AuthRole
  = Admin
  | Student
  | Local
  deriving (Show, Read, Eq)

instance JSON.FromJSON AuthRole where
  parseJSON = JSON.withText "AuthRole" $ \s -> case s of
    "super-user" -> pure Admin
    "student" -> pure Student
    "local" -> pure Local
    _ -> fail "unknown auth role"


oidBIGINT :: Oid
oidBIGINT = Oid 20

oidJSONB :: Oid
oidJSONB = Oid 3802

-- oidJSON :: Oid
-- oidJSON = Oid 114

oidTEXT :: Oid
oidTEXT = Oid 25

-- oidARRAY :: Oid
-- oidARRAY = Oid 2277
-- select oid,typname from pg_type where typname like '%arr%';

-- oidNUMERIC :: Oid
-- oidNUMERIC = Oid 1700
--
-- mkNum :: (Show a, Num a) => a -> Maybe (Oid, ByteString, Format)
-- mkNum i = Just (oidNUMERIC, BSC.pack (show i), Text)

mkBigInt :: ScenarioId -> Maybe (Oid, ByteString, Format)
mkBigInt (ScenarioId i) = Just (oidBIGINT, BSC.pack (show i), Text)

mkInt64 :: Int64 -> Maybe (Oid, ByteString, Format)
mkInt64 i = Just (oidBIGINT, BSC.pack (show i), Text)

createScenario :: Connection
               -> Int64 -- ^ token (callID)
               -> Maybe Int64 -- ^ User identifier
               -> Maybe AuthRole
               -> BS.ByteString -- ^ scenario name
               -> BS.ByteString -- ^ GeoJSON Feature Collection
               -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
createScenario conn token userId authRole scName scenario  = do
  mrez <- execParams conn "SELECT wrap_result($1,create_scenario($2,$3,$4,$5));"
    [ mkInt64 token
    , userId >>= mkInt64
    , mkAuthRole authRole
    , Just (oidTEXT, BSC.filter (\c -> isAlphaNum c || c == ' ') scName, Text)
    , Just (oidJSONB, scenario, Text)
    ]
    Text
  justResult mrez $ flip checkResult id

-- | Duplicate scenario content into a new scenario record.
copyScenario :: Connection
             -> Int64 -- ^ token
             -> Maybe Int64 -- ^ UserId (owner of a new scenario)
             -> Maybe AuthRole
             -> Int64 -- ^ Scenario Id
             -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
copyScenario conn token userId authRole scId = do
  mrez <- execParams conn "SELECT wrap_result($1,copy_scenario($2,$3,$4));"
    [ mkInt64 token
    , userId >>= mkInt64
    , mkAuthRole authRole
    , mkInt64 scId
    ]
    Text
  justResult mrez $ flip checkResult id


mkAuthRole :: Maybe AuthRole -> Maybe (Oid, ByteString, Format)
mkAuthRole mauth = (\ar -> Just (oidTEXT, BSC.pack $ show ar, Text)) $ fromMaybe Local mauth

deleteScenario :: Connection
               -> Int64 -- ^ token (callID)
               -> Maybe Int64 -- ^ User Id
               -> Maybe AuthRole
               -> ScenarioId -- ^ ScID (scenario id)
               -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
deleteScenario conn token userId authRole scID = do
  mrez <- execParams conn "SELECT wrap_result($1,delete_scenario($2,$3,$4));"
    [ mkInt64 token
    , userId >>= mkInt64
    , mkAuthRole authRole
    , mkBigInt scID
    ] Text
  justResult mrez $ flip checkResult id


recoverScenario :: Connection
                -> Int64 -- ^ token (callID)
                -> ScenarioId -- ^ ScID (scenario id)
                -> Maybe Int64 -- ^ User Id
                -> Maybe AuthRole
                -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
recoverScenario conn token scID userId authRole = do
  mrez <- execParams conn "SELECT wrap_result($1,recover_scenario($2,$3,$4));"
    [ mkInt64 token
    , mkBigInt scID
    , userId >>= mkInt64
    , mkAuthRole authRole
    ] Text
  justResult mrez $ flip checkResult id


updateScenario :: Connection
               -> Int64 -- ^ token (callID)
               -> Maybe Int64 -- ^ User Id
               -> Maybe AuthRole
               -> ScenarioId -- ^ ScID (scenario id)
               -> BS.ByteString -- ^ GeoJSON Feature Collection
               -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
updateScenario conn token  userId authRole scID scenario = do
  mrez <- execParams conn "SELECT wrap_result($1,update_scenario($2,$3,$4,$5));"
    [ mkInt64 token
    , userId >>= mkInt64
    , mkAuthRole authRole
    , mkBigInt scID
    , Just (oidJSONB, scenario, Text)
    ]
    Text
  justResult mrez $ flip checkResult id


listScenarios :: Connection
              -> Int64 -- ^ token (callID)
              -> Maybe Int64 -- ^ User Id
              -> Maybe AuthRole
              -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
listScenarios conn token userId authRole = do
  mrez <- execParams conn "SELECT wrap_result($1,list_scenarios($1,$2));"
     [ mkInt64 token
     , userId >>= mkInt64
     , mkAuthRole authRole
     ] Text
  justResult mrez $ \rez -> checkResult rez id

getScenario :: Connection
            -> Int64 -- ^ token (callID)
            -> ScenarioId -- ^ ScID (scenario id)
            -> IO (Either BS.ByteString BS.ByteString) -- ^ Either error or json result
getScenario conn token scID = do
  mrez <- execParams conn "SELECT wrap_result($1,get_scenario($2));" [mkInt64 token, mkBigInt scID] Text
  justResult mrez $ \rez -> checkResult rez id


getLastScUpdates :: Connection
                 -> [Int64] -- ^ token (callID)
                 -> ScenarioId -- ^ ScID (scenario id)
                 -> IO (Either BS.ByteString [BS.ByteString]) -- ^ Either error or json results
getLastScUpdates conn tokens scID = do
  mrez <- execParams conn
      ( mconcat
        [ "SELECT wrap_progress_many(ARRAY"
        , BSC.pack (show tokens)
        , "::bigint[],50.0,get_last_sc_update($1));"
        ]
      )
    [ mkBigInt scID ]
    Text
  justResult mrez $ \rez -> checkResults rez id


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

checkResults :: Result -> (ByteString -> a) -> IO (Either ByteString [a])
checkResults rez f = do
  rstatus <- resultStatus rez
  ncol <- nfields rez
  nrow <- ntuples rez
  if nrow > 0 && ncol > 0 &&
     (rstatus == CommandOk || rstatus == TuplesOk)
  then Right . fmap f. catMaybes <$> traverse (flip (getvalue rez) 0) [0..nrow-1]
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
  , copyScenarioF
  , deleteScenarioF
  , listScenariosF
  , getScenarioF
  , recoverScenarioF
  , updateScenarioF
  , populateDBF
  , dropDBF
  , getLastScUpdateF
  , wrapResultF
  ]

createScenarioF :: BS.ByteString
createScenarioF = $(embedFile "sql/create_scenario.sql")

copyScenarioF :: BS.ByteString
copyScenarioF = $(embedFile "sql/copy_scenario.sql")

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


getLastScUpdateF :: BS.ByteString
getLastScUpdateF = $(embedFile "sql/get_last_sc_update.sql")

populateDBF :: BS.ByteString
populateDBF = $(embedFile "sql/populatedb.sql")

dropDBF :: BS.ByteString
dropDBF = $(embedFile "sql/dropdb.sql")


wrapResultF :: BS.ByteString
wrapResultF = $(embedFile "sql/wrap_result.sql")
