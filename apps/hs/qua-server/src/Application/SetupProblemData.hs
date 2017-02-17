{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
module Application.SetupProblemData
    ( importProblemData
    ) where


import Import

import Database.Persist.Sql
import qualified Data.ByteString as BS (readFile)
import qualified Data.Text.IO as Text (readFile)
--import qualified Data.ByteString.Base64 as BSB (encode)


importProblemData :: ConnectionPool -> IO ()
importProblemData pool = do
    registerAdmins pool
    importProblemRun0 pool
    importProblemRun1 pool


registerAdmins :: ConnectionPool -> IO ()
registerAdmins pool = flip runSqlPool pool $ do
    -- make me the first admin
    mme <- getBy $ ETHUserName (Just "achirkin")
    case mme of
     Nothing -> insert_ $ User "Artem Chirkin" UR_ADMIN (Just "achirkin") Nothing
     Just (Entity key _) -> update key [UserRole =. UR_ADMIN]


importProblemRun0 :: ConnectionPool -> IO ()
importProblemRun0 pool = do

    let df = "static" </> "data" </> "run0"
    sctaskfile <- BS.readFile $ df </> "mooctask.geojson"
    sctaskpreview <- BS.readFile $ df </> "mooctask.png"

    cCentrality    <- loadCriterion df "Centrality" "Centrality"
    cConnectivity  <- loadCriterion df "Connectivity" "Connectivity"
    cAccessibility <- loadCriterion df "Accessibility" "Accessibility"
    cVisibility    <- loadCriterion df "Visibility" "Visibility"

    flip runSqlPool pool $ do
      -- Id of the firt problem (Sep-Nov 2016)
      let pId = toSqlKey 0
      repsert pId (ScenarioProblem sctaskpreview sctaskfile "Empower Shack scenario" 0.001)
      cIdCentrality    <- upsertCriterion cCentrality
      cIdConnectivity  <- upsertCriterion cConnectivity
      cIdAccessibility <- upsertCriterion cAccessibility
      cIdVisibility    <- upsertCriterion cVisibility

      -- register criteria in the problem
      deleteWhere [ProblemCriterionProblemId ==. pId]
      insert_ (ProblemCriterion pId cIdCentrality)
      insert_ (ProblemCriterion pId cIdConnectivity)
      insert_ (ProblemCriterion pId cIdAccessibility)
      insert_ (ProblemCriterion pId cIdVisibility)


importProblemRun1 :: ConnectionPool -> IO ()
importProblemRun1 pool = do

    let df = "static" </> "data" </> "run1"
    sctaskfile <- BS.readFile $ df </> "mooctask.geojson"
    sctaskpreview <- BS.readFile $ df </> "mooctask.png"

    cDistribution  <- loadCriterion df "Density / Distribution" "Distribution"
    cAccessibility <- loadCriterion df "Accessibility / Connectivity" "AccConn"
    cVisibility    <- loadCriterion df "Visibility" "Visibility"
    cCentrality    <- loadCriterion df "Centrality" "Centrality"

    flip runSqlPool pool $ do
      -- Id of the firt problem (Sep-Nov 2016)
      let pId = toSqlKey 1
      repsert pId (ScenarioProblem sctaskpreview sctaskfile "Empower Shack scenario 2" 40000)
      cIdDistribution  <- upsertCriterion cDistribution
      cIdAccessibility <- upsertCriterion cAccessibility
      cIdVisibility    <- upsertCriterion cVisibility
      cIdCentrality    <- upsertCriterion cCentrality

      -- register criteria in the problem
      deleteWhere [ProblemCriterionProblemId ==. pId]
      insert_ (ProblemCriterion pId cIdCentrality)
      insert_ (ProblemCriterion pId cIdDistribution)
      insert_ (ProblemCriterion pId cIdAccessibility)
      insert_ (ProblemCriterion pId cIdVisibility)





-- | Load criterion content from a file
loadCriterion :: FilePath -> Text -> String -> IO Criterion
loadCriterion fp name fname = do
    cHtml <- Text.readFile $ fp </> fname <.> "html"
    cImg  <- BS.readFile $ fp </> fname <.> "png"
    cIcon <- fmap decodeUtf8 . BS.readFile $ fp </> fname <.> "svg"
    return $ Criterion name cHtml cImg cIcon

-- | Update or create criterion defenition
upsertCriterion :: MonadIO m
                => Criterion -> ReaderT SqlBackend m (Key Criterion)
upsertCriterion c@(Criterion _ cHtml cImg cIcon)
  = entityKey <$> upsert c
      [ CriterionDescription =. cHtml
      , CriterionImage =. cImg
      , CriterionIcon =. cIcon
      ]
