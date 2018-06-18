{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Mooc.Analysis
  ( getBrowseSAR
  , getSAImageR
  ) where

-- import Database.Persist.Sql (rawSql, Single (..), toSqlKey)
-- import qualified Data.Text as Text
import Import hiding ((==.), on)
import Database.Esqueleto as E
import Text.Read (readMaybe)
-- import Import.BootstrapUtil
-- import qualified Text.Blaze as Blaze
-- import Text.Read (read)


getBrowseSAR :: Handler Html
getBrowseSAR = do

  offsetV <- getParam "offset" 0
  limitV  <- getParam "limit" 200
  authorIdV   <- lookupParamSQLKey @User "authorId"
  exerciseIdV   <- lookupParamSQLKey @Exercise "exerciseId"

  let -- limit the output per page with reasonable defaults
      withLimitsQ = offset offsetV >> limit limitV

      constrainExQ scenario = maybe (return ()) f exerciseIdV
        where
          f exId = where_ (scenario ^. ScenarioExerciseId ==. val exId)

      constrainAuthorQ scenario = maybe (return ()) f authorIdV
        where
          f uId = where_ (scenario ^. ScenarioAuthorId ==. val uId)


      -- the main part of the query
      coreQ ( scAnalysis `InnerJoin`
              scenario   `InnerJoin`
              blocksImg  `InnerJoin`
              votesB     `InnerJoin`
              votesT     `InnerJoin`
              votesR2    `InnerJoin`
              votesR3    `InnerJoin`
              votesR4    `InnerJoin`
              votesR5    `InnerJoin`
              votesR6    `InnerJoin`
              votesR7
            ) = do
        on ( scAnalysis ^. ScenarioAnalysisVotesR7 ==. votesR7 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesR6 ==. votesR6 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesR5 ==. votesR5 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesR4 ==. votesR4 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesR3 ==. votesR3 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesR2 ==. votesR2 ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesT  ==. votesT ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisVotesB  ==. votesB ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisBlocksImg ==. blocksImg ^. SAImageId )
        on ( scAnalysis ^. ScenarioAnalysisScenarioId ==. scenario ^. ScenarioId )

        -- optional constraints
        constrainAuthorQ scenario
        constrainExQ scenario

        -- limit output
        withLimitsQ

        return ( ( scenario ^. ScenarioExerciseId
                 , scenario ^. ScenarioAuthorId
                 , scenario ^. ScenarioId
                 , scenario ^. ScenarioLastUpdate
                 , scenario ^. ScenarioDescription
                 , scAnalysis ^. ScenarioAnalysisId
                 , scAnalysis ^. ScenarioAnalysisObjectCount
                 )
               , blocksImg
               , ( votesB
                 , votesT
                 , ( votesR2
                   , votesR3
                   , votesR4
                   , votesR5
                   , votesR6
                   , votesR7
                   )
                 )
               )


  scenarios <- fmap (fmap stupidTuples) . runDB $ select $ from coreQ

  fullLayout Nothing "Qua-kit design analysis" $ do
    setTitle "Qua-kit design analysis"
    [whamlet|
      $forall s <- scenarios
         <div class="row">
          <div class="col-xl-12 col-lg-12 col-md-12 col-sm-12 col-xs-12">
            ^{scenarioAnalysisWidget s}

    |]

scenarioAnalysisWidget :: AInfo -> Widget
scenarioAnalysisWidget AInfo {..} =
  [whamlet|
      <div class="card margin-bottom-no">
        <div class="card-main">
          <div class="card-inner">
            <div style="width: 128px; float: left">
              <img src="@{ProposalPreviewR aiExerciseId aiUserId}" width="128px" >
              <p>
                exerciseId:  #{show $ fromSqlKey aiExerciseId}
              <p>
                autorId:     #{show $ fromSqlKey aiUserId}
              <p>
                scenarioId:  #{show $ fromSqlKey aiScenarioId}
              <p>
                date:        #{show $ utctDay aiTimeStamp}
              <p>
                objectCount: #{show aiObjectCount}

            ^{aimgWidget aiBlocks}
            ^{aimgWidget aiVotesB}
            ^{aimgWidget aiVotesT}
            ^{aimgWidget aiVotesR2}
            ^{aimgWidget aiVotesR3}
            ^{aimgWidget aiVotesR4}
            ^{aimgWidget aiVotesR5}
            ^{aimgWidget aiVotesR6}
            ^{aimgWidget aiVotesR7}
  |]

aimgWidget :: AImg -> Widget
aimgWidget AImg {..} =
  [whamlet|
    <div style="width: 128px; float: left">
      <img src="@{SAImageR aiId}" width="128px" height="128px" >
      <p>
        span: #{roundD aiMin} - #{roundD aiMax}
      <p>
        mean: #{roundD aiMean}
      <p>
        var: #{roundD aiVar}
  |]


getSAImageR :: SAImageId -> Handler TypedContent
getSAImageR imgId = do
    img <- runDB $ get404 imgId
    addHeader "Content-Disposition" "inline"
    sendResponse ("image/png" :: ByteString, toContent $ sAImageData img)


stupidTuples
  :: ( ( E.Value ExerciseId
       , E.Value UserId
       , E.Value ScenarioId
       , E.Value UTCTime
       , E.Value Text
       , E.Value ScenarioAnalysisId
       , E.Value Int)
     , Entity SAImage
     , ( Entity SAImage
       , Entity SAImage
       , ( Entity SAImage, Entity SAImage, Entity SAImage
         , Entity SAImage, Entity SAImage, Entity SAImage)
       )
     ) -> AInfo
stupidTuples
  ( ( E.Value aiExerciseId
    , E.Value aiUserId
    , E.Value aiScenarioId
    , E.Value aiTimeStamp
    , E.Value aiDescription
    , E.Value aiScenarioAnalysisId
    , E.Value aiObjectCount)
    , aiBlocks'
    , ( aiVotesB', aiVotesT'
      , ( aiVotesR2', aiVotesR3', aiVotesR4', aiVotesR5', aiVotesR6', aiVotesR7' )
      )
  ) = AInfo {..}
  where
    toAImg (Entity i (SAImage _ m v mi ma)) = AImg i m v mi ma
    aiBlocks = toAImg aiBlocks'
    aiVotesB = toAImg aiVotesB'
    aiVotesT = toAImg aiVotesT'
    aiVotesR2 = toAImg aiVotesR2'
    aiVotesR3 = toAImg aiVotesR3'
    aiVotesR4 = toAImg aiVotesR4'
    aiVotesR5 = toAImg aiVotesR5'
    aiVotesR6 = toAImg aiVotesR6'
    aiVotesR7 = toAImg aiVotesR7'

data AInfo = AInfo
  { aiExerciseId         :: ExerciseId
  , aiUserId             :: UserId
  , aiScenarioId         :: ScenarioId
  , aiTimeStamp          :: UTCTime
  , aiDescription        :: Text
  , aiScenarioAnalysisId :: ScenarioAnalysisId
  , aiObjectCount        :: Int
  , aiBlocks             :: AImg
  , aiVotesB             :: AImg
  , aiVotesT             :: AImg
  , aiVotesR2            :: AImg
  , aiVotesR3            :: AImg
  , aiVotesR4            :: AImg
  , aiVotesR5            :: AImg
  , aiVotesR6            :: AImg
  , aiVotesR7            :: AImg
  } deriving (Eq, Show)

data AImg = AImg
  { aiId   :: SAImageId
  , aiMean :: Double
  , aiVar  :: Double
  , aiMin  :: Double
  , aiMax  :: Double
  } deriving (Eq, Show)


lookupParamSQLKey :: ToBackendKey SqlBackend record
                  => Text -> Handler (Maybe (Key record))
lookupParamSQLKey = fmap (fmap toSqlKey) . lookupParam

lookupParam :: Read a => Text -> Handler (Maybe a)
lookupParam pname = (>>= (readMaybe . unpack)) <$> lookupGetParam pname

getParam :: Read a => Text -> a -> Handler a
getParam pname da = fromMaybe da <$> lookupParam pname


roundD :: Double -> String
roundD x = if abs p > 4 then sho (multd * base) ++ "e" ++ show p
                        else sho (mult  * base)
  where
    sho = trimS . show
    trimS s = reverse . go . reverse $ take 6 s
      where
        go ('0':xs) = go xs
        go xs = xs
    base = fromIntegral (round (x / mult) :: Int)
    digits = 3 :: Int
    p = floor $ logBase 10 x :: Int
    mult = 10 ^^ (p - digits) :: Double
    multd = 10 ^^ negate digits :: Double
