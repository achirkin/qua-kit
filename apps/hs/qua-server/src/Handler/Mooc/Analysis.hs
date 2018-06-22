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
import Numeric
-- import Import.BootstrapUtil
-- import qualified Text.Blaze as Blaze
-- import Text.Read (read)


data SortType = SortAsc | SortDesc
  deriving (Eq, Ord, Show, Read)

data AResultOrder
  = ByDate
  | ByScoreB
  | ByScoreT
  | ByScoreR2
  | ByScoreR3
  | ByScoreR4
  | ByScoreR5
  | ByScoreR6
  | ByScoreR7
  deriving (Eq, Ord, Show, Read)



getBrowseSAR :: Handler Html
getBrowseSAR = do

  offsetV <- getParam "offset" 0
  limitV  <- getParam "limit" 200
  authorIdV   <- lookupParamSQLKey @User "authorId"
  exerciseIdV   <- lookupParamSQLKey @Exercise "exerciseId"
  arOrder <- lookupParam "order"
  arSType <- getParam "sort" SortDesc


  let -- limit the output per page with reasonable defaults
      withLimitsQ = offset offsetV >> limit limitV

      constrainExQ scenario = maybe (return ()) f exerciseIdV
        where
          f exId = where_ (scenario ^. ScenarioExerciseId ==. val exId)

      constrainAuthorQ scenario = maybe (return ()) f authorIdV
        where
          f uId = where_ (scenario ^. ScenarioAuthorId ==. val uId)

      orderQ scenario votesB votesT votesR2 votesR3
             votesR4 votesR5 votesR6 votesR7 = maybe (return ()) f arOrder
        where
          sortE :: ( Esqueleto query expr backend, PersistField a )
                => expr (E.Value a) -> expr OrderBy
          sortE = case arSType of
            SortAsc -> asc
            SortDesc -> desc
          f ByDate = orderBy [sortE $ scenario ^. ScenarioLastUpdate ]
          f ByScoreB  = orderByScore votesB
          f ByScoreT  = orderByScore votesT
          f ByScoreR2 = orderByScore votesR2
          f ByScoreR3 = orderByScore votesR3
          f ByScoreR4 = orderByScore votesR4
          f ByScoreR5 = orderByScore votesR5
          f ByScoreR6 = orderByScore votesR6
          f ByScoreR7 = orderByScore votesR7
          orderByScore v = orderBy [ sortE $ v  ^. SAImageMax ] --  /. ( val 0.0001 +.v ^. SAImageMean ) ]
             -- (v  ^. SAImageMax -. v ^. SAImageMean) *. (v  ^. SAImageMax -. v ^. SAImageMean) /. (val 0.0001 +. v ^. SAImageVar ) ]



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

        -- sorting
        orderQ scenario votesB votesT votesR2 votesR3
               votesR4 votesR5 votesR6 votesR7

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
    toWidgetBody
      [julius|
        $(function() {
          var colnames = $("#colnames");
          $(window).on("scroll", function(e) {
            if (window.scrollY > 260) {
              colnames.addClass("colnamesdetached");
            } else {
              colnames.removeClass("colnamesdetached");
            }

          });
        });
      |]
    toWidgetHead
      [cassius|
        .anacol
          width: 128px
          float: left
          text-align: center
          color: #b71c1c
          margin: 2px

        .colnamesdetached
          position: fixed
          top: 10px
          left: auto
          z-index: 1000
      |]
    let sortOrdT newOrd = case arSType of
            SortAsc  -> "&sort=" <> setsort SortAsc SortDesc
            SortDesc -> "&sort=" <> setsort SortDesc SortAsc
          where
            setsort a b = show $ if Just newOrd /= arOrder then a else b
        exerciseIdT = case exerciseIdV of
          Nothing -> ""
          Just eid -> "&exerciseId=" <> show (E.fromSqlKey eid)
        oflimT = "&offset=" <> show offsetV
              <> "&limit=" <> show limitV
        pamsT newOrd = "?order="  <> show newOrd
                     <> sortOrdT newOrd <> exerciseIdT <> oflimT

    [whamlet|
      <div #colnames class="row">
        <div class="col-xl-12 col-lg-12 col-md-12 col-sm-12 col-xs-12">
          <div class="card margin-bottom-no">
            <div class="card-main">
              <div class="card-inner" style="margin: 8px 8px 38px 8px;">
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByDate}">
                    Scenario
                <div.anacol>
                  Objects 2D
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreB}">
                    Bilateral
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreT}">
                    Translational
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR2}">
                    2-Fold
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR3}">
                    3-Fold
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR4}">
                    4-Fold
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR5}">
                    5-Fold
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR6}">
                    6-Fold
                <div.anacol>
                  <a href="@{BrowseSAR}#{pamsT ByScoreR7}">
                    7-Fold
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
    <div style="width: 128px; float: left; margin: 2px">
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
roundD x = if abs p > 4 then showEFloat (Just 4) x ""
                        else trimS $  showFFloat (Just 4) x ""
  where
    trimS s = reverse . noP . go . reverse $ take 6 s
      where
        go ('0':xs) = go xs
        go xs = xs
        noP ('.':xs) = xs
        noP xs = xs
    p = floor $ logBase 10 x :: Int
