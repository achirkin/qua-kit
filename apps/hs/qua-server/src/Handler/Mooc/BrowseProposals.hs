{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.BrowseProposals
  ( getBrowseProposalsR
  , getBrowseProposalsForExpertsR
  , fetchLastSubmissions
  , mkSubmissionsWidget
  , noProposalParams
  , ProposalParams(..)
  , ProposalSortParam (..)
  ) where

import Application.Grading (designRatingToVisual)
import Database.Persist.Sql (rawSql, Single (..), toSqlKey)
import qualified Data.Text as Text
import Import
import Import.BootstrapUtil
import qualified Text.Blaze as Blaze
import Text.Read (read)

pageSize :: Int
pageSize = 80




getBrowseProposalsR :: Int -> Handler Html
getBrowseProposalsR = getBrowseProposalsPamsR noProposalParams


-- | We are using a separate route to have different sorting defaults.
--   i.e. `noProposalParams` differs from `convenientReviewOrder`,
--   and it would take more time to implement all these defaults via get parameters
--   (and also links would become more weird and less typesafe TM).
getBrowseProposalsForExpertsR :: Handler Html
getBrowseProposalsForExpertsR = getBrowseProposalsPamsR convenientReviewOrder 1

getBrowseProposalsPamsR :: ProposalParams -> Int -> Handler Html
getBrowseProposalsPamsR defParams page = do
    exercises <- runDB $ selectList [] []
    exerciseGet <- lookupGetParam "exercise_id"
    let params = defParams {onlyByExerciseId = byExerciseDef}
          where
            byExerciseDef = orElse (toSqlKey . read . unpack <$> exerciseGet)
                                   (onlyByExerciseId defParams)
            orElse x y = case x of
                           Just _  -> x
                           Nothing -> y
    ((res, widget), _) <- runFormGet $ proposalsForm params exercises
    let params' = case res of
                   (FormSuccess ps) -> ps
                   _ -> params
    submissions <- runDB $ fetchLastSubmissions $ params' {
                             propLimit  = Just pageSize
                           , propOffset = (max 0 $ page-1)*pageSize }
    submissionsWidget <- mkSubmissionsWidget submissions
    pages <- negate . (`div` pageSize) . negate <$> runDB (countUniqueSubmissions params')
    let is = [1..pages]
    fullLayout Nothing "Qua-kit student designs" $ do
      setTitle "Qua-kit student designs"
      toWidgetHead $
        -- move card-action down. Note, height 210 is 200 for image + 10 for overlapping margin of element above
        [julius|
          $(document).ready(function() {
            $('#proposalsForm').change( function() {
              this.submit();
            })
          });
        |]
      toWidgetHead $
        [cassius|
          .form-inline
            .form-group
              display: inline-block
              margin-left: 15px
            .btn-default
              margin-left: 15px
          .pageSelector
            display:inline
            background: none !important
            color: inherit
            border: none
            margin: 2px
            padding: 0 !important
            font: inherit
            cursor: pointer
            color: #ff6f00
            &:hover
              color: #b71c1c
          @media (min-width: 2000px)
            .col-xl-3
              width: 25%
            .container
              max-width: 3000px
        |]
      [whamlet|
        <form .form-inline #proposalsForm action=1 method=GET>
          <div class="ui-card-wrap">
            <div class=row>
              ^{widget}
            <div class=row>
              ^{submissionsWidget}

          <!-- footer with page numbers -->
          $if pages == 0
            <p>No submissions found with the selected criteria.
          $else
           <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9">
              <div class="card margin-bottom-no">
                <div class="card-main">
                  <div class="card-inner">
                   $forall i <- is
                    $if i == page
                      <p style="margin:2px;padding:0;display:inline">#{i}
                    $else
                      <input
                        type=submit
                        class=pageSelector
                        value=#{i}
                        formaction=@{BrowseProposalsR i}>
      |]

mkSubmissionsWidget :: [Submission] -> Handler Widget
mkSubmissionsWidget submissions = do
  role <- muserRole <$> maybeAuth
  let isExpert = role == UR_EXPERT
      cOpacity i = 0.5 + fromIntegral i / 198 :: Double
  return $ do
    toWidgetHead
      [cassius|
        span.card-comment.card-criterion
          width: 24px
          padding: 0px
          margin: 4px
          display: inline-block
          color: #ff6f00
          text-align: center
        .containerCard
          position: relative
          padding: 0
        .criterions
          padding: 3px 16px 0 16px
          position: absolute
          bottom: 0
          width: 100%
          background: white
        @media (min-width: 500px)
          .submissionCard
            height: 200px
      |]
    [whamlet|
      $forall sub <- submissions
        $with (Entity _ sc) <- scenario sub
          <div class="col-xl-3 col-lg-4 col-md-6 col-xs-12">
            <div.card.submissionCard>
              <aside.card-side.card-side-img.pull-left.card-side-moocimg>
                <img
                  src=@{submissionPreviewR sc}
                  width="200px" height="200px" style="margin-left: -25px;">
              <div.card-main.containerCard>
                <div.card-inner style="margin: 10px 12px;">
                  <p style="margin: 6px 0px; color: #b71c1c;">
                    #{authorName sub}
                      <br>
                    #{show $ utctDay $ currentScenarioLastUpdate sc}
                  <p style="margin: 6px 0px; white-space: pre-line; overflow-y: hidden; color: #555;">
                   #{shortComment $ currentScenarioDescription sc}
                <div.card-comment.card-action.criterions>
                  $forall (CriterionData svg cname crating) <- criterions sub
                   $maybe rating <- crating
                    <span.card-comment.card-criterion style="opacity: #{cOpacity rating}" title="#{cname}">
                      #{svg}
                      <p style="display: inline; margin: 0; padding:0; color: rgb(#{156 + rating}, 111, 0)">
                        #{rating}
                   $nothing
                    <span.card-comment.card-criterion style="opacity: 0.3"
                      title="Not enough votes to show rating">
                      #{svg}
                      \ - #
                  $maybe expertgrade <- avgExpertGrade sub
                    <span.card-comment.card-criterion  title="Expert Grade">
                      <span class="icon icon-lg" style="width:24px; height:24px;">star</span>
                        <p style="display: inline; margin: 0; padding:0; color: #b71c1c;)">
                          #{expertgrade}
                  <div.card-action-btn.pull-right>
                    $with subViewLink <- submissionR sc
                      $if isExpert && (isNothing $ currentScenarioGrade sc)
                        <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                            style="background: red; color: white !important;"
                            href=@{subViewLink} target="_blank">
                          <span.icon>visibility
                          Review
                      $else
                        <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect
                            href=@{subViewLink} target="_blank">
                          <span.icon>visibility
                          View
    |]



shortLength :: Int
shortLength = 140

maxLines :: Int
maxLines = 3

data CriterionData = CriterionData {
      critIcon   :: Blaze.Markup
    , critName   :: Text
    , critRating :: Maybe Int
    }

data Submission = Submission {
      scenario        :: Entity CurrentScenario
    , authorName      :: Text
    , avgExpertGrade  :: Maybe Double
    , criterions      :: [CriterionData]
  }

data ProposalSortParam = Default
                       | Newest
                       | Oldest
                       | GradeDesc
                       | GradeAsc
                       deriving (Eq, Show)

data ProposalParams = ProposalParams {
      onlyNeedsReview   :: Maybe ()
    , onlyByAuthorId    :: Maybe UserId
    , onlyByExerciseId  :: Maybe ExerciseId
    , sortOrder         :: ProposalSortParam
    , propLimit         :: Maybe Int
    , propOffset        :: Int
    }

noProposalParams :: ProposalParams
noProposalParams = ProposalParams {
      onlyNeedsReview   = Nothing
    , onlyByAuthorId    = Nothing
    , onlyByExerciseId  = Nothing
    , sortOrder         = Default
    , propLimit         = Nothing
    , propOffset        = 0
    }

convenientReviewOrder :: ProposalParams
convenientReviewOrder = ProposalParams {
      onlyNeedsReview   = Just ()
    , onlyByAuthorId    = Nothing
    , onlyByExerciseId  = Nothing
    , sortOrder         = Oldest
    , propLimit         = Nothing
    , propOffset        = 0
    }

proposalsForm :: ProposalParams -> [Entity Exercise] -> Html -> MForm Handler (FormResult ProposalParams, Widget)
proposalsForm defParams exercises extra = do
  maybeMe <- lift maybeAuthId
  let exerciseList = map (\(Entity spId sp) -> (exerciseDescription sp, Just spId)) exercises
  (onlyNeedsReviewRes, onlyNeedsReviewView) <- mreq (bootstrapSelectFieldList [
                      ("All"::Text,    Nothing)
                    , ("Needs review", Just ())
                    ]) "" (Just $ onlyNeedsReview defParams)
  (onlyByAuthorIdRes, onlyByAuthorView) <- mreq (bootstrapSelectFieldList [
                      ("All users"::Text, Nothing)
                    , ("Only my submissions", maybeMe)
                    ]) "" (Just $ onlyByAuthorId defParams)
  (onlyByExerciseIdRes, onlyByExerciseIdView) <- mreq (bootstrapSelectFieldList $
                      ("All exercises"::Text, Nothing) : exerciseList
                    ) "" (Just $ onlyByExerciseId defParams)
  (sortOrderRes, sortOrderView) <- mreq (bootstrapSelectFieldList [
                      ("Default order"::Text, Default)
                    , ("Newest first", Newest)
                    , ("Oldest first", Oldest)
                    , ("Best grade first", GradeDesc)
                    , ("Best grade last", GradeAsc)
                    ]) "" (Just $ sortOrder defParams)

  let proposalParams = ProposalParams <$> onlyNeedsReviewRes
                                      <*> onlyByAuthorIdRes
                                      <*> onlyByExerciseIdRes
                                      <*> sortOrderRes
                                      <*> FormSuccess (propLimit  defParams)
                                      <*> FormSuccess (propOffset defParams)
  let widget = do
        [whamlet|
          #{extra}
          ^{fvInput onlyNeedsReviewView}
          ^{fvInput onlyByAuthorView}
          ^{fvInput onlyByExerciseIdView}
          ^{fvInput sortOrderView}
          <input type=submit
                 value=Filter
                 class="btn btn-default"
                 formaction=@{BrowseProposalsR 1}
                 >
        |]
  return (proposalParams, widget)


shortComment :: Text -> Text
shortComment t = dropInitSpace . remNewLines $
  if Text.length t < shortLength
    then t
    else remLong t <> "..."
  where remLong = Text.dropEnd 1
                . Text.dropWhileEnd (\c -> c /= ' ' && c /= '\n' && c /= '\t')
                . Text.take shortLength
        remNewLines = Text.dropWhileEnd (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
                    . Text.unlines
                    . take maxLines
                    . filter (not . Text.null)
                    . map (Text.dropWhile (\c -> c == ' ' || c == '\r' || c == '\t'))
                    . Text.lines
        dropInitSpace = Text.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

-- TODO: turn this abomination into a proper esqueleto
generateJoins :: ProposalParams -> ([PersistValue], Text, Text)
generateJoins ps = (whereParams ++ limitParams, joinStr, orderStr)
  where
    joinStr = Text.unlines [
        " FROM ("
      , "   SELECT s.*, \"user\".name as username"
      , "   FROM current_scenario as s"
      , "   JOIN \"user\""
      , "     ON \"user\".id = s.author_id"
      ,     whereClause
      , "   ORDER BY ", primaryOrder, ", s.id DESC"
      ,     limitClause
      , " ) s"
      , " JOIN exercise_criterion"
      , "   ON s.exercise_id = exercise_criterion.exercise_id"
      , " JOIN criterion"
      , "   ON criterion.id = exercise_criterion.criterion_id"
      , " LEFT OUTER JOIN rating"
      , "   ON s.exercise_id    = rating.exercise_id AND"
      , "      s.author_id  = rating.author_id AND"
      , "      criterion.id = rating.criterion_id"
      , " LEFT OUTER JOIN ("
      , "   SELECT scenario_id, AVG(grade) as expertgrade"
      , "   FROM expert_review"
      , "   GROUP BY scenario_id"
      , " ) eg"
      , " ON s.history_scenario_id = eg.scenario_id"
      ]
    orderStr = Text.unlines [
        "ORDER BY ", primaryOrder, ", s.id DESC, criterion.id ASC"
      ]
    primaryOrder = case sortOrder ps of
                     Default   -> "s.exercise_id DESC, COALESCE(s.grade, 0) DESC"
                     Newest    -> "s.last_update DESC"
                     Oldest    -> "s.last_update ASC"
                     GradeDesc -> "COALESCE(s.grade, 0) DESC"
                     GradeAsc  -> "COALESCE(s.grade, 0) ASC"
    pOffset = toPersistValue $ propOffset ps
    (limitParams, limitClause) = -- use default limit size if no limit provided
                                 -- to make sqlite happy
      ([toPersistValue $ fromMaybe pageSize (propLimit ps), pOffset], "LIMIT ? OFFSET ?")
    (whereParams, whereClause) =
      if null wheres
      then ([], "")
      else (map fst wheres, "WHERE " ++ intercalate " AND " (map snd wheres))
      where
        wheres = catMaybes [
                  onlyByAuthorId   ps >>= \a -> Just (toPersistValue a, "s.author_id = ?")
                , onlyNeedsReview  ps >>= \_ -> Just (toPersistValue (0::Int), "s.grade IS NULL AND 0 = ?")
                , onlyByExerciseId ps >>= \e -> Just (toPersistValue e, "s.exercise_id = ?")
                 ]

-- | get user name, scenario, and ratings
fetchLastSubmissions :: ProposalParams -> ReaderT SqlBackend Handler [Submission]
fetchLastSubmissions params = groupSubs <$> map convertTypes <$> rawSql query preparedParams
  where
    (preparedParams, joinStr, orderStr) = generateJoins params

    groupSubs (sub:subs) = sub' : groupSubs subs'
      where
        (sub', subs') = go sub subs
        go y (x:xs)
            | currentScenarioHistoryScenarioId (entityVal $ scenario y) ==
              currentScenarioHistoryScenarioId (entityVal $ scenario x)
                = let crits = criterions y ++ criterions x
                  in  go (y {criterions = crits}) xs
            | otherwise = (y, x:xs)
        go y [] = (y, [])
    groupSubs [] = []


    currentScenarioCols = [ "s.id"
                          , "s.history_scenario_id"
                          , "s.author_id"
                          , "s.exercise_id"
                          , "s.description"
                          , "s.grade"
                          , "s.last_update"
                          , "s.edx_grading_id"
                          ]
    query = Text.unlines [
            " SELECT ", intercalate ", " currentScenarioCols
          , "   , username, eg.expertgrade"
          , "   , criterion.icon, criterion.name"
          , "   , COALESCE(rating.current_evidence_w, 0), COALESCE(rating.value, 0)"
          , joinStr
          , orderStr
          ,";"
          ]

countUniqueSubmissions :: ProposalParams -> ReaderT SqlBackend Handler Int
countUniqueSubmissions params = getVal <$> rawSql query preparedParams
  where
    (preparedParams, joinStr, _) = generateJoins params
    getVal (Single c:_)  = c
    getVal [] = 0
    query = Text.unlines
          [ "SELECT count(DISTINCT s.author_id)"
          , joinStr
          , ";"
          ]

convertTypes :: ( Single CurrentScenarioId --currentScId
                , ( Single ScenarioId --historyScenarioId
                  , Single UserId --authorId
                  , Single ExerciseId --exId
                  , Single Text --description
                  , Single (Maybe Double) --grade
                  , Single UTCTime --lastUpdate
                  , Single (Maybe EdxGradingId)--edxGradingId
                  )
                , Single Text --authorName
                , Single (Maybe Double) --avgExpertGrade
                , ( Single Text --criterionIcon
                  , Single Text --criterionName
                  , Single Double --evidence
                  , Single Double --rating
                  )
                ) -> Submission
convertTypes ( Single currentScId
             , ( Single historyScenarioId
               , Single authorId
               , Single exerciseId
               , Single description
               , Single grade
               , Single lastUpdate
               , Single edxGradingId
               )
             , Single authorName
             , Single avgExpertGrade
             , ( Single criterionIcon
               , Single criterionName
               , Single evidence
               , Single rating
               )
             )
             = Submission {
                scenario    = Entity currentScId $ CurrentScenario {
                  currentScenarioHistoryScenarioId = historyScenarioId
                , currentScenarioAuthorId          = authorId
                , currentScenarioExerciseId        = exerciseId
                , currentScenarioDescription       = description
                , currentScenarioEdxGradingId      = edxGradingId
                , currentScenarioGrade             = grade
                , currentScenarioLastUpdate        = lastUpdate
                }
              , authorName  = authorName
              , avgExpertGrade = avgExpertGrade
              , criterions  = [ CriterionData {
                    critIcon   = Blaze.preEscapedToMarkup (criterionIcon :: Text)
                  , critName   = criterionName
                  , critRating = designRatingToVisual evidence rating
                } ]
             }
