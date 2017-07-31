{-|
Module      :  Application.Grading
Copyright   :  (c) Artem Chirkin
License     :  MIT

Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
Stability   :  experimental

This module provides functions to construct and update user ratings and grades
for qua-kit exercises.




= Exercise grading

Qua-kit has to give grades for two exercises:

  1. "design" exercise -- make a design submission;
  2. "compare" exercise -- compare a given pair of designs w.r.t. given criterion.

For both exercises qua-kit needs to compute a single absolute value (and send it to edX).
Since we have a variable number of criteria, we compute a submission rating for each criterion,
and then average the design-criterion ratings into a single grade.

For design exercise, the rating means
/"how good the design submission w.r.t. given criterion, according to the majority vote"/.

For compare exercise, the rating means /"how good this person as an expert, according to majority vote"/,
i.e. the rating is higher when user's votes agree with majority.





== Rating model

Users can update their submissions, so we need to discount older ratings.
Define the moments of time when scenarios are updated as /t/[sub /τ/] (/τ/ = 0,1.., /t/[sub 0] = 0).

We model all ratings as random variables and try to keep their expected values and variances constant.
That is, we assume the following property holds for the user rating:
[center /r/[sub /i/](/t/[sub /τ/]) = (1 - /k/)[sup 0.5] /r/[sub /i/](/t/[sub /τ/-1]) + /k/[sup 0.5]/ε/,]
The initial state is /r/[sub /i/][literal (0)] = /ε/;
/k/ ∈ [0,1] is a constant that defines how much older submissions of a user affect
the current rating of the submission;
/r/[sub /i/](/t/[sub /τ/]) is a submission of a user /i/ made at time /t/[sub /τ/.]
According to the given definition, /r/[sub /i/] preserves variance for any /t/.

We model real voter rating /v/[sub /i/] as one more random variable.
We assume the voter rating /v/[sub /i/] does not change over time.

== Modeling

The goal of the rating modeling is to estimate real rating values via series
of comparison votes.

=== Fair initial assumptions

In the beginning, we do not have any evidence about the real values,
so the trivial assumption is the theoretical expected value:
[center  /r/[sub /i/][literal (0)] = 0 and /v/[sub /i/] = 0. ]

=== Supplementary information

Every vote of a submission adds up to the total evidence of the rating.
But, when a user updates their submission, the evidence drops
because submission had changed and old votes may not represent the actual quality
of a new submission version.
To address this problem, we introduce two supplementary variables.

The first variable represents the evidence weight of the current design submission version:
[center /w/[sub /i/](/t/[sub /τ/]) = (1 - /k/) /w/[sub /i/](/t/[sub /τ/-1]) + /kn/[sub /i/](/t/[sub /τ/]), /w/[sub /i/][literal (0)] = 1. ]
Here,
/n/[sub /i/](/t/[sub /τ/]) represent a number of votes casted for submission of a user /i/,
since time /t/[sub /τ/-1] till time /t/[sub /τ/].

The second variable represents the evidence weight of the voter score:
[center /ω/[sub /i/] = ∑/w/[sub /j/], /j/ goes over all votes casted by user /i/. ]

-}

module Application.Grading where



-- import Data.Time.Clock (DiffTime)
import qualified Database.Persist.Sql as P
import Control.Monad.Trans.Maybe
import Import hiding ((/=.), (==.), (=.), isNothing, on, update)
import Database.Esqueleto
import Text.Shakespeare.Text (st)
-- import Model.Rating
-- import qualified Data.Map.Strict as Map
--
-- import qualified Data.Text as Text
-- import Control.Concurrent (forkIO)
-- import Web.LTI

-- * Program-wise constants

-- | Constant /k/ tells how much we appreciate submission history of a user.
--   If /k/ == 0 then we assume quality of design does not change at all.
--   If /k/ == 1 then we assume a new submission is not related to previous at all.
constK :: Double
constK = 0.7

-- | Standard deviation of the dependent variable /z/ = /δ/ log ( 1 + e[sup /v/ + /δ/(/r/[sub /j/] - /r/[sub /i/])]).
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /r/ = 1.
constSz :: Double

-- | Correlation of the dependent variable /z/ = /δ/ log ( 1 + e[sup /v/ + /δ/(/r/[sub /j/] - /r/[sub /i/])])
--   and /r/[sub /i/] (NB, 0 < Cov[/r/[sub /i/],/z/] ≡ - Cov[/r/[sub /j/],/z/]).
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /r/ = 1.
constCz :: Double

-- | Standard deviation of the dependent variable /y/ = /δ/(/r/[sub /i/] - /r/[sub /j/]).
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /v/ = 1.
constSy :: Double

-- | Correlation between vote rating and correctness of voter's decisions,
--   (i.e. correlation between /v/ and /y/ = /δ/(/r/[sub /i/] - /r/[sub /j/]) ).
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /v/ = 1.
constCy :: Double


constSz = sqrt 0.2
constCz = 0.45
constSy = sqrt 0.46
constCy = 0.33

-- | Minimum evidence required to grade a design.
--   1 + constK * n where n is number of votes
minimumEvidence :: Double
minimumEvidence = 1 + constK * 3

-- * Updating ratings

{- |
==== On design submission

When a submission is updated, the only variable we need to update is
the design rating evidence.
To avoid cheating, we should update the evidence if any votes were casted on the previous submission only.
In addition, we have to make sure that the evidence weight /w/ does not go below allowed minimum.
The updation formula is simple:
[center /w/[sub /i/] → (1 - /k/)/w/[sub /i/].]

Also, we need to keep track of number of votes for the latest design submission:
[center /n/[sub /i/] → 0.]
-}
updateRatingOnSubmission :: MonadIO a
                         => ScenarioId
                         -> ReaderT SqlBackend a ()
updateRatingOnSubmission scId = do
    ratings <- select $ from $ \(InnerJoin scenario rating) -> do
        on ( rating ^. RatingProblemId ==. scenario ^. ScenarioTaskId
         &&. rating ^. RatingAuthorId  ==. scenario ^. ScenarioAuthorId
           )
        where_ $ scenario ^. ScenarioId ==. val scId
        return rating
    forM_ ratings $ \(Entity i r) ->
      update $ \rating -> do
        set rating [ RatingCurrentVotesN =. val 0
                   , RatingCurrentEvidenceW =. val (updateEvidence r)
                   ]
        where_ $ rating ^. RatingId ==. val i
  where
    updateEvidence r    -- if there were no votes for the last submission,
                        -- do not decrease evidence
                      | ratingCurrentVotesN r == 0 = ratingCurrentEvidenceW r
                        -- if the evidence is at its minimum,
                        -- do not decrease it any more
                      | ratingCurrentEvidenceW r < 1/(1.0001 - constK) = 1
                      | otherwise = (1 - constK) * ratingCurrentEvidenceW r



{- |
==== On voting

Voting procedure affects three users:

  * Submission author /i/
  * Submission author /j/
  * Voter

First, we need to update counters:
[center /n/[sub /i/] → /n/[sub /i/] + 1,]
[center /n/[sub /j/] → /n/[sub /j/] + 1,]
[center /w/[sub /i/] → /w/[sub /i/] + k,]
[center /w/[sub /j/] → /w/[sub /j/] + k,]
[center /ω/ → /ω/ + /w/[sub /i/] + /w/[sub /j/].]

Second, update actual ratings.
Refer to Grading Rmd for more formulae.
-}
updateRatingsOnVoting :: (MonadLogger a, MonadIO a)
                      => VoteId
                      -> ReaderT SqlBackend a ()
updateRatingsOnVoting voteId = do
    Just vote <- get voteId
    ratingB <- mratingQ (voteCriterionId vote) (voteBetterId vote) >>= fromMaybeRating (voteCriterionId vote) (voteBetterId vote)
    ratingW <- mratingQ (voteCriterionId vote) (voteWorseId vote)  >>= fromMaybeRating (voteCriterionId vote) (voteWorseId vote)
    ratingV <- mVoteRating vote >>= fromMaybeVRating vote

    -- assume δ == 1, so i is better than j
    let ni = ratingCurrentVotesN $ entityVal ratingB
        nj = ratingCurrentVotesN $ entityVal ratingW
        wi = ratingCurrentEvidenceW $ entityVal ratingB
        wj = ratingCurrentEvidenceW $ entityVal ratingW
        ri = ratingValue $ entityVal ratingB
        rj = ratingValue $ entityVal ratingW
        ω  = voteRatingEvidenceW $ entityVal ratingV
        v  = voteRatingValue $ entityVal ratingV

        -- compute updates
        ni' = ni + 1
        nj' = nj + 1
        wi' = wi + constK
        wj' = wj + constK
        ω'  = ω + wi + wj
        v'  = ( sqrt ω * v + (ri - rj) / constSy
                              / ( sqrt (constCy * constCy * ω + wi + wj)
                                  + constCy * sqrt ω
                                )
              )
              / sqrt ω'

        -- helpers
        z   = log $ 1 + exp (v + rj - ri)
        fc w = recip $ (sqrt ( w * constCz * constCz + 1 ) + sqrt w * constCz) * constSz
        fww w = sqrt $ w / (w + 1)
        fw1 w = sqrt $ 1 / (w + 1)

        -- compute rating updates
        ri' = fww wi * ri + fw1 wi * fc wi * z
        rj' = fww wj * rj - fw1 wj * fc wj * z

    $(logDebug) $ [st|
      --------------------------------------------------------------------------
      -- Update ratings on voting ----------------------------------------------
      - ni - wi - ri ----- nj - wj - rj ----------------------------------------
      - #{show ni} - #{show wi} - #{show ri} --- #{show nj} - #{show wj} - #{show rj}
      - ω - v ------------ ω' - v' ---------------------------------------------
      - #{show ω} - #{show v} --- #{show ω'} - #{show v'}
      - ri' - rj' --------------------------------------------------------------
      - #{show ri'} - #{show rj'}
      --------------------------------------------------------------------------
      |]

    P.update (entityKey ratingB)
      [ RatingCurrentVotesN P.=. ni'
      , RatingCurrentEvidenceW P.=. wi'
      , RatingValue P.=. ri'
      ]

    P.update (entityKey ratingW)
      [ RatingCurrentVotesN P.=. nj'
      , RatingCurrentEvidenceW P.=. wj'
      , RatingValue P.=. rj'
      ]

    P.update (entityKey ratingV)
      [ VoteRatingEvidenceW P.=. ω'
      , VoteRatingValue P.=. v'
      ]

    -- update current scenario grade if needed
    updateCurrentScenarioGrade (voteBetterId vote)
    updateCurrentScenarioGrade (voteWorseId vote)

  where
    -- select the rating correcponsing to a better or worse design
    mratingQ crId scId = select $ from $ \(InnerJoin rating scenario) -> do
      on $ rating ^. RatingProblemId ==. scenario ^. ScenarioTaskId
       &&. rating ^. RatingAuthorId  ==. scenario ^. ScenarioAuthorId
       &&. rating ^. RatingCriterionId ==. val crId
      where_ $ scenario ^. ScenarioId ==. val scId
      return rating
    -- insert rating into DB if necessary
    fromMaybeRating _ _ (x:_) = pure x
    fromMaybeRating crId scId [] = do
      insertSelect $ from $ \scenario -> do
        where_ $ scenario ^. ScenarioId ==. val scId
        return $ Rating <#  (scenario ^. ScenarioTaskId)
                        <&> val crId
                        <&> (scenario ^. ScenarioAuthorId)
                        <&> val 0 <&> val 0 <&> val 1
      unsafeHead <$> mratingQ crId scId

    -- select voter rating
    mVoteRating vote = select $ from $ \(InnerJoin voteR scenario) -> do
      on $ scenario ^. ScenarioTaskId ==. voteR ^. VoteRatingProblemId
      where_ $ scenario ^. ScenarioId ==. val (voteBetterId vote)
           &&. voteR ^. VoteRatingStudentId ==. val (voteVoterId vote)
      return voteR
    fromMaybeVRating _ (x:_) = pure x
    fromMaybeVRating vote [] = do
      insertSelect $ from $ \scenario -> do
        where_ $ scenario ^. ScenarioId ==. val (voteBetterId vote)
        return $ VoteRating <#  (scenario ^. ScenarioTaskId)
                            <&> val (voteVoterId vote)
                            <&> val 0
                            <&> val 0
      unsafeHead <$> mVoteRating vote

-- | whenever we update ratings, we should try to update CurrentScenario grade
--   using this function.
updateCurrentScenarioGrade :: MonadIO a
                           => ScenarioId
                           -> ReaderT SqlBackend a ()
updateCurrentScenarioGrade scId = void . runMaybeT $ do
  Entity csId CurrentScenario{..} <- MaybeT $ getBy $ LatestSubmissionId scId

  lift $ if currentScenarioExpertEnforced
    then do
      grades <- P.selectList
        [ ExpertReviewScenarioId P.==. scId
        ]
        []
      if (null ratings)
      then P.update csId [CurrentScenarioGrade P.=. Nothing]
      else do
        let avgRating = sum (ratingValue . entityVal <$> ratings) / fromIntegral (length ratings)
        P.update csId [CurrentScenarioGrade P.=. Just (designRatingToVisualGrade $ avgRating)]
    else do
      ratings <- P.selectList
        [ RatingProblemId P.==. currentScenarioTaskId
        , RatingAuthorId P.==. currentScenarioAuthorId
        , RatingCurrentEvidenceW P.>=. minimumEvidence
        ]
        []
      if (null ratings)
      then P.update csId [CurrentScenarioGrade P.=. Nothing]
      else do
        let avgRating = sum (ratingValue . entityVal <$> ratings) / fromIntegral (length ratings)
        P.update csId [CurrentScenarioGrade P.=. Just (designRatingToVisualGrade $ avgRating)]




-- | Run the grading process simulation:
--   update ratings for every single vote or design submission
--   in chronological order.
--lift $
--   Useful for doing and testing changes in rating+grading system.
simulateGradingLearning :: (MonadLogger a, MonadIO a, MonadResource a)
                        => ReaderT SqlBackend a ()
simulateGradingLearning = do
    $(logWarn) [st| Starting simulating grading process... |]
    -- first, delete all ratings.
    P.deleteWhere ([] :: [Filter Rating])
    P.deleteWhere ([] :: [Filter VoteRating])
    -- read votes and submissions one by one in chonological order
    let votes = P.selectSource ([] :: [Filter Vote]) [Asc VoteTimestamp]
        submissions = P.selectSource ([] :: [Filter Scenario]) [Asc ScenarioLastUpdate]
    process (newResumableSource votes) (newResumableSource submissions)
    $(logWarn) [st| Grading simulation finished! |]
  where
    process v s = do
      (v', mvote) <- v $$++ await
      case mvote of
        -- no votes, so consume all remaining submissions
        Nothing -> do
            closeResumableSource v'
            s $$+- awaitForever (lift . updateRatingOnSubmission . entityKey)
        Just ev -> do
          (s', msubmission) <- s $$++ await
          case msubmission of
            -- no submissions, consume all remaining votes
            Nothing -> do
                closeResumableSource s'
                v' $$+- (leftover ev >> awaitForever (lift . updateRatingsOnVoting . entityKey))
            Just es -> if voteTimestamp (entityVal ev) < scenarioLastUpdate (entityVal es)
                       then do
                         updateRatingsOnVoting (entityKey ev)
                         (s'', ()) <- s' $$++ leftover es
                         process v' s''
                       else do
                         updateRatingOnSubmission (entityKey es)
                         (v'', ()) <- v' $$++ leftover ev
                         process v'' s'


-- * Communicating with edX

--
-- -- | Send all grade changes from GradingQueue table
-- --   And remove those grades from the table.
-- sendPendingGrades ::
-- case (,) <$> lis_result_sourcedid <*> lis_outcome_service_url of
--               Nothing -> return ()
--               Just (sourcedId, outcomeUrl) -> do
--                 ye <- getYesod
--                 req <- replaceResultRequest (appLTICredentials $ appSettings ye) (Text.unpack outcomeUrl) sourcedId 0.6 Nothing
--                 _ <- httpNoBody req
-- return ()


-- | This formula is based on plots generated by Application/Grading.html
--   The idea is to give 60% of grade for free and remaining 40% from rating.
compareRatingToEdxGrade :: Double -> Double
compareRatingToEdxGrade r = 0.6 + min 0.4 (max 0 r)


-- | This formula is based on plots generated by Application/Grading.html
--   The idea is to give 60% of grade for free and remaining 40% from rating.
--   Also, give less than 60% for really worst submissions :)
designRatingToEdxGrade :: Double -> Double
designRatingToEdxGrade r = 0.8 + 0.2 * min 1 r

-- | Grades from 1 to 5
designRatingToVisualGrade :: Double -> Double
designRatingToVisualGrade r = (max (-1) (min 1 r)) * 4 + 1


-- | The scale to visualize in gallery (1..99)
designRatingToVisual :: Double -> Int
designRatingToVisual = max 1 .  min 99 . round . (100*) . max (-1.05) . min 1.05
