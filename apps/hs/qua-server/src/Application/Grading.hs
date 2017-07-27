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

We model all ratings as standard normal distributions (/N(0,1)/).
That is, we assume the following property holds for the user rating:
[center /r/[sub /i/](/t/[sub /τ/]) = (1 - /k/) /r/[sub /i/](/t/[sub /τ/-1]) + /kε/,]
where /ε/ ~ /N(0,1)/, and the initial state is /r/[sub /i/][literal (0)] = /ε/;
/k/ ∈ [0,1] is a constant that defines how much older submissions of a user affect
the current rating of the submission;
/r/[sub /i/](/t/[sub /τ/]) is a submission of a user /i/ made at time /t/[sub /τ/.]

According to the given definition, /r/[sub /i/] preserves standard normal distribution
for any /t/:
[center ∀/τ/=0,1,.. : /r/[sub /i/](t[sub /τ/]) ~ /N(0,1)/.]

We model real voter rating as one more standard normal random variable:
[center /v/[sub /i/] = /N(0,1)/.]
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

module Application.Grading
  -- (
  -- -- reviewRating, compareRating, combR, scheduleUpdateGrades
  -- )
   where



-- import Data.Time.Clock (DiffTime)
import qualified Database.Persist.Sql as P
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

-- | Variance of dependent variable /z/ = /δ/e[sup /v/ + /δ/(/r/[sub /j/] - /r/[sub /i/])].
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /r/ = 1.
constDz :: Double
constDz = 2.5

-- | Covariance of dependent variable /z/ = /δ/e[sup /v/ + /δ/(/r/[sub /j/] - /r/[sub /i/])]
--   and /r/[sub /i/] (NB, 0 > Cov[/r/[sub /i/],/z/] ≡ - Cov[/r/[sub /j/],/z/]).
--   We get the value of this constant empirically.
--   It is important to keep this value correct to maintain property Var /r/ = 1.
constKz :: Double
constKz = 0.25

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


TODO: The following formulae are totally incorrect!

Second, update actual ratings.
Define /δ/ = 1 if the voter prefers /i/ and  /δ/ = -1 otherwise.
Then the upated voter rating is defined as follows:
[center /v/ → (/ωv/ + 0.5/δ/(/w/[sub /i/] + /w/[sub /j/])(/r/[sub /i/] - /r/[sub /j/])) \/ (/ω/ + /w/[sub /i/] + /w/[sub /j/]).]

The updated score for the submission of users /i/ and /j/:
[center  /r/[sub /i/] → (/w/[sub /i/] /r/[sub /i/] + 0.5 /δ/ max(0, 1 + /v/) (1 + /r/[sub /j/] - /r/[sub /i/])) \/  (/w/[sub /i/] + max(0, 1 + /v/))  ]
[center  /r/[sub /j/] → (/w/[sub /j/] /r/[sub /j/] - 0.5 /δ/ max(0, 1 + /v/) (1 + /r/[sub /i/] - /r/[sub /j/])) \/  (/w/[sub /j/] + max(0, 1 + /v/))  ]

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
        v'  = ( ω * v + 0.5 * (wi + wj) * (ri - rj) ) / ω'

        -- helpers
        z   = log $ 1 + exp (v + rj - ri)
        fc w = recip $ sqrt ( w * constKz * constKz + constDz ) + sqrt w * constKz
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






--
--
-- -- mininum number of votes for this particular design to participate
-- minNi :: Int
-- minNi = 2
--
-- -- minimum number of votes for a criterion to participate
-- minNn :: Int
-- minNn = 20


----------------------------------------------------------------------------------------------------
--
--
-- reviewRating :: [(DiffTime,NDCount,Bool)] -> Double
-- reviewRating = min 1 . max 0 . (+0.4) . uncurry (/) . second (max 1) . foldl' (\(x, a) (_dt,dn,p) -> let d = vote dn in (if p then x + d else x - d, a+d)) (0,0)
--   where
--     vote = recip . fromIntegral . (+1)
--
-- compareRating :: [(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)]
-- compareRating xs = Map.toList $ iterN 10 naiveMap
--   where
--     -- cumsum
--     nn = recip $ foldl' (\s (_, (_,dn1), (_,dn2)) -> s + vote dn1 + vote dn2) 0 xs / 2
--
--     -- discount votes with nr of versions passed since last vote
--     vote = recip . fromIntegral . (+1)
--
--     -- at first, we naively count votes for and against each proposal
--     updateHMNaive hm (_dt, (u1,dn1), (u2,dn2)) = Map.alter (Just . (+(- vote dn2)) . fromMaybe 0) u2 $ Map.alter (Just . (+vote dn1) . fromMaybe 0) u1 hm
--     naiveMap :: Map.Map UserId Double
--     naiveMap = normMap $ foldl' updateHMNaive Map.empty xs
--
--     -- then we use this logic to iteratively come closer to solution
--     updateR (ob,ow) (cb,cw) = let diff = log (1 + exp (ow-ob)) * nn
--                               in (cb + diff, cw - diff)
--
--     iterN 0 hm = hm
--     iterN n hm = hm `seq` iterN (n-1 :: Int) (updateAllHM hm)
--
--     updateAllHM hm = normMap $ foldl' (updateHM hm) hm xs
--
--     updateHM :: Map.Map UserId Double -> Map.Map UserId Double -> (DiffTime, (UserId, NDCount), (UserId, NDCount)) -> Map.Map UserId Double
--     updateHM ohm hm (_, (u1,_), (u2,_)) = let ov1 = fromMaybe 0 $ Map.lookup u1 ohm
--                                               ov2 = fromMaybe 0 $ Map.lookup u2 ohm
--                                               v1 = fromMaybe 0 $ Map.lookup u1 hm
--                                               v2 = fromMaybe 0 $ Map.lookup u2 hm
--                                               (v1', v2') = updateR (ov1,ov2) (v1,v2)
--                                           in Map.alter (const $ Just v1') u1 $ Map.alter (const $ Just v2') u2 hm
--
--     normMap hm = let (mi, ma) = Map.foldl' (\(l,u) v -> (min l v, max u v)) (0,0) hm
--                      diff = max (ma-mi) 1
--                  in Map.map (\v -> (v - mi) / diff) hm
--
-- combR :: (Int, Int, Maybe Rating) -> (Int, Int, Maybe Rating) -> Rating
-- combR (_,_,Nothing) (_,_,Nothing) = error "[combR] Impossible: both ratings not here."
-- combR (n,i,Just (Rating pid cid uid v)) (_,_,Nothing) = Rating pid cid uid $ filterV n i v
-- combR (_,_,Nothing) (m,j,Just (Rating pid cid uid v)) = Rating pid cid uid $ filterV m j v
-- combR rv@(n,i,Just (Rating pid cid uid v)) ru@(m,j,Just (Rating _ _ _ u))
--     | n >= minNn && i >= minNi = combR (n,i, Nothing) ru
--     | m >= minNn && j >= minNi = combR rv (m,j, Nothing)
--     | otherwise = let cv = fromIntegral $ i * m
--                       cu = fromIntegral $ j * n
--                   in Rating pid cid uid $ (cv * v + cu * u) / (cv + cu)
--
-- filterV :: Int -> Int -> Double -> Double
-- filterV n i x = if n >= minNn && i >= minNi then x else 0
--
--
--
-- ----------------------------------------------------------------------------------------------------
--
--
-- --scheduleUpdateGrades dt app pool = void . forkIO . forever $ do
-- --    flip runSqlPool pool . flip runReaderT (appHttpManager app) $ _
--
-- scheduleUpdateGrades :: Int -> App -> ConnectionPool -> IO ()
-- scheduleUpdateGrades dt app pool = void . forkIO . forever $ do
--     -- wait ten seconds just to let yesod do other work first
--     threadDelay (10*1000000)
--     runResourceT . flip runReaderT (appHttpManager app) . flip runSqlPool pool $ sendGrades app
--     threadDelay dts
--   where
--     dts = dt * 1000000
--
-- sendGrades :: App -> ReaderT SqlBackend (ReaderT Manager (ResourceT IO)) ()
-- sendGrades app = do
--     -- send grades for all vote tasks
--     selectSource [] [] $$ awaitForever
--           (\(Entity _ vg) -> do
--             req <- replaceResultRequest (appLTICredentials $ appSettings app)
--                 (Text.unpack $ voteGradeEdxOutcomeUrl vg) (voteGradeEdxResultId vg) (0.6 + 0.4 * voteGradeGrade vg) Nothing
--             lift . lift $ catch (void $ httpNoBody req) (\e -> putStrLn $ "[edX send vote grades error] " <> Text.pack (show (e :: SomeException)))
--           )
--     -- send grades for all design tasks
--     allRatingGrades >>= mapM_
--           (\(ourl, rid, val) -> do
--             req <- replaceResultRequest (appLTICredentials $ appSettings app) ourl rid (min 1 $ 0.7 + 0.5*val)  Nothing
--             lift $ catch (void $ httpNoBody req) (\e -> putStrLn $ "[edX send design grades error] " <> Text.pack (show (e :: SomeException)))
--           )
--
--
-- allRatingGrades :: MonadIO a => ReaderT SqlBackend a [( String
--                                                       , Text
--                                                       , Double
--                                                       )]
-- allRatingGrades = fmap getVal <$> rawSql query []
--   where
--     getVal (Single a, Single b, Single c) = (a,b,c)
--     query = Text.unlines
--         ["SELECT t.edx_outcome_url, t.edx_result_id, AVG(COALESCE(t.value,0))"
--         ,"FROM criterion"
--         ,"LEFT OUTER JOIN "
--         ,"    ( SELECT scenario.edx_outcome_url, scenario.edx_result_id, rating.criterion_id, rating.value"
--         ,"      FROM scenario"
--         ,"      INNER JOIN rating"
--         ,"              ON scenario.task_id = rating.problem_id AND scenario.author_id = rating.author_id"
--         ,"      WHERE scenario.edx_outcome_url IS NOT NULL"
--         ,"      AND scenario.edx_result_id IS NOT NULL"
--         ,"      AND scenario.task_id = 1" -- TODO: make a proper logic to update ongoing grades only.
--         ,"      GROUP BY rating.problem_id"
--         ,"             , rating.author_id"
--         ,"             , scenario.edx_outcome_url"
--         ,"             , scenario.edx_result_id"
--         ,"             , rating.criterion_id"
--         ,"             , rating.value"
--         ,"    ) t ON criterion.id = t.criterion_id"
--         ,"GROUP BY t.edx_outcome_url"
--         ,"       , t.edx_result_id;"
--         ]
