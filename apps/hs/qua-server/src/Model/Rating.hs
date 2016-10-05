-----------------------------------------------------------------------------
-- |
-- Module      :  Model.Rating
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Model.Rating
  ( updateRatings, NDCount (..)
  , scheduleUpdateRatings
  , scheduleGradeVotes
  ) where


import Import.NoFoundation
import Database.Persist.Sql (rawSql, Single(..), toSqlKey)
import qualified Data.Text as Text
import Data.Time.Clock
import Control.Concurrent (forkIO)
--import Control.Monad.Trans.Control

--Rating
--    authorId    UserId
--    problemId   ScenarioProblemId
--    criterionId CriterionId
--    value       Double
--    RatingOf authorId problemId criterionId

-- | Number of submissions of a certain design later than a date specified
newtype NDCount = NDCount Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral,PersistField)

-- | Update ratings for all designs based on several rules every n seconds
scheduleUpdateRatings :: Int
                      -- ^ time interval in seconds
                      -> ([(DiffTime,NDCount,Bool)] -> Double)
                      -- ^ How to evaluate review rating of a design given time since vote,
                      --    number of new versions since vote, and vote value [for all relevant votes]
                      -> ([(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)])
                      -- ^ How to evaluate comparison rating of all designs given a series of votes
                      --   {time since vote, better design id and nr of versions, worse design id and nr of versions}
                      -> ((Int, Int, Maybe Rating) -> (Int, Int, Maybe Rating) -> Rating)
                      -- ^ How to combine review (first) and comarison (second) ratings.
                      --   Each triple is (overall number of votes, number of votes for this design, rating value)
                      -> ReaderT SqlBackend IO ()
scheduleUpdateRatings dt rf cf combinef = liftBaseWith $ \run -> void . forkIO . forever $ do
     run $ updateRatings rf cf combinef
     threadDelay dts
  where
    dts = dt * 1000000


scheduleGradeVotes :: Int -> ReaderT SqlBackend IO ()
scheduleGradeVotes dt = liftBaseWith $ \run -> void . forkIO . forever $ do
     run gradeVotes
     threadDelay dts
  where
    dts = dt * 1000000



-- | Update ratings for all designs based on several rules.
updateRatings :: MonadIO a
              => ([(DiffTime,NDCount,Bool)] -> Double)
              -- ^ How to evaluate review rating of a design given time since vote,
              --    number of new versions since vote, and vote value [for all relevant votes]
              -> ([(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)])
              -- ^ How to evaluate comparison rating of all designs given a series of votes
              --   {time since vote, better design id and nr of versions, worse design id and nr of versions}
              -> ((Int, Int, Maybe Rating) -> (Int, Int, Maybe Rating) -> Rating)
              -- ^ How to combine review (first) and comarison (second) ratings.
              --   Each triple is (overall number of votes, number of votes for this design, rating value)
              -> ReaderT SqlBackend a ()
updateRatings rf cf combinef = do
    rr <- groups . sortOn fst <$> calculateReviewRatings rf
--    liftIO $ mapM_ (mapM_ (\(Rating pid cid uid v, i) -> print (fromSqlKey pid, fromSqlKey cid, fromSqlKey uid, v, i))) rr
--    liftIO $ putStrLn "------------------"
    cr <- groups . sortOn fst <$> calculateComparisonRatings cf
--    liftIO $ mapM_ (mapM_ (\(Rating pid cid uid v, i) -> print (fromSqlKey pid, fromSqlKey cid, fromSqlKey uid, v, i))) cr
--    liftIO $ putStrLn "------------------"
    deleteWhere ([] :: [Filter Rating])
    insertMany_ $ groupCombine rr cr
  where
    sumup = foldl' (\a (_,i) -> a + i) 0
    cmpg (Rating p1 c1 _ _) (Rating p2 c2 _ _) = compare p1 p2 <> compare c1 c2
    cmpf (Rating p1 c1 u1 _) (Rating p2 c2 u2 _) = compare p1 p2 <> compare c1 c2 <> compare u1 u2
    groupCombine :: [[(Rating,Int)]] -> [[(Rating,Int)]] -> [Rating]
    groupCombine gxs@(xs@((x,_):_):gxs') gys@(ys@((y,_):_):gys') = case cmpg x y of
          LT -> combine (sumup xs) 0 xs [] ++ groupCombine gxs' gys
          GT -> combine 0 (sumup ys) [] ys ++ groupCombine gxs gys'
          EQ -> combine (sumup xs) (sumup ys) xs ys ++ groupCombine gxs' gys'
    groupCombine ([]:gxs') (ys:gys') = combine 0 (sumup ys) [] ys ++ groupCombine gxs' gys'
    groupCombine (xs:gxs') ([]:gys') = combine (sumup xs) 0 xs [] ++ groupCombine gxs' gys'
    groupCombine [] (ys:gys') = combine 0 (sumup ys) [] ys ++ groupCombine [] gys'
    groupCombine (xs:gxs') [] = combine (sumup xs) 0 xs [] ++ groupCombine gxs' []
    groupCombine [] []  = []
    combine n m ((x,i):xs) ((y,j):ys) = case cmpf x y of
          LT -> combinef (n, i, Just x ) (m, 0, Nothing) : combine n m xs ((y,j):ys)
          GT -> combinef (n, 0, Nothing) (m, j, Just y ) : combine n m ((x,i):xs) ys
          EQ -> combinef (n, i, Just x ) (m, j, Just y ) : combine n m xs ys
    combine n m [] ((y,j):ys) = combinef (n, 0, Nothing) (m, j, Just y ) : combine n m [] ys
    combine n m ((x,i):xs) [] = combinef (n, i, Just x ) (m, 0, Nothing) : combine n m xs []
    combine _ _ [] []         = []
    groups = groupBy (\(Rating p1 c1 _ _, _) (Rating p2 c2 _ _, _) -> p1 == p2 && c1 == c2)

calculateReviewRatings :: MonadIO a
                       => ([(DiffTime,NDCount,Bool)] -> Double)
                       -> ReaderT SqlBackend a [(Rating, Int)]
calculateReviewRatings f = do
    lr <- findLatestReviews
    t <- liftIO getCurrentTime
    return . ratingTemplates . groups $ convert t lr
  where
    convert ct ((t,scpId,uId,nd,crId,p):xs) = ((scpId, uId, crId), (realToFrac $ diffUTCTime ct t :: DiffTime, nd, p)) : convert ct xs
    convert _ [] = []
    ratingTemplates (xs@(((scpId, uId, crId), _):_):xxs) = (Rating scpId crId uId . f $ map snd xs, length xs) : ratingTemplates xxs
    ratingTemplates ([]:xxs) = ratingTemplates xxs
    ratingTemplates [] = []
    groups = groupBy (\x y -> fst x == fst y)

calculateComparisonRatings :: MonadIO a
                           => ([(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)])
                           -> ReaderT SqlBackend a [(Rating, Int)]
calculateComparisonRatings f = do
    lc <- findLatestComparisons
    t <- liftIO getCurrentTime
    return . ratingTemplates . groups $ convert t lc
  where
    convert ct ((t,cr,scpId,ub,uw):xs) = ((scpId, cr), (realToFrac $ diffUTCTime ct t :: DiffTime, ub, uw)) : convert ct xs
    convert _ [] = []
    ratingTemplates (xs@(((scpId, crId), _):_):xxs)
        = let rez = f sou
              sou = map snd xs
          in ((\(u,v, i) -> (Rating scpId crId u v, i)) <$> countv rez sou) ++ ratingTemplates xxs
    ratingTemplates ([]:xxs) = ratingTemplates xxs
    ratingTemplates [] = []
    groups = groupBy (\x y -> fst x == fst y)
    countv :: [(UserId,Double)] -> [(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double, Int)]
    countv ((userId, val):xs) ys = (userId, val, length $ filter (inhere userId) ys) : countv xs ys
    countv [] _ = []
    inhere i (_, (j,_), (k,_)) = i == j || i == k

-- | Find the latest review votes from each user
--   on each submission
--   + number of designs were made since then
findLatestReviews :: MonadIO a
                  => ReaderT SqlBackend a [( UTCTime
                                           , ScenarioProblemId
                                           , UserId
                                           , NDCount
                                           , CriterionId
                                           , Bool)]
findLatestReviews = fmap getVal <$> rawSql query []
  where
    getVal (Single a, Single b, Single c, Single d, Single e, Single f) = (a,b,c,d,e,f)
    query = Text.unlines
        ["SELECT review.\"timestamp\",t.task_id,t.author_id,COUNT(scenario.id), review.criterion_id,review.positive"
        ,"FROM review"
        ,""
        ,"INNER JOIN"
        ,"(SELECT review.reviewer_id,review.criterion_id,"
        ,"    MAX(review.\"timestamp\") as lasttime,"
        ,"    scenario.task_id, scenario.author_id"
        ,"FROM review"
        ,"INNER JOIN scenario"
        ,"        ON scenario.id = review.scenario_id"
        ,"GROUP BY review.reviewer_id,review.criterion_id, scenario.task_id, scenario.author_id) t"
        ,"ON review.\"timestamp\" = t.lasttime AND review.reviewer_id = t.reviewer_id AND review.criterion_id = t.criterion_id"
        ,""
        ,"LEFT OUTER JOIN scenario"
        ,"ON  t.task_id = scenario.task_id AND t.author_id = scenario.author_id"
        ,"AND review.\"timestamp\" < scenario.last_update"
        ,"GROUP BY review.reviewer_id,review.criterion_id, t.task_id, t.author_id, review.\"timestamp\", review.positive"
        ,""
        ,"ORDER BY t.task_id ASC, t.author_id ASC, review.criterion_id ASC,review.positive ASC, review.\"timestamp\" ASC;"]


-- | Find the latest comparison votes from each user
--   on each submission
--   + number of designs were made since then
findLatestComparisons :: MonadIO a
                      => ReaderT SqlBackend a [( UTCTime
                                               , CriterionId
                                               , ScenarioProblemId
                                               , (UserId, NDCount)
                                               , (UserId, NDCount)
                                               )]
findLatestComparisons = fmap getVal <$> rawSql query []
  where
    getVal (Single a, Single b, Single c, Single d, Single e, Single f, Single g) = (a,b,c,(d,f),(e,g))
    query = Text.unlines
        ["SELECT vote.\"timestamp\",vote.criterion_id,tb.task_id,tb.author_id,tw.author_id, COUNT(s1.id), COUNT(s2.id)"
        ,"FROM vote"
        ,""
        ,"INNER JOIN"
        ,"(SELECT vote.voter_id,vote.criterion_id,"
        ,"    MAX(vote.\"timestamp\") as lasttime,"
        ,"    scenario.task_id, scenario.author_id"
        ,"FROM vote"
        ,"INNER JOIN scenario"
        ,"        ON scenario.id = vote.better_id"
        ,"GROUP BY vote.voter_id,vote.criterion_id, scenario.task_id, scenario.author_id) tb"
        ,"ON vote.\"timestamp\" = tb.lasttime AND vote.voter_id = tb.voter_id AND vote.criterion_id = tb.criterion_id"
        ,""
        ,"INNER JOIN"
        ,"(SELECT vote.voter_id,vote.criterion_id,"
        ,"    MAX(vote.\"timestamp\") as lasttime,"
        ,"    scenario.task_id, scenario.author_id"
        ,"FROM vote"
        ,"INNER JOIN scenario"
        ,"        ON scenario.id = vote.worse_id"
        ,"GROUP BY vote.voter_id,vote.criterion_id, scenario.task_id, scenario.author_id) tw"
        ,"ON vote.\"timestamp\" = tw.lasttime AND vote.voter_id = tw.voter_id AND vote.criterion_id = tw.criterion_id"
        ,""
        ,"LEFT OUTER JOIN scenario s1"
        ,"ON  tb.task_id = s1.task_id AND tb.author_id = s1.author_id AND vote.\"timestamp\" < s1.last_update"
        ,""
        ,"LEFT OUTER JOIN scenario s2"
        ,"ON  tw.task_id = s2.task_id AND tw.author_id = s2.author_id AND vote.\"timestamp\" < s2.last_update"
        ,""
        ,"GROUP BY vote.voter_id, vote.criterion_id, tb.task_id, tb.author_id, tw.task_id, tw.author_id, vote.\"timestamp\""
        ,""
        ,"ORDER BY tb.task_id ASC, vote.criterion_id ASC, vote.\"timestamp\" ASC;"]




----------------------------------------------------------------------------------------------------



gradeVotes :: MonadIO a => ReaderT SqlBackend a ()
gradeVotes = do
    voteGrades <- map makeResult . groupBy voteKeys <$> allVotes
    deleteWhere ([] :: [Filter VoteGrade])
    insertMany_ $ fjust voteGrades
  where
    voteKeys (a1,b1,c1,d1,_) (a2,b2,c2,d2,_) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
--    takeSome  xs = let l = length xs
--                   in take (max 10 ((l * 2) `div` 3)) xs
    calculate xs = let n = fromIntegral $ length xs
                       g = foldl' (\a dx -> a + dx*(1-dx)) 0 xs
                   in max 0 . min 1 $ g/n + 1
    makeResult xs@((uid, resId, ourl,sid,_):_)
      = let vs = get5 xs
        in Just $ VoteGrade uid resId ourl sid (calculate vs)
    makeResult [] = Nothing
    get5 ((_,_,_,_,a):xs) = a : get5 xs
    get5 [] = []
    fjust (Just x : xs) = x : fjust xs
    fjust (Nothing: xs) = fjust xs
    fjust [] = []


allVotes :: MonadIO a => ReaderT SqlBackend a [( UserId
                                               , EdxResourceId
                                               , Text
                                               , Text
                                               , Double
                                               )]
allVotes = fmap getVal <$> rawSql query []
  where
    getVal (Single a, Single b, Single c, Single d, Single e) = (toSqlKey a, toSqlKey b,c,d,e)
    query = Text.unlines
        ["WITH rate AS ("
        ,"    SELECT scenario.id, rating.criterion_id, rating.value"
        ,"    FROM scenario INNER JOIN rating"
        ,"                          ON scenario.author_id = rating.author_id AND scenario.task_id = rating.problem_id"
        ,")"
        ,""
        ,"SELECT vote.voter_id, vote.edx_resource, vote.edx_outcome_url, vote.edx_result_id, (better.value - worse.value) as dx"
        ,"FROM vote"
        ,"INNER JOIN rate better"
        ,"        ON vote.criterion_id = better.criterion_id AND vote.better_id = better.id"
        ,"INNER JOIN rate worse"
        ,"        ON vote.criterion_id = worse.criterion_id AND vote.worse_id = worse.id"
        ,"WHERE vote.edx_resource IS NOT NULL"
        ,"  AND vote.edx_outcome_url IS NOT NULL"
        ,"  AND vote.edx_result_id IS NOT NULL"
        ,"ORDER BY vote.voter_id ASC, vote.edx_outcome_url ASC, dx DESC;"
        ]


