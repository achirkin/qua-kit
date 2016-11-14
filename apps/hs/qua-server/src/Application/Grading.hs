-----------------------------------------------------------------------------
-- |
-- Module      :  Application.Grading
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Application.Grading
  ( reviewRating, compareRating, combR, scheduleUpdateGrades
  ) where


import Data.Time.Clock (DiffTime)
import Database.Persist.Sql
import Import
import Model.Rating
import qualified Data.Map.Strict as Map

import qualified Data.Text as Text
import Control.Concurrent (forkIO)
import Web.LTI

-- mininum number of votes for this particular design to participate
minNi :: Int
minNi = 2

-- minimum number of votes for a criterion to participate
minNn :: Int
minNn = 20


----------------------------------------------------------------------------------------------------


reviewRating :: [(DiffTime,NDCount,Bool)] -> Double
reviewRating = min 1 . max 0 . (+0.4) . uncurry (/) . second (max 1) . foldl' (\(x, a) (_dt,dn,p) -> let d = vote dn in (if p then x + d else x - d, a+d)) (0,0)
  where
    vote = recip . fromIntegral . (+1)

compareRating :: [(DiffTime, (UserId, NDCount), (UserId, NDCount))] -> [(UserId,Double)]
compareRating xs = Map.toList $ iterN 10 naiveMap
  where
    -- cumsum
    nn = recip $ foldl' (\s (_, (_,dn1), (_,dn2)) -> s + vote dn1 + vote dn2) 0 xs / 2

    -- discount votes with nr of versions passed since last vote
    vote = recip . fromIntegral . (+1)

    -- at first, we naively count votes for and against each proposal
    updateHMNaive hm (_dt, (u1,dn1), (u2,dn2)) = Map.alter (Just . (+(- vote dn2)) . fromMaybe 0) u2 $ Map.alter (Just . (+vote dn1) . fromMaybe 0) u1 hm
    naiveMap :: Map.Map UserId Double
    naiveMap = normMap $ foldl' updateHMNaive Map.empty xs

    -- then we use this logic to iteratively come closer to solution
    updateR (ob,ow) (cb,cw) = let diff = log (1 + exp (ow-ob)) * nn
                              in (cb + diff, cw - diff)

    iterN 0 hm = hm
    iterN n hm = hm `seq` iterN (n-1 :: Int) (updateAllHM hm)

    updateAllHM hm = normMap $ foldl' (updateHM hm) hm xs

    updateHM :: Map.Map UserId Double -> Map.Map UserId Double -> (DiffTime, (UserId, NDCount), (UserId, NDCount)) -> Map.Map UserId Double
    updateHM ohm hm (_, (u1,_), (u2,_)) = let ov1 = fromMaybe 0 $ Map.lookup u1 ohm
                                              ov2 = fromMaybe 0 $ Map.lookup u2 ohm
                                              v1 = fromMaybe 0 $ Map.lookup u1 hm
                                              v2 = fromMaybe 0 $ Map.lookup u2 hm
                                              (v1', v2') = updateR (ov1,ov2) (v1,v2)
                                          in Map.alter (const $ Just v1') u1 $ Map.alter (const $ Just v2') u2 hm

    normMap hm = let (mi, ma) = Map.foldl' (\(l,u) v -> (min l v, max u v)) (0,0) hm
                     diff = max (ma-mi) 1
                 in Map.map (\v -> (v - mi) / diff) hm

combR :: (Int, Int, Maybe Rating) -> (Int, Int, Maybe Rating) -> Rating
combR (_,_,Nothing) (_,_,Nothing) = error "[combR] Impossible: both ratings not here."
combR (n,i,Just (Rating pid cid uid v)) (_,_,Nothing) = Rating pid cid uid $ filterV n i v
combR (_,_,Nothing) (m,j,Just (Rating pid cid uid v)) = Rating pid cid uid $ filterV m j v
combR rv@(n,i,Just (Rating pid cid uid v)) ru@(m,j,Just (Rating _ _ _ u))
    | n >= minNn && i >= minNi = combR (n,i, Nothing) ru
    | m >= minNn && j >= minNi = combR rv (m,j, Nothing)
    | otherwise = let cv = fromIntegral $ i * m
                      cu = fromIntegral $ j * n
                  in Rating pid cid uid $ (cv * v + cu * u) / (cv + cu)

filterV :: Int -> Int -> Double -> Double
filterV n i x = if n >= minNn && i >= minNi then x else 0



----------------------------------------------------------------------------------------------------


--scheduleUpdateGrades dt app pool = void . forkIO . forever $ do
--    flip runSqlPool pool . flip runReaderT (appHttpManager app) $ _

scheduleUpdateGrades :: Int -> App -> ConnectionPool -> IO ()
scheduleUpdateGrades dt app pool = void . forkIO . forever $ do
    -- wait ten seconds just to let yesod do other work first
    threadDelay (10*1000000)
    runResourceT . flip runReaderT (appHttpManager app) . flip runSqlPool pool $ sendGrades app
    threadDelay dts
  where
    dts = dt * 1000000

sendGrades :: App -> ReaderT SqlBackend (ReaderT Manager (ResourceT IO)) ()
sendGrades app = do
    -- send grades for all vote tasks
    selectSource [] [] $$ awaitForever
          (\(Entity _ vg) -> do
            req <- replaceResultRequest (appLTICredentials $ appSettings app)
                (Text.unpack $ voteGradeEdxOutcomeUrl vg) (voteGradeEdxResultId vg) (0.6 + 0.4 * voteGradeGrade vg) Nothing
            lift . lift $ catch (void $ httpNoBody req) (\e -> putStrLn $ "[edX send vote grades error] " <> Text.pack (show (e :: SomeException)))
          )
    -- send grades for all design tasks
    allRatingGrades >>= mapM_
          (\(ourl, rid, val) -> do
            req <- replaceResultRequest (appLTICredentials $ appSettings app) ourl rid (min 1 $ 0.7 + 0.5*val)  Nothing
            lift $ catch (void $ httpNoBody req) (\e -> putStrLn $ "[edX send design grades error] " <> Text.pack (show (e :: SomeException)))
          )


allRatingGrades :: MonadIO a => ReaderT SqlBackend a [( String
                                                      , Text
                                                      , Double
                                                      )]
allRatingGrades = fmap getVal <$> rawSql query []
  where
    getVal (Single a, Single b, Single c) = (a,b,c)
    query = Text.unlines
        ["SELECT t.edx_outcome_url, t.edx_result_id, AVG(COALESCE(t.value,0))"
        ,"FROM criterion"
        ,"LEFT OUTER JOIN "
        ,"    ( SELECT scenario.edx_outcome_url, scenario.edx_result_id, rating.criterion_id, rating.value"
        ,"      FROM scenario"
        ,"      INNER JOIN rating"
        ,"              ON scenario.task_id = rating.problem_id AND scenario.author_id = rating.author_id"
        ,"      WHERE scenario.edx_outcome_url IS NOT NULL"
        ,"      AND scenario.edx_result_id IS NOT NULL"
        ,"      GROUP BY rating.problem_id"
        ,"             , rating.author_id"
        ,"             , scenario.edx_outcome_url"
        ,"             , scenario.edx_result_id"
        ,"             , rating.criterion_id"
        ,"             , rating.value"
        ,"    ) t ON criterion.id = t.criterion_id"
        ,"GROUP BY t.edx_outcome_url"
        ,"       , t.edx_result_id;"
        ]
