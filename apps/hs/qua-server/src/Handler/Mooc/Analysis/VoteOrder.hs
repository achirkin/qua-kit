{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Analysis.VoteOrder
  ( getVoteOrderR
  , getDoVoteOrderR
  , postDoVoteOrderR
  , getOrderExitPollR
  , postOrderExitPollR
  ) where

import Import hiding ((==.), (!=.), on)
import Database.Esqueleto as E
import Text.Read (readMaybe)
import Crypto.Number.Generate


-- | Log in and be redirected to do voting
getVoteOrderR :: ExerciseId -> Handler Html
getVoteOrderR exId = do
    clearCreds False
    setCreds False $ Creds "temporary" "Anonymous user" []
    setMyVoteCount 0

    fullLayout Nothing "Qua-kit design order voting" $ do
      setTitle "Qua-kit design order voting"
      [whamlet|
        <div class="row">
          <div class="col-xl-6 col-lg-6 col-md-8 col-sm-12 col-xs-12">
            <div.card>
              <div.card-main>
                <div.card-inner>
                  <p>
                    Dear participant,

                  <p>
                    In this experiment, I am asking for your help in understanding #
                    of what is the order in the context of qua-kit design study.

                  <p>
                    I want you to go through a series of pairs of 2D plans of #
                    student submissions and select designs that feature more #
                    structure, order, symmetry, or some kind of alignment.

                  <p>
                    Please, do not spend more than a few seconds on each pair of designs,
                    even if in doubt.

                  <p>
                    You are asked to vote for at least twenty pairs of designs,
                    but you are welcome to do as many of them as you want!

                  <p style="text-align: right">
                    Thank you,
                    <br>
                    Artem Chirkin

                <div.card-action>
                  <div.card-action-btn.pull-left>
                      <a.btn.btn-flat.btn-red.waves-attach.waves-effect href="@{DoVoteOrderR exId}">
                        Let's go!
      |]




getDoVoteOrderR :: ExerciseId -> Handler Html
getDoVoteOrderR exId = do

    mvc <- getMyVoteCount

    (scId1, imgId1) <- runDB $ getRandomDesign exId Nothing
    (scId2, imgId2) <- runDB $ getRandomDesign exId (Just scId1)
    let s1 = show $ fromSqlKey scId1
        s2 = show $ fromSqlKey scId2

    fullLayout Nothing "Please, click on a design that features more order." $ do
      setTitle "Qua-kit design order voting"
      [whamlet|
        <div class="row">
          <div class="col-xl-6 col-lg-6 col-md-8 col-sm-12 col-xs-12">
            <div.card>
              <div.card-main>
                <div.card-inner style="text-align: center">
                  <form #submitvoteform1 method="post" style="display: inline-block; width: 49%">
                    <input id="betterId" name="betterId" type="hidden" value="#{s1}">
                    <input id="worseId"  name="worseId"  type="hidden" value="#{s2}">
                    <input type="image" src=@{SAImageR imgId1} style="width: 100%">
                  <form #submitvoteform2 method="post" style="display: inline-block; width: 49%">
                    <input id="betterId" name="betterId" type="hidden" value="#{s2}">
                    <input id="worseId"  name="worseId"  type="hidden" value="#{s1}">
                    <input type="image" src=@{SAImageR imgId2} style="width: 100%">
                  $if mvc > 20
                    <br>
                    <div.pull-right>
                      <a href="@{OrderExitPollR}">
                        Finish here if you are tired of clicking.
      |]


getRandomDesign :: ExerciseId
                -> Maybe ScenarioId
                -> YesodDB App (ScenarioId, SAImageId)
getRandomDesign exId mfirstId = do
    n <- subCountQ
    getRandomDesign' n 100
  where

    -- attempt to get a first result a few times
    getRandomDesign' :: Int64 -> Int -> YesodDB App (ScenarioId, SAImageId)
    getRandomDesign' _ 0 = notFound
    getRandomDesign' n k = do
      off <- liftIO $ generateBetween 0 (fromIntegral $ n - 1)
      r <- select .  from . mainQ $ fromInteger off
      case r of
        [] -> getRandomDesign' n (k-1)
        (s,i):_ -> return (unValue s, unValue i)

    subCountQ = fmap f . select $ from $ \ scenario -> do
        where_ ( scenario ^. ScenarioExerciseId ==. val exId)
        return ( E.count $ scenario ^. ScenarioId )
      where
        f [] = 1
        f (n:_) = max 1 $ unValue n

    -- should return one row, but sometimes may return zero
    mainQ off (
        scenario   `InnerJoin`
        scAnalysis `InnerJoin`
        blocksImg
      ) = do
      on ( scAnalysis ^. ScenarioAnalysisBlocksImg  ==. blocksImg ^. SAImageId )
      on ( scAnalysis ^. ScenarioAnalysisScenarioId ==. scenario ^. ScenarioId )

      offset off
      where_ ( scenario ^. ScenarioExerciseId ==. val exId)

      notFirstOneQ scenario
      limit 1
      return ( scenario ^. ScenarioId, blocksImg ^. SAImageId )

    -- make sure not to show two exactly same scenarios
    notFirstOneQ scenario = case mfirstId of
      Nothing -> pure ()
      Just fI -> where_ ( scenario ^. ScenarioId !=. val fI)



postDoVoteOrderR :: ExerciseId -> Handler Html
postDoVoteOrderR exId = do
    userId <- requireAuthId
    mBetterId <- lookupParamSQLKey "betterId"
    mWorseId <- lookupParamSQLKey "worseId"
    case (,) <$> mBetterId <*> mWorseId of
      Nothing -> notFound
      Just (betterId, worseId) -> do
        t <- liftIO getCurrentTime
        runDB $ insert_ $ VoteOrder userId betterId worseId t
        getMyVoteCount >>= setMyVoteCount . (+1)
    getDoVoteOrderR exId



lookupParamSQLKey :: ToBackendKey SqlBackend record
                  => Text -> Handler (Maybe (Key record))
lookupParamSQLKey = fmap (fmap toSqlKey) . lookupParam

lookupParam :: Read a => Text -> Handler (Maybe a)
lookupParam pname = (>>= (readMaybe . unpack)) <$> lookupPostParam pname


getMyVoteCount :: Handler Int
getMyVoteCount = fromMaybe 0
              . (>>= (readMaybe . unpack)) <$> lookupSession "myVoteCount"

setMyVoteCount :: Int -> Handler ()
setMyVoteCount = setSession "myVoteCount" . pack . show


getOrderExitPollR :: Handler Html
getOrderExitPollR = do
    fullLayout Nothing "Thank you for your participation!" $ do
      setTitle "Qua-kit design order voting"
      [whamlet|
        <div class="row">
          <div class="col-xl-6 col-lg-6 col-md-8 col-sm-12 col-xs-12">
            <div.card>
              <div.card-main>
                <div.card-inner>
                  <p>
                    <a5 style="color: #ff6f00;">
                      One last question: #
                    could you please describe the logic of your choices? #
                    In other words, #
                      <i>
                        how did you decide that one submission was more ordered #
                        than another?
                  <form #submitvoteform method="post">
                    <div.form-group.form-group-label>
                      <label.floating-label for="explanation">
                         Please, write couple sentences here.
                      <textarea.form-control.textarea-autosize id="explanation" name="explanation" rows="1">
                      <div.card-action-btn.pull-left>
                        <button.btn.btn-flat.btn-red.waves-attach.waves-effect type="submit">
                          Submit
      |]

postOrderExitPollR :: Handler Html
postOrderExitPollR = do
    userId <- requireAuthId
    explanation <- fromMaybe "" <$> lookupPostParam "explanation"
    t <- liftIO getCurrentTime
    runDB . insert_ $ ExplainOrder userId explanation t
    setMessageI ("Thank you for your contribution!" :: Text)
    redirect MoocHomeR
