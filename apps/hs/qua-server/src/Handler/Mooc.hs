{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE CPP #-}
module Handler.Mooc
  ( getMoocHomeR, postMoocHomeR
  ) where




import Import hiding ((/=.), (==.), (=.), on, isNothing, count)
import Handler.Mooc.BrowseProposals
#if EXPO
#else
import qualified Data.Maybe as Mb
import Database.Esqueleto
import Handler.Mooc.EdxLogin
import Handler.Mooc.User
import qualified Text.Blaze as Blaze
#endif



postMoocHomeR :: Handler TypedContent
#if EXPO
postMoocHomeR = toTypedContent <$> getBrowseProposalsR 1
#else
postMoocHomeR = do
  master <- getYesod
  yreq <- getRequest
  dispatchLti (appLTICredentials $ appSettings master) yreq
#endif

getMoocHomeR :: Handler TypedContent
#if EXPO
getMoocHomeR = toTypedContent <$> getBrowseProposalsR 1
#else
getMoocHomeR = toTypedContent <$> do
    setUltDestCurrent
    muser <- maybeAuth
    createAccW <- case muser of
      Nothing -> return mempty
      Just (Entity userId user) -> do
        return $ if Mb.isNothing $ userEmail user
                 then setupLocalAccountFromExistingUserW userId
                 else mempty

--    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    let urole = muserRole muser
    mSubmissionsWidget <- case muser of
      Just (Entity uId _) -> do
        submissions <- runDB $ fetchLastSubmissions $ noProposalParams {
                                 propLimit      = Just 1
                               , onlyByAuthorId = Just uId
                               , sortOrder      = Newest }
        widget <- mkSubmissionsWidget submissions
        return $ Just widget
         -- fmap listToMaybe $ runDB $ selectList [CurrentScenarioAuthorId ==. uId] []
      Nothing -> return Nothing

    someSubmissionsWidget <- do
      submissions <- runDB $ fetchLastSubmissions $ noProposalParams
                              { propLimit = Just 6 }
      mkSubmissionsWidget submissions

    newsItems <- renderNewsItems muser
    mvoteCountWidget <- renderVoteCountWidget muser

    let showEditorBtn = Mb.isNothing mSubmissionsWidget || urole /= UR_STUDENT

    fullLayout Nothing "Welcome to QUA-KIT!" $ do
        setTitle "qua-kit"
        toWidgetHead
          [hamlet|
            <meta property="og:url"         content="@{MoocHomeR}" />
            <meta property="og:type"        content="website" />
            <meta property="og:title"       content="Quick Urban Analysis kit" />
            <meta property="og:description" content="Qua-kit is an urban design, education, sharing, and analysis platform." />
            <meta property="og:image"       content="@{StaticR img_bgimg_png}" />
          |]
        toWidgetHead
          [cassius|
            .critIcon
              position: relative
              top: 5px
            .stars
              color: #ff6f00
            .newscard
              width: 100%
              padding: 0 10px
              .stars
                margin: 8px 0
              .commentPara
                margin-top: 3px
                >.icon24
                  margin-right: 3px
          |]
        $(widgetFile "mooc/home")


renderNewsItems :: Maybe (Entity User) -> Handler [(UTCTime, Widget)]
renderNewsItems Nothing = return []
renderNewsItems (Just (Entity uId _)) = do
  rs  <- fetchReviews uId
  eRs <- fetchExpertReviews uId
  bVs <- fetchBetterVotes uId
  wVs <- fetchWorseVotes uId
  let newestFirst (d1, _) (d2, _)
        | d1 < d2   = GT
        | otherwise = LT
  return $ sortBy newestFirst $ rs ++ eRs ++ bVs ++ wVs

-- | TODO: at some point, it is better to implement pagination instead of a hard limit
newsLimit :: Int64
newsLimit = 20


fetchExpertReviews :: UserId -> Handler [(UTCTime, Widget)]
fetchExpertReviews uId = do
  reviewScs <- runDB $ select $ from $ \(expReview `InnerJoin` scenario) -> do
                 on $ expReview ^. ExpertReviewScenarioId ==. scenario ^. ScenarioId
                 where_ $ scenario ^. ScenarioAuthorId ==. val uId
                 orderBy [desc $ expReview ^. ExpertReviewTimestamp]
                 limit newsLimit
                 return (expReview, scenario)
  let renderReview (Entity _ r, Entity _ sc) =
        let stars =
              let grade = expertReviewGrade r
              in  mconcat $
                (replicate grade [whamlet|<span.icon.icon-lg>star</span>|]) ++
                replicate (5 - grade) [whamlet|<span.icon.icon-lg>star_border</span>|]
            widget =
              [whamlet|
                ^{viewSubmissionBtn sc}
                <p .stars>
                  ^{stars}
                <p>#{expertReviewComment r}
              |]
        in  (expertReviewTimestamp r, widget)
  return $ map renderReview reviewScs

fetchReviews :: UserId -> Handler [(UTCTime, Widget)]
fetchReviews uId = do
  reviewData <- runDB $ select $
                  from $ \(review
                    `InnerJoin` scenario
                    `InnerJoin` criterion
                    `InnerJoin` user
                    ) -> do
                  on (review ^. ReviewReviewerId  ==. user      ^. UserId)
                  on (review ^. ReviewCriterionId ==. criterion ^. CriterionId)
                  on (review ^. ReviewScenarioId  ==. scenario  ^. ScenarioId)
                  where_ $ scenario ^. ScenarioAuthorId ==. val uId
                  orderBy [desc $ review ^. ReviewTimestamp]
                  limit newsLimit
                  return (review, scenario, criterion, user)
  let renderReview (Entity _ r, Entity _ sc, Entity _ crit, Entity _ reviewer) =
        let critIcon = Blaze.preEscapedToMarkup $ criterionIcon crit
            widget =
              [whamlet|
                ^{viewSubmissionBtn sc}
                <p .commentPara>
                  <span .critIcon>#{ critIcon }
                  <span class="icon icon24 text-brand-accent">
                    $if reviewPositive r
                      thumb_up
                    $else
                      thumb_down
                  Review from #{userName reviewer}
                <p>
                  #{reviewComment r}
              |]
        in  (reviewTimestamp r, widget)
  return $ map renderReview reviewData

fetchBetterVotes :: UserId -> Handler [(UTCTime, Widget)]
fetchBetterVotes = fetchVotesWithComment VoteBetterId renderReview
  where
    renderReview (Entity _ v, Entity _ sc) =
      let widget =
            [whamlet|
              ^{viewSubmissionBtn sc}
              <p>
                <strong>Your submission was voted better than another
                $maybe expl <- voteExplanation v
                  – #{expl}
            |]
      in (voteTimestamp v, widget)

fetchWorseVotes :: UserId -> Handler [(UTCTime, Widget)]
fetchWorseVotes = fetchVotesWithComment VoteWorseId renderReview
  where
    renderReview (Entity _ v, Entity _ sc) =
      let widget =
            [whamlet|
              ^{viewSubmissionBtn sc}
              <p>
                <strong>Another submission was voted even better than yours
                $maybe expl <- voteExplanation v
                  – #{expl}
            |]
      in  (voteTimestamp v, widget)

fetchVotesWithComment :: EntityField Vote ScenarioId
                      -> ((Entity Vote, Entity Scenario) -> (UTCTime, Widget))
                      -> UserId
                      -> Handler [(UTCTime, Widget)]
fetchVotesWithComment voteBetterOrWorse renderReview uId = do
  votes <- runDB $ select $ from $ \(vote `InnerJoin` scenario) -> do
             on $ vote ^. voteBetterOrWorse ==. scenario ^. ScenarioId
             where_ $ scenario ^. ScenarioAuthorId ==. val uId
                  &&. (not_ $ vote ^. VoteExplanation ==. nothing)
             orderBy [desc $ vote ^. VoteTimestamp]
             limit newsLimit
             return (vote, scenario)
  return $ map renderReview votes


renderVoteCountWidget :: Maybe (Entity User) -> Handler (Maybe Widget)
renderVoteCountWidget Nothing = return Nothing
renderVoteCountWidget (Just (Entity uId _)) = do
  let countVotes voteBetterOrWorse = do
        counts <- runDB $ select $ from $ \(vote `InnerJoin` scenario) -> do
                    on $ vote ^. voteBetterOrWorse ==. scenario ^. ScenarioId
                    where_ $ scenario ^. ScenarioAuthorId ==. val uId
                    return $ count $ vote ^. VoteId
        return $ case counts of
                   c:_ -> unValue c
                   []  -> 0::Int
  betterVoteCount <- countVotes VoteBetterId
  worseVoteCount  <- countVotes VoteWorseId
  let totalVoteCount = betterVoteCount + worseVoteCount
      widget =
        [whamlet|
          <div class="col-lg-4 col-md-6 col-sm-9">
            <div .card>
              <div .card-inner>
                <p>
                  Your submission was compared #{totalVoteCount} times to another:
                <p>
                  <span class="icon icon24 text-brand-accent">
                    thumb_up
                  #{betterVoteCount} times it was voted better,
                <p>
                  <span class="icon icon24 text-brand-accent">
                    thumb_down
                  #{worseVoteCount} times the other one was voted better.
        |]
  return $ if totalVoteCount > 0
           then Just widget
           else Nothing


viewSubmissionBtn :: Scenario -> Widget
viewSubmissionBtn sc = [whamlet|
  <div class="card-action-btn pull-right">
    <a href=@{ SubmissionR (scenarioExerciseId sc) (scenarioAuthorId sc) }>
      <span .icon>visibility
  |]

#endif
