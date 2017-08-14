{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc
  ( getMoocHomeR, postMoocHomeR
  ) where




--import qualified Data.Map as Map

import qualified Data.Function as Fn
import qualified Data.Maybe as Mb
import Database.Esqueleto
import Import hiding ((/=.), (==.), (=.), on, isNothing)
import Handler.Mooc.BrowseProposals
import Handler.Mooc.EdxLogin
import Handler.Mooc.User
import qualified Text.Blaze as Blaze


postMoocHomeR :: Handler TypedContent
postMoocHomeR = do
  master <- getYesod
  yreq <- getRequest
  dispatchLti (appLTICredentials $ appSettings master) yreq

getMoocHomeR :: Handler TypedContent
getMoocHomeR  = toTypedContent <$> do
    setUltDestCurrent
    muser <- maybeAuth
    createAccW <- case muser of
      Nothing -> return mempty
      Just (Entity userId user) -> do
        return $ if Mb.isNothing $ userEmail user
                 then setupLocalAccountW userId
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

    newsItems <- renderNewsItems muser

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
          |]
        $(widgetFile "mooc/home")

renderNewsItems :: Maybe (Entity User) -> Handler [(UTCTime, Widget)]
renderNewsItems Nothing = return []
renderNewsItems (Just (Entity uId _)) = do
  rs  <- fetchReviews uId
  eRs <- fetchExpertReviews uId
  bVs <- fetchBetterVotes uId
  wVs <- fetchWorseVotes uId
  return $ sortBy (Fn.on compare fst) $ rs ++ eRs ++ bVs ++ wVs

fetchExpertReviews :: UserId -> Handler [(UTCTime, Widget)]
fetchExpertReviews uId = do
  reviewScs <- runDB $ select $ from $ \(expReview `InnerJoin` scenario) -> do
                 on $ expReview ^. ExpertReviewScenarioId ==. scenario ^. ScenarioId
                 where_ $ scenario ^. ScenarioAuthorId ==. val uId
                 limit 20
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
                    ) -> do
                  on (review ^. ReviewCriterionId ==. criterion ^. CriterionId)
                  on (review ^. ReviewScenarioId  ==. scenario  ^. ScenarioId)
                  where_ $ scenario ^. ScenarioAuthorId ==. val uId
                  limit 20
                  return (review, scenario, criterion)
  let renderReview (Entity _ r, Entity _ sc, Entity _ crit) =
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
                  #{reviewComment r}
              |]
        in  (reviewTimestamp r, widget)
  return $ map renderReview reviewData

fetchBetterVotes :: UserId -> Handler [(UTCTime, Widget)]
fetchBetterVotes uId = do
  betterVotes <- runDB $ select $ from $ \(vote `InnerJoin` scenario) -> do
                   on $ vote ^. VoteBetterId ==. scenario ^. ScenarioId
                   where_ $ scenario ^. ScenarioAuthorId ==. val uId
                   limit 20
                   return (vote, scenario)
  let renderReview (Entity _ v, Entity _ sc) =
        let widget =
              [whamlet|
                ^{viewSubmissionBtn sc}
                <p>
                  <strong>Your submission was voted better than another
                  $maybe expl <- voteExplanation v
                    – #{expl}
              |]
        in  (voteTimestamp v, widget)
  return $ map renderReview betterVotes

fetchWorseVotes :: UserId -> Handler [(UTCTime, Widget)]
fetchWorseVotes uId = do
  betterVotes <- runDB $ select $ from $ \(vote `InnerJoin` scenario) -> do
                   on $ vote ^. VoteWorseId ==. scenario ^. ScenarioId
                   where_ $ scenario ^. ScenarioAuthorId ==. val uId
                   limit 20
                   return (vote, scenario)
  let renderReview (Entity _ v, Entity _ sc) =
        let widget =
              [whamlet|
                ^{viewSubmissionBtn sc}
                <p>
                  <strong>Another submission was voted even better than yours
                  $maybe expl <- voteExplanation v
                    – #{expl}
              |]
        in  (voteTimestamp v, widget)
  return $ map renderReview betterVotes

viewSubmissionBtn :: Scenario -> Widget
viewSubmissionBtn sc = [whamlet|
  <div class="card-action-btn pull-right">
    <a href=@{ SubmissionViewerR (scenarioTaskId sc) (scenarioAuthorId sc) }>
      <span .icon>visibility
  |]
