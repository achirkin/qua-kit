{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Criteria
  ( getCriteriaListR
  , getCriteriaImgR
  ) where

import Import
import Text.Blaze as Blaze
import Database.Persist.Sql

getCriteriaListR :: Handler Html
getCriteriaListR = do
    scp_id <- getCurrentScenarioProblem

    criteria <- runDB $ currentCriteria scp_id
    oldCriteria <- runDB $ otherCriteria scp_id

    fullLayout Nothing "Qua-kit design criteria" $ do
      setTitle "Qua-kit design criteria"
      toWidgetBody $
        [hamlet|
          <div class="ui-card-wrap">
            <div class="row">
              $forall crit <- criteria
                ^{criterionCard crit}

          <div class="ui-card-wrap">
            <div class="row">
              <div class="col-lg-7 col-md-8 col-sm-9">
                <div class="card card-red">
                  <div class="card-main">
                    <div class="card-inner">
                      <p>
                        Next, you can find a list of criteria we used
                        for the previous runs of this online course.
                        <br>
                        You do not need them, but we keep the whole list here for the
                        sake of completeness.

          <div class="ui-card-wrap">
            <div class="row">
              $forall crit <- oldCriteria
                ^{criterionCard crit}
        |]
  where
    criterionCard (Entity cId criterion) =
      [hamlet|
        <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
          <div class="card">
            <aside class="card-side card-side-img pull-left card-side-moocimg">
              <img src="@{CriteriaImgR cId}">
            <div class="card-main">
              <div.card-header>
                <div.card-inner>
                  <h5.h5.margin-top-no.text-brand-accent style="display: inline-block; vertical-align: middle; margin-bottom: -4px;">
                    #{Blaze.preEscapedToMarkup $ criterionIcon criterion}
                  <h5.h5.margin-bottom-no.margin-top-no.text-brand-accent style="display: inline; vertical-align: middle">
                    #{criterionName criterion}
              <div class="card-inner" style="margin: 10px 12px;">
                #{preEscapedText $ criterionDescription criterion}
      |]

getCriteriaImgR :: CriterionId -> Handler TypedContent
getCriteriaImgR ident = do
    criterion <- runDB $ get404 ident
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ criterionImage criterion)



currentCriteria :: ScenarioProblemId -> ReaderT SqlBackend Handler [Entity Criterion]
currentCriteria scp_id = rawSql query [toPersistValue scp_id]
  where
    query = unlines
        ["SELECT ?? FROM criterion,problem_criterion"
        ,"         WHERE problem_criterion.problem_id = ?"
        ,"           AND problem_criterion.criterion_id = criterion.id;"
        ]

otherCriteria :: ScenarioProblemId -> ReaderT SqlBackend Handler [Entity Criterion]
otherCriteria scp_id = rawSql query [toPersistValue scp_id]
  where
    query = unlines
        ["SELECT ??"
        ,"  FROM criterion"
        ," WHERE criterion.id NOT IN"
        ," ("
        ,"  SELECT criterion.id FROM criterion,problem_criterion"
        ,"                     WHERE problem_criterion.problem_id = ?"
        ,"                       AND problem_criterion.criterion_id = criterion.id"
        ," );"
        ]
