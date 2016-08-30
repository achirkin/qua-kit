-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.Criteria
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.Criteria
  ( getCriteriaListR
  , getCriteriaImgR
  ) where

import Import

getCriteriaListR :: Handler Html
getCriteriaListR = do
    criteria <- runDB $ selectList [] []
    fullLayout Nothing "Qua-kit design criteria" $ do
      setTitle "Qua-kit design criteria"
      toWidgetBody $
        [hamlet|
          <div class="ui-card-wrap">
            <div class="row">
              $forall (Entity cId criterion) <- criteria
                <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9 story_cards">
                  <div class="card">
                    <aside class="card-side card-side-img pull-left card-side-moocimg">
                      <img src="@{CriteriaImgR cId}">
                    <div class="card-main">
                      <div class="card-inner" style="margin: 10px 12px;">
                        <p style="margin: 6px 0px;">
                          #{criterionName criterion}
                        <p style="margin: 6px 0px; white-space: pre-line; overflow-y: hidden; color: #555;">
                         #{criterionDescription criterion}
        |]

getCriteriaImgR :: CriterionId -> Handler TypedContent
getCriteriaImgR ident = do
    criterion <- runDB $ get404 ident
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ criterionImage criterion)
