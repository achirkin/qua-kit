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
import Text.Blaze

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
                <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
                  <div class="card">
                    <aside class="card-side card-side-img pull-left card-side-moocimg">
                      <img src="@{CriteriaImgR cId}">
                    <div class="card-main">
                      <div.card-header>
                        <div.card-inner>
                          <h5.h5.margin-bottom-no.margin-top-no.text-brand-accent>
                            #{criterionName criterion}
                      <div class="card-inner" style="margin: 10px 12px;">
                        #{preEscapedText $ criterionDescription criterion}
        |]

getCriteriaImgR :: CriterionId -> Handler TypedContent
getCriteriaImgR ident = do
    criterion <- runDB $ get404 ident
    addHeader "Content-Disposition" "inline"
    sendResponse (("image/png" :: ByteString), toContent $ criterionImage criterion)
