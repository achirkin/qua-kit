-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Feedback
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Feedback
  ( getFeedbackR
  ) where


import Import

getFeedbackR :: Handler Html
getFeedbackR = fullLayout Nothing "Feedback" $ do
      setTitle "Feedback"
      toWidgetBody $
        [hamlet|
          <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9 story_cards">
              <div class="card">
                <div class="card-main">
                  <div class="card-header">
                    <div class="card-inner">
                      <h3 class="h5 margin-bottom-no margin-top-no">
                        Send us a message
                  <div class="card-inner">
                    Here I will add feedback form
        |]
