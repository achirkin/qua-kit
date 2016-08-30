-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.About
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.About
  ( getAboutR
  ) where

import Import

getAboutR :: Handler Html
getAboutR =
    fullLayout Nothing "About qua-kit" $ do
      setTitle "About qua-kit"
      toWidgetBody $
        [hamlet|
          <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9 story_cards">
              <div class="card">
                <div class="card-main">
                  <div class="card-header">
                    <div class="card-inner">
                      <h3 class="h5 margin-bottom-no margin-top-no">
                        About us
                  <div class="card-inner">
                    Here I will add some info about the project and us.
        |]
