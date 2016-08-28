-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.BrowseProposals
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.BrowseProposals
  ( getBrowseProposalsR
  ) where

import Import

getBrowseProposalsR :: Handler Html
getBrowseProposalsR = do
    scenarios <- runDB $ selectList [] []
    defaultLayout $ do
      setTitle "EdX User Stories"
      toWidgetBody $
        [hamlet|
          $forall (Entity _ scenario) <- scenarios
            <div>
              <img src="#{decodeUtf8 $ scenarioImage scenario}" height=400>
              <div>
                #{scenarioDescription scenario}
        |]

