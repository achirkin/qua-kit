-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.SubmitProposal
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.SubmitProposal
  ( postSubmitProposalR
  ) where

import Import


postSubmitProposalR :: Handler Html
postSubmitProposalR = do
  muser <- maybeAuth
  mgeometry <- lookupPostParam "geometry"
  mpreview  <- lookupPostParam "preview"
  defaultLayout $ do
    toWidgetBody
      [hamlet|
        $maybe (Entity _ user) <- muser
          <div>
            #{userName user}
        $maybe preview <- mpreview
          <div>
            <img src="#{preview}" style="width: 500px;">
        $maybe geom <- mgeometry
          <div>
            #{geom}
      |]
