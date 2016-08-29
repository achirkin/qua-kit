-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.EditProposal
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.EditProposal
  ( getEditProposalR
  ) where

import Import

getEditProposalR :: Handler Html
getEditProposalR = do
  setSession "custom_exercise_type" "design"
  redirect HomeR
