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
import Database.Persist.Sql (toSqlKey)
import Data.Text.Read (decimal)

import qualified Data.ByteString.Base64 as BSB (decodeLenient)


-- | Submit a design proposal
--   session params:
--     * qua_view_mode=edit -- means a user works on a scenario in edit mode => can save something
--     * scenario_id -- id of previous submission
--   post params:
--     * geometry    -- geoJSON, obligatory
--     * preview     -- image, obligatory
--     * description -- text, optional

postSubmitProposalR :: Handler Html
postSubmitProposalR = do
  Entity uId user <- requireAuth
  geometry <- requirePostParam "geometry" "Geometry input is not found!"
  preview  <- requirePostParam "preview" "Preview input is not found!"
  description  <- lookupPostParam "description"

  qua_view_mode <- requireSession "qua_view_mode" "You must be in an 'edit' mode to save scenario."
  when (qua_view_mode /= "edit") $ invalidArgsI ["You must be in an 'edit' mode to save scenario." :: Text]
  escenario_id   <- decimal <$> requireSession "scenario_id" "I do not know wich scenario you were working on."
  deleteSession "scenario_id"
  case escenario_id of
    Left _ -> invalidArgsI ["Cannot parse custom_exercise_id. Please, consult the developer." :: Text]
    Right (scenario_id, _) -> runDB $ do
      prevScenario <- get404 $ toSqlKey scenario_id
      when (uId /= scenarioAuthorId prevScenario) $
        invalidArgsI ["You can work only on your own scenarios!" :: Text]
      t <- liftIO getCurrentTime
      insert_ $ prevScenario
             { scenarioImage = BSB.decodeLenient . encodeUtf8 . drop 1 $ dropWhile (',' /=) preview
             , scenarioDescription = fromMaybe "" description
             , scenarioGeometry = encodeUtf8 geometry
             , scenarioLastUpdate = t
             }
      setMessage . toHtml $ "Thank you, " <> userName user <> ", your design proposal has been saved."
  redirectUltDest MoocHomeR


requireSession :: Text -> Text -> Handler Text
requireSession pam errstr = lookupSession pam >>= \mv -> case mv of
    Nothing -> invalidArgsI [errstr]
    Just v  -> return v


requirePostParam :: Text -> Text -> Handler Text
requirePostParam pam errstr = lookupPostParam pam >>= \mv -> case mv of
    Nothing -> invalidArgsI [errstr]
    Just v  -> return v
