-----------------------------------------------------------------------------
--
-- Module      :  Handler.Mooc.Scenario
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Handler.Mooc.Scenario
  ( getScenarioR
  ) where

import Import

getScenarioR :: ScenarioProblemId -> Handler Text
getScenarioR scpId = do
  muserId <- maybeAuthId
  resource_link_id        <- lookupSession "resource_link_id"
  lis_outcome_service_url <- lookupSession "lis_outcome_service_url"
  lis_result_sourcedid    <- lookupSession "lis_result_sourcedid"
  case muserId of
    Nothing -> return ""
    Just uId -> fmap (fromMaybe "" . (fmap (decodeUtf8 . scenarioGeometry))) . runDB $ do
      mscenario <- selectSource
          [ ScenarioAuthorId ==. uId
          , ScenarioTaskId   ==. scpId
          ]
          [ Desc ScenarioLastUpdate
          ] $$ await
      case mscenario of
        Just scenario -> return $ Just $ entityVal scenario
        Nothing ->
          case resource_link_id of
             Nothing -> return Nothing
             Just rli -> do
               mscproblem <- get scpId
               medxres <- getBy $ EdxResLinkId rli
               case (,) <$> mscproblem <*> medxres of
                 Nothing -> return Nothing
                 Just (scp, Entity edxResId _) -> do
                   t <- liftIO getCurrentTime
                   scId <- insert $ Scenario
                                     uId
                                     scpId
                                     (scenarioProblemImage scp)
                                     (scenarioProblemGeometry scp)
                                     (scenarioProblemDescription scp)
                                     (scenarioProblemScale scp)
                                     edxResId
                                     lis_outcome_service_url
                                     lis_result_sourcedid
                                     t
                   get scId
