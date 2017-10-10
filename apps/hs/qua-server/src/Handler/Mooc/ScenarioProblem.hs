{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.ScenarioProblem
  ( getScenarioProblemR
  ) where

import Import

getScenarioProblemR :: ScenarioProblemId -> Handler Text
getScenarioProblemR scpId = maybe "{}" (decodeUtf8 . scenarioProblemGeometry) <$> runDB (get scpId)
