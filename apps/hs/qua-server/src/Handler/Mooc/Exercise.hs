{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Exercise
  ( getExerciseR
  ) where

import Import

getExerciseR :: ExerciseId -> Handler Text
getExerciseR exId = maybe "{}" (decodeUtf8 . exerciseGeometry) <$> runDB (get exId)
