{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Tests
  ( getSimulateDesignExerciseR
  , getSimulateCompareExerciseR
  , getLoginAsUserIdR
  ) where

import Import
import System.Random


getSimulateDesignExerciseR :: Handler TypedContent
getSimulateDesignExerciseR = do
  name <- genRandomName
  clearSession
  setCredsRedirect
          . Creds "lti" name
          $  [("resource_link_id"       , "courses.edx.org-a5822022c0724e3ba24a416600299056")
             ,("context_id"             , "course-v1:ETHx+FC-01x+2T2015")
             ,("lis_outcome_service_url", "https://courses.edx.org/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@a58220dfgh00299056/handler_noauth/outcome_service_handler")
             ,("lis_result_sourcedid"   , "course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-a5822022c0724e3ba24a416600299056:a9f4ca11f3b6")
             ,("custom_exercise_type", "design")
             ,("custom_exercise_id", "1")
             ]


getSimulateCompareExerciseR :: Handler TypedContent
getSimulateCompareExerciseR = do
  name <- genRandomName
  clearSession
  setCredsRedirect
          . Creds "lti" name
          $  [("resource_link_id"       , "courses.edx.org-ebec3307675542c08b337cb15c47b96b")
             ,("context_id"             , "course-v1:ETHx+FC-01x+2T2015")
             ,("lis_outcome_service_url", "https://courses.edx.org/courses/course-v1:ETHx+FC-01x+2T2015/xblock/block-v1:ETHx+FC-01x+2T2015+type@lti_consumer+block@a58220dfgh00299056/handler_noauth/outcome_service_handler")
             ,("lis_result_sourcedid"   , "course-v1%3AETHx%2BFC-01x%2B2T2015:courses.edx.org-a5822022c0724e3ba24a416600299056:a9f4ca11f3b6")
             ,("custom_exercise_type", "compare")
             ,("custom_exercise_id", "1")
             ,("custom_exercise_count", "10")
             ]

getLoginAsUserIdR :: UserId -> Handler TypedContent
getLoginAsUserIdR userId = do
  u <- runDB $ get404 userId
  clearSession
  setCredsRedirect $ Creds "lti" (fromMaybe "not an edX user" $ userEdxUserId u) []


genRandomName :: Handler Text
genRandomName = fmap pack . liftIO $ foldM (\t _ -> (:t) <$> randomRIO ('a','z')) "" [1..16::Int]
