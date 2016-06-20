-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Luci.Connect
import Luci.Connect.Base
import Luci.Messages
import Data.Aeson
import Data.Aeson.Types
import Data.Text

main :: IO ()
main = runReallySimpleLuciClient () $ do
    yieldMessage $ simpleMessage RemoteRegister
      { exampleCall = object
                [ "run" .= ("AddingNumbers" :: Text)
                , "x" .= (5 :: Double)
                , "y" .= (2.5 :: Double)
                ]
      , serviceName = "AddingNumbers"
      , inputs = Just $ object [ "x" .= ("number" :: Text)
                               , "y" .= ("number" :: Text)
                               , "run" .= ("AddingNumbers" :: Text)
                               ]
      , outputs = Just $ object [ "XOR result" .= ("number" :: Text)
                                , "XOR error" .= ("string" :: Text)
                                ]
      }
    awaitForever calculate
  where
    calculate msg = case fromMessage msg of
       Error _   -> yield $ Processing msg
       Success (RunCalculate x y) -> yieldMessage . simpleMessage . object $
         [ "result" .= object [ "sum" .= (x + y) ]
         ]


data RunCalculate = RunCalculate Double Double


instance FromJSON RunCalculate where
  parseJSON (Object v) = RunCalculate
                   <$> v .: "x"
                   <*> v .: "y"
  parseJSON invalid = typeMismatch "RunCalculate" invalid
