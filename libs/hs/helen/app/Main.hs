-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
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
                [ "run" .= ("helen" :: Text)
                , "x" .= (4 :: Double)
                , "y" .= (2.5 :: Double)
                ]
      , serviceName = "helen"
      , description = "adding two numbers together"
      , inputs = Just $ object [ "x" .= ("number" :: Text)
                               , "y" .= ("number" :: Text)
                               , "run" .= ("helen" :: Text)
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
