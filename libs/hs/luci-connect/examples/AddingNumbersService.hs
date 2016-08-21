-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- An implementation of a simple luci service.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Luci.Connect
import Luci.Messages
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack)
import Data.Monoid ((<>))

main :: IO ()
main = runReallySimpleLuciClient () $ do
    yield $ registerMessage 14
    awaitForever calculate
  where
    calculate (MsgRun token "AddingNumbers" pams _)
      = case fromJSON (Object pams) of
          Success (RunCalculate x y) ->
             yield $ MsgResult token (resultJSON [ "sum" .= (x + y) ]) []
          Error err ->
             yield $ MsgError token $ "Cannot parse run message: " <> pack err
    calculate msg@(MsgRun token _ _ _) =
             yield $ MsgError token $ "Unexpected run message " <> showJSON (toJSON . fst $ makeMessage msg)
    calculate msg = logInfoN $ "Ignoring message " <> showJSON (toJSON . fst $ makeMessage msg)

data RunCalculate = RunCalculate Double Double
instance FromJSON RunCalculate where
  parseJSON (Object v) = RunCalculate
                   <$> v .: "x"
                   <*> v .: "y"
  parseJSON invalid = typeMismatch "RunCalculate" invalid


-- | A message we send to register in luci
registerMessage :: Token -> Message
registerMessage token = MsgRun token "RemoteRegister" o []
  where
    o = objectJSON
      [ "description" .= String "adding two numbers together"
      , "serviceName" .= String "AddingNumbers"
      , "inputs"      .= object
          [ "x"   .= String "number"
          , "y"   .= String "number"
          ]
      , "outputs"     .= object
          [ "sum" .= String "number"
          ]
      , "exampleCall" .= object
          [ "run" .= String "AddingNumbers"
          , "x"   .= Number 1
          , "y"   .= Number 2.5
          ]
      ]
