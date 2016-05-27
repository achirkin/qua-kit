-----------------------------------------------------------------------------
-- |
-- Module      :  Luci.Messages
-- Copyright   :  Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Luci.Messages
    ( -- * Messages
      RemoteRegister (..)
    ) where

import Data.Text
import Data.Aeson

data RemoteRegister = RemoteRegister
  { exampleCall :: !Value
  , serviceName :: !Text
  , inputs  :: !(Maybe Value)
  , outputs :: !(Maybe Value)
  }

instance ToJSON RemoteRegister where
  toJSON RemoteRegister{..} = object $
      "exampleCall" .= exampleCall
    : "serviceName" .= serviceName
    : ( maybeAdd ( ( "inputs"  .=) <$> inputs)
      $ maybeAdd ( ( "outputs" .=) <$> outputs) [])


maybeAdd :: Maybe a -> [a] -> [a]
maybeAdd Nothing = id
maybeAdd (Just x) = (x:)
