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

import Luci.Connect.Internal

-- | Register a service in Luci
data RemoteRegister = RemoteRegister
  { exampleCall :: !Value
  , serviceName :: !Text
  , inputs  :: !(Maybe Value)
  , outputs :: !(Maybe Value)
  }

instance ToJSON RemoteRegister where
  toJSON RemoteRegister{..} = objectM
    [ "run"         .=! ("RemoteRegister" :: Text)
    , "exampleCall" .=! exampleCall
    , "serviceName" .=! serviceName
    , "inputs"      .=? inputs
    , "outputs"     .=? outputs
    ]



--Right (Object (fromList [("newCallID",Number 10.0)]),[])
--Right (Object (fromList [("instanceID",Number 0.0),("progress",Object (fromList [])),("callID",Number 10.0),("serviceName",String "RemoteRegister"),("percentage",Number 0.0)]),[])
--Right (Object (fromList [("instanceID",Number 0.0),("result",Object (fromList [("registeredName",String "CoolTestService")])),("callID",Number 10.0),("serviceName",String "RemoteRegister")]),[])


