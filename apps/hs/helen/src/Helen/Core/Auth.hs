module Helen.Core.Auth
    ( enforceQuaKitRoles
    ) where

import Control.Lens ((.~))
import Control.Monad.State
import Data.Function
import qualified Data.Set as Set
import qualified Network.Socket as Network

import Luci.Messages
       (AuthRole(Local), Message, msgSenderAuthRole, msgSenderId)

import Helen.Core.Types

enforceQuaKitRoles :: Network.SockAddr -> Message -> HelenRoom Message
enforceQuaKitRoles client message = do
    trusteds <- gets trustedClients
    pure $ if Set.member client trusteds
           then message
           else message & msgSenderId .~ Nothing
                        & msgSenderAuthRole .~ Local
