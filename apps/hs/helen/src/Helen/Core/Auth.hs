module Helen.Core.Auth
    ( enforceQuaKitRoles
    ) where

import Control.Lens ((.~))
import Control.Monad.State
import Data.Function
import qualified Data.Set as Set
import qualified Network.Socket as Network
import qualified Data.IP as IP

import Luci.Messages
       (AuthRole(Local), Message, msgSenderAuthRole, msgSenderId)

import Helen.Core.Types

enforceQuaKitRoles :: Network.SockAddr -> Message -> HelenRoom Message
enforceQuaKitRoles client message = do
    trusteds <- gets trustedClients
    let stripAuth message = message & msgSenderId .~ Nothing
                                    & msgSenderAuthRole .~ Local
    pure $ ($ message) $ case client of
        Network.SockAddrInet _ ha -> if Set.member (IP.fromHostAddress ha) trusteds
           then id
           else stripAuth
        _ -> stripAuth

