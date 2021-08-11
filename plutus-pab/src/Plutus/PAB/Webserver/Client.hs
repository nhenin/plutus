{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | Servant client for PAB
module Plutus.PAB.Webserver.Client (
    PabClient(..)
  , InstanceClient(..)
  , pabClient
) where

import           Data.Aeson                 (FromJSON, ToJSON (..))
import qualified Data.Aeson                 as JSON
import           Data.Proxy
import           Data.Text                  (Text)
import           Plutus.PAB.Events.Contract
import           Plutus.PAB.Webserver.API
import           Plutus.PAB.Webserver.Types
import           Servant.API
import           Servant.Client

-- | Client for PAB. The first type-argument is contract type that is used for PAB-simulator.
data PabClient t walletId = PabClient
  { healthcheck      :: ClientM ()
      -- ^ call healthcheck method
  , fullreport       :: ClientM (FullReport t)
      -- ^ call fullreport method
  , activateContract :: ContractActivationArgs t -> ClientM ContractInstanceId
      -- ^ call activate contract method
  , instanceClient   :: Text -> InstanceClient t
      -- ^ call methods for instance client. We should turn @ContractInstanceId@ to @Text@ for the first argument.
  , getWallet        :: walletId -> ClientM [ContractInstanceClientState t]
      -- ^ get wallet
  , getInstances     :: ClientM [ContractInstanceClientState t]
      -- ^ get instance
  , getDefinitions   :: ClientM [ContractSignatureResponse t]
      -- ^ get definitions
  }

-- TODO: ??? Review the PR for why we didn't like this.
--
--
-- instance ToHttpApiData ContractInstanceId where
--   toUrlPiece = undefined -- unContractInstanceId

-- data WebSocketClient = WebSocketClient
--   { contractInstanceUpdates :: ContractInstanceId -> Client ClientM WebSocketPending
--   , combinedWebsocket :: Client ClientM WebSocketPending
--   }

-- TODO: Discussing with Torsten, this is a bit out of my league to be able to
-- implement.
-- instance HasClient ClientM WebSocketPending where
--   clientWithRoute = undefined
--   hoistClientMonad _ _ f ma = f ma

-- wsClient :: WebSocketClient
-- wsClient = WebSocketClient{..}
--   where
--     ( contractInstanceUpdates :<|> combinedWebsocket ) = client (Proxy @WSAPI)


-- | Contract instance endpoints
data InstanceClient t = InstanceClient
  { getInstanceStatus    :: ClientM (ContractInstanceClientState t)
      -- ^ get instance status
  , getInstanceSchema    :: ClientM (ContractSignatureResponse t)
      -- ^ get instance schema
  , callInstanceEndpoint :: String -> JSON.Value -> ClientM ()
      -- ^ call instance endpoint
  , stopInstance         :: ClientM ()
      -- ^ call stop instance method
  }

-- | Init generic pab client
pabClient :: forall t walletId. (ToJSON t, FromJSON t, ToHttpApiData walletId) => PabClient t walletId
pabClient = PabClient{..}
  where
    (healthcheck
      :<|> fullreport
      :<|> activateContract
      :<|> toInstanceClient
      :<|> getWallet
      :<|> getInstances
      :<|> getDefinitions
      ) = client (Proxy @(API t walletId))

    instanceClient cid = InstanceClient{..}
        where
          (getInstanceStatus
            :<|> getInstanceSchema
            :<|> callInstanceEndpoint
            :<|> stopInstance
            ) = toInstanceClient cid
