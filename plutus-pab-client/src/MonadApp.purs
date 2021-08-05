module MonadApp where

import Prelude
import Types (HAction, Output(..), State, WebData, _contractInstanceIdString)
import Animation (class MonadAnimate, animate)
import Clipboard (class MonadClipboard, copy)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Lens (view)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen (HalogenM, liftEffect, raise)
import Network.RemoteData as RemoteData
import Playground.Lenses (_getEndpointDescription)
import Plutus.PAB.Webserver (SPParams_, getApiContractInstanceByContractinstanceidSchema, getApiFullreport, getApiContractInstances, getApiContractDefinitions, postApiContractActivate, postApiContractInstanceByContractinstanceidEndpointByEndpointname, getApiContractInstanceByContractinstanceidStatus)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState, ContractSignatureResponse, FullReport, CombinedWSStreamToServer, ContractActivationArgs)
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Wallet.Types (EndpointDescription, ContractInstanceId)
import ContractExample (ExampleContracts)

class
  Monad m <= MonadApp m where
  getFullReport :: m (WebData (FullReport ExampleContracts))
  getContractInstanceStatus :: ContractInstanceId -> m (WebData (ContractInstanceClientState ExampleContracts))
  getContractInstanceClientState :: m (WebData (Array (ContractInstanceClientState ExampleContracts)))
  getContractDefinitions :: m (WebData (Array (ContractSignatureResponse ExampleContracts)))
  invokeEndpoint :: RawJson -> ContractInstanceId -> EndpointDescription -> m (WebData Unit)
  activateContract :: ContractActivationArgs ExampleContracts -> m (WebData ContractInstanceId)
  sendWebSocketMessage :: CombinedWSStreamToServer -> m Unit
  log :: String -> m Unit

newtype HalogenApp m a
  = HalogenApp (HalogenM (State ExampleContracts) (HAction ExampleContracts) () Output m a)

derive instance newtypeHalogenApp :: Newtype (HalogenApp m a) _

derive newtype instance functorHalogenApp :: Functor (HalogenApp m)

derive newtype instance applicativeHalogenApp :: Applicative (HalogenApp m)

derive newtype instance applyHalogenApp :: Apply (HalogenApp m)

derive newtype instance bindHalogenApp :: Bind (HalogenApp m)

derive newtype instance monadHalogenApp :: Monad (HalogenApp m)

derive newtype instance monadTransHalogenApp :: MonadTrans HalogenApp

derive newtype instance monadStateHalogenApp :: MonadState (State ExampleContracts) (HalogenApp m)

derive newtype instance monadAskHalogenApp :: MonadAsk env m => MonadAsk env (HalogenApp m)

derive newtype instance monadEffectHalogenApp :: MonadEffect m => MonadEffect (HalogenApp m)

derive newtype instance monadAffHalogenApp :: MonadAff m => MonadAff (HalogenApp m)

instance monadAnimateHalogenApp :: MonadAff m => MonadAnimate (HalogenApp m) (State ExampleContracts) where
  animate toggle action = HalogenApp $ animate toggle (unwrap action)

instance monadClipboardHalogenApp :: MonadEffect m => MonadClipboard (HalogenApp m) where
  copy = liftEffect <<< copy

------------------------------------------------------------
runHalogenApp :: forall m a. HalogenApp m a -> HalogenM (State ExampleContracts) (HAction ExampleContracts) () Output m a
runHalogenApp = unwrap

instance monadAppHalogenApp :: (MonadAff m, MonadAsk (SPSettings_ SPParams_) m) => MonadApp (HalogenApp m) where
  getFullReport = runAjax getApiFullreport
  getContractInstanceStatus contractInstanceId =
    runAjax
      $ getApiContractInstanceByContractinstanceidStatus
          (view _contractInstanceIdString contractInstanceId)
  getContractInstanceClientState = runAjax getApiContractInstances
  getContractDefinitions = runAjax getApiContractDefinitions
  invokeEndpoint payload contractInstanceId endpointDescription =
    runAjax
      $ postApiContractInstanceByContractinstanceidEndpointByEndpointname
          payload
          (view _contractInstanceIdString contractInstanceId)
          (view _getEndpointDescription endpointDescription)
  activateContract contract = runAjax $ postApiContractActivate contract
  sendWebSocketMessage msg = HalogenApp $ raise $ SendWebSocketMessage msg
  log str = liftEffect $ Console.log str

runAjax :: forall m a. Functor m => ExceptT AjaxError m a -> m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action
