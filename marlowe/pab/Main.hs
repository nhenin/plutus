{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main(main) where

import           Control.Monad                       (guard, void)
import qualified Data.Aeson                          as JSON
import qualified Data.Aeson.Types                    as JSON
import qualified Data.Map                            as Map
import           Data.Maybe                          (listToMaybe)
import qualified Language.Marlowe.Client             as Marlowe
import           Language.Marlowe.Semantics          (Action (..), Case (..), Contract (..), MarloweParams, Party (..),
                                                      Payee (..), Value (..))
import qualified Language.Marlowe.Semantics          as Marlowe
import           Language.Marlowe.Util               (ada)
import           Ledger                              (PubKeyHash, Slot, pubKeyHash)
import qualified Ledger.Value                        as Val
import           MarloweContract                     (MarloweContract (..), handlers)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified PlutusTx.AssocMap                   as AssocMap

main :: IO ()
main = runWith (Builtin.handleBuiltin @MarloweContract)
