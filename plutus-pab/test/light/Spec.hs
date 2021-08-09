module Main
    ( main
    ) where

import qualified Cardano.Api.NetworkId.ExtraSpec
import qualified Cardano.Metadata.ServerSpec
import qualified Cardano.Metadata.TypesSpec
import qualified Cardano.Wallet.ServerSpec
import qualified Control.Concurrent.STM.ExtraSpec
import           Test.Tasty                       (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Cardano.Api.NetworkId.ExtraSpec.tests
        , Cardano.Metadata.ServerSpec.tests
        , Cardano.Metadata.TypesSpec.tests
        , Cardano.Wallet.ServerSpec.tests
        , Control.Concurrent.STM.ExtraSpec.tests
        ]
