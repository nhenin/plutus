{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Tx
    ( module Export
    , ChainIndexTxOut(..)
    , _PublicKeyChainIndexTxOut
    , _ScriptChainIndexTxOut
    , toTxOut
    , getTxOutAddr
    , getTxOutValue
    -- * Transactions
    , addSignature
    , pubKeyTxOut
    , scriptTxOut
    , scriptTxOut'
    , updateUtxo
    , txOutRefs
    , unspentOutputsTx
    -- ** Hashing transactions
    , txId
    ) where

import           Cardano.Crypto.Hash       (SHA256, digest)
import qualified Codec.CBOR.Write          as Write
import           Codec.Serialise.Class     (Serialise, encode)
import           Control.Lens
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))
import           GHC.Generics              (Generic)
import           Ledger.Address            (pubKeyAddress, scriptAddress)
import           Ledger.Crypto             (PrivateKey, PubKey, signTx, toPublicKey)
import           Ledger.Scripts            (datumHash)
import           Plutus.V1.Ledger.Api      (Datum, DatumHash, TxId (..), Validator, ValidatorHash, Value, toBuiltin)
import           Plutus.V1.Ledger.Tx       as Export

-- | Transaction output that comes from a chain index query.
--
-- It is defined here instead of the plutus-chain-index because plutus-ledger
-- uses that datatype, and plutus-chain-index can't depend on plutus-ledger
-- because of a cyclic dependency.
data ChainIndexTxOut =
    PublicKeyChainIndexTxOut { pkOutAddress :: Address, pkOutValue :: Value }
  | ScriptChainIndexTxOut { scOutAddress   :: Address
                          , scOutValidator :: Either ValidatorHash Validator
                          , scOutDatum     :: Either DatumHash Datum
                          , scOutValue     :: Value
                          }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

makePrisms ''ChainIndexTxOut

-- | Converts a transaction output from the chain index the plutus ledger transaction
-- output.
toTxOut :: ChainIndexTxOut -> TxOut
toTxOut (PublicKeyChainIndexTxOut addr v)          = TxOut addr v Nothing
toTxOut (ScriptChainIndexTxOut addr _ (Left dh) v) = TxOut addr v (Just dh)
toTxOut (ScriptChainIndexTxOut addr _ (Right d) v) = TxOut addr v (Just $ datumHash d)

-- | Gets the 'Address' of a chain index transaction output.
getTxOutAddr :: ChainIndexTxOut -> Address
getTxOutAddr PublicKeyChainIndexTxOut { pkOutAddress } = pkOutAddress
getTxOutAddr ScriptChainIndexTxOut { scOutAddress }    = scOutAddress

-- | Gets the 'Value of a chain index transaction output.
getTxOutValue :: ChainIndexTxOut -> Value
getTxOutValue PublicKeyChainIndexTxOut { pkOutValue } = pkOutValue
getTxOutValue ScriptChainIndexTxOut { scOutValue }    = scOutValue

instance Pretty ChainIndexTxOut where
    pretty PublicKeyChainIndexTxOut {pkOutAddress, pkOutValue} =
                hang 2 $ vsep ["-" <+> pretty pkOutValue <+> "addressed to", pretty pkOutAddress]
    pretty ScriptChainIndexTxOut {scOutAddress, scOutValue} =
                hang 2 $ vsep ["-" <+> pretty scOutValue <+> "addressed to", pretty scOutAddress]

instance Pretty Tx where
    pretty t@Tx{txInputs, txCollateral, txOutputs, txMint, txFee, txValidRange, txSignatures, txMintScripts, txData} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList txInputs)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList txCollateral)))
                , hang 2 (vsep ("outputs:" : fmap pretty txOutputs))
                , "mint:" <+> pretty txMint
                , "fee:" <+> pretty txFee
                , hang 2 (vsep ("mps:": fmap pretty (Set.toList txMintScripts)))
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList txSignatures)))
                , "validity range:" <+> viaShow txValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList txData) ))
                ]
            txid = txId t
        in nest 2 $ vsep ["Tx" <+> pretty txid <> colon, braces (vsep lines')]

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = TxId $ toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` spentOutputs tx) `Map.union` unspentOutputsTx tx

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a validator script hash
--   with the given data script attached.
scriptTxOut' :: Value -> Address -> Datum -> TxOut
scriptTxOut' v a ds = TxOut a v (Just (datumHash ds))

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> Validator -> Datum -> TxOut
scriptTxOut v vs = scriptTxOut' v (scriptAddress vs)

-- | Create a transaction output locked by a public key.
pubKeyTxOut :: Value -> PubKey -> TxOut
pubKeyTxOut v pk = TxOut (pubKeyAddress pk) v Nothing

-- | Sign the transaction with a 'PrivateKey' and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Tx -> Tx
addSignature privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (txId tx) privK
    pubK = toPublicKey privK

