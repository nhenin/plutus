{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
-- Code temporarily copied over from cardano-api.
module Plutus.Contract.CardanoAPITemp (makeTransactionBody') where

import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set                    as Set

import           Cardano.Api
import           Cardano.Api.Shelley hiding (toShelleyTxOut)
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.Constraints as Ledger

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra

import qualified Cardano.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

data AnyScriptWitness era where
     AnyScriptWitness :: ScriptWitness witctx era -> AnyScriptWitness era

makeTransactionBody' :: TxBodyContent BuildTx AlonzoEra -> Either (TxBodyError AlonzoEra) (TxBody AlonzoEra)
makeTransactionBody'
    txbodycontent@TxBodyContent {
        txIns,
        txInsCollateral,
        txOuts,
        txFee,
        txValidityRange = (lowerBound, upperBound),
        txExtraKeyWits,
        txWithdrawals,
        txCertificates,
        txMintValue
    } =
    return $
      ShelleyTxBody ShelleyBasedEraAlonzo
        (Alonzo.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (case txInsCollateral of
             TxInsCollateralNone     -> Set.empty
             TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          SNothing -- ignoring txUpdatePropsal in CardanoAPITemp
          (case txExtraKeyWits of
             TxExtraKeyWitnessesNone   -> Set.empty
             TxExtraKeyWitnesses _ khs -> Set.fromList
                                            [ Shelley.coerceKeyRole kh
                                            | PaymentKeyHash kh <- khs ])
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v)
          SNothing -- ignoring txProtocolParams in CardanoAPITemp
          SNothing -- ignoring txMetadata and txAuxScripts in CardanoAPITemp
          SNothing) -- TODO alonzo: support optional network id in TxBodyContent
        scripts
        (TxBodyScriptData ScriptDataInAlonzoEra datums redeemers)
        Nothing -- ignoring txMetadata and txAuxScripts in CardanoAPITemp
        -- TODO alonzo: support the supplementary script data
  where
    witnesses :: [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts =
      [ toShelleyScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: [Alonzo.Data StandardAlonzo]
    datums =
      [ toAlonzoData d
      | (_, AnyScriptWitness
              (PlutusScriptWitness
                 _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
      ]

    redeemers :: Alonzo.Redeemers StandardAlonzo
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (ptr, (toAlonzoData d, toAlonzoExUnits e))
          | (ptr, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> Shelley.Wdrl StandardCrypto
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]

collectTxBodyScriptWitnesses :: TxBodyContent BuildTx AlonzoEra
                             -> [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
collectTxBodyScriptWitnesses TxBodyContent {
                               txIns,
                               txWithdrawals,
                               txCertificates,
                               txMintValue
                             } =
    concat
      [ scriptWitnessesTxIns        txIns
      , scriptWitnessesWithdrawals  txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting      txMintValue
      ]
  where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
      -> [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
    scriptWitnessesTxIns txins =
        [ (Alonzo.RdmrPtr Alonzo.Spend ix, AnyScriptWitness witness)
          -- The tx ins are indexed in the map order by txid
        | (ix, BuildTxWith (ScriptWitness _ witness)) <- zip [0..] (orderTxIns txins)
        ]

    -- This relies on the TxId Ord instance being consistent with the
    -- Shelley.TxId Ord instance via the toShelleyTxId conversion
    -- This is checked by prop_ord_distributive_TxId
    orderTxIns :: Ord k => [(k, v)] -> [v]
    orderTxIns = Map.elems . Map.fromList

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx AlonzoEra
      -> [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
    scriptWitnessesWithdrawals  TxWithdrawalsNone = []
    scriptWitnessesWithdrawals (TxWithdrawals _ withdrawals) =
        [ (Alonzo.RdmrPtr Alonzo.Rewrd ix, AnyScriptWitness witness)
          -- The withdrawals are indexed in the map order by stake credential
        | (ix, BuildTxWith (ScriptWitness _ witness))
             <- zip [0..] (orderStakeAddrs withdrawals)
        ]

    -- This relies on the StakeAddress Ord instance being consistent with the
    -- Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion
    -- This is checked by prop_ord_distributive_StakeAddress
    orderStakeAddrs :: Ord k => [(k, x, v)] -> [v]
    orderStakeAddrs = Map.elems . Map.fromList . map (\(k, _, v) -> (k, v))

    scriptWitnessesCertificates
      :: TxCertificates BuildTx AlonzoEra
      -> [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
    scriptWitnessesCertificates  TxCertificatesNone = []
    scriptWitnessesCertificates (TxCertificates _ certs (BuildTxWith witnesses)) =
        [ (Alonzo.RdmrPtr Alonzo.Cert ix, AnyScriptWitness witness)
          -- The certs are indexed in list order
        | (ix, cert) <- zip [0..] certs
        , ScriptWitness _ witness <- maybeToList $ do
                                       stakecred <- selectStakeCredential cert
                                       Map.lookup stakecred witnesses
        ]

    selectStakeCredential cert =
      case cert of
        StakeAddressDeregistrationCertificate stakecred   -> Just stakecred
        StakeAddressDelegationCertificate     stakecred _ -> Just stakecred
        _                                                 -> Nothing

    scriptWitnessesMinting
      :: TxMintValue BuildTx AlonzoEra
      -> [(Alonzo.RdmrPtr, AnyScriptWitness AlonzoEra)]
    scriptWitnessesMinting  TxMintNone = []
    scriptWitnessesMinting (TxMintValue _ value (BuildTxWith witnesses)) =
        [ (Alonzo.RdmrPtr Alonzo.Mint ix, AnyScriptWitness witness)
          -- The minting policies are indexed in policy id order in the value
        | let ValueNestedRep bundle = valueToNestedRep value
        , (ix, ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        ]

toShelleyTxOut :: forall era ledgerera.
                 (ShelleyLedgerEra era ~ ledgerera,
                  IsShelleyBasedEra era, Ledger.ShelleyBased ledgerera)
               => TxOut era -> Ledger.TxOut ledgerera
toShelleyTxOut (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInMaryEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
                 (toAlonzoTxOutDataHash txoutdata)

toAlonzoTxOutDataHash :: TxOutDatumHash era
                      -> StrictMaybe (Alonzo.DataHash StandardCrypto)
toAlonzoTxOutDataHash TxOutDatumHashNone    = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
