{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Spec.Emulator(tests) where


import           Control.Lens
import           Control.Monad                  (void)
import qualified Control.Monad.Freer            as Eff
import qualified Control.Monad.Freer.Error      as E
import           Control.Monad.Freer.Extras
import           Control.Monad.Freer.Extras.Log (logMessageContent)
import           Control.Monad.Freer.Writer     (Writer, runWriter, tell)
import           Control.Monad.Trans.Except     (runExcept)
import qualified Data.Aeson                     as JSON
import qualified Data.Aeson.Extras              as JSON
import qualified Data.Aeson.Internal            as Aeson
import qualified Data.ByteString                as BSS
import qualified Data.ByteString.Lazy           as BSL
import           Data.ByteString.Lazy.Char8     (pack)
import           Data.Default                   (Default (..))
import           Data.Either                    (isLeft, isRight)
import           Data.Foldable                  (fold, foldl', traverse_)
import           Data.List                      (sort)
import qualified Data.Map                       as Map
import           Data.Monoid                    (Sum (..))
import qualified Data.Set                       as Set
import           Data.String                    (IsString (fromString))
import           Hedgehog                       (Property, forAll, property)
import qualified Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Range                 as Range
import           Ledger
import qualified Ledger.Ada                     as Ada
import           Ledger.Bytes                   as LedgerBytes
import           Ledger.Generators              (Mockchain (Mockchain))
import qualified Ledger.Generators              as Gen
import qualified Ledger.Index                   as Index
import           Ledger.Typed.Scripts           (wrapValidator)
import           Ledger.Value                   (CurrencySymbol, Value (Value))
import qualified Ledger.Value                   as Value
import           Plutus.Contract.Test           hiding (not)
import           Plutus.Trace                   (EmulatorTrace, PrintEffect (..))
import qualified Plutus.Trace                   as Trace
import qualified PlutusTx
import           PlutusTx.AssocMap              as AssocMap
import qualified PlutusTx.Builtins              as Builtins
import qualified PlutusTx.Numeric               as P
import qualified PlutusTx.Prelude               as PlutusTx
import           Test.Tasty
import           Test.Tasty.Golden              (goldenVsString)
import           Test.Tasty.HUnit               (testCase)
import qualified Test.Tasty.HUnit               as HUnit
import           Test.Tasty.Hedgehog            (testProperty)
import           Wallet
import qualified Wallet.API                     as W
import qualified Wallet.Emulator.Chain          as Chain
import           Wallet.Emulator.MultiAgent     (EmulatorEvent' (..), eteEvent)
import qualified Wallet.Emulator.NodeClient     as NC
import           Wallet.Emulator.Types
import qualified Wallet.Emulator.Wallet         as Wallet
import qualified Wallet.Graph


tests :: TestTree
tests = testGroup "all tests" [
    testGroup "UTXO model" [
        testProperty "compute UTxO of trivial blockchain" utxo,
        testProperty "validate transaction" txnValid,
        testProperty "validate transaction when it can be validated" txnValidFromOld,
        testProperty "validate transaction when it can be validated" txnValidFromV2,
        testProperty "update UTXO set after each transaction" txnUpdateUtxoOld,
        testProperty "update UTXO set after each transaction" txnUpdateUtxoV2
        ],
    testGroup "traces" [
        testProperty "accept valid txn" validTraceOld,
        testProperty "accept valid txn" validTraceV2,
        testProperty "accept valid txn 2" validTrace2Old,
        testProperty "accept valid txn 2" validTrace2V2,
        testProperty "reject invalid txn" invalidTraceOld,
        testProperty "reject invalid txn" invalidTraceV2,
        testProperty "notify wallet" notifyWalletOld,
        testProperty "notify wallet" notifyWalletV2,
        testProperty "watch funds at an address" walletWatchinOwnAddressOld,
        testProperty "log script validation failures" invalidScriptOld,
        testProperty "log script validation failures" invalidScriptV2,
        testProperty "payToPubkey" payToPubKeyScriptOld,
        testProperty "payToPubkey" payToPubKeyScriptV2,
        testProperty "payToPubkey-2" payToPubKeyScript2Old,
        testProperty "payToPubkey-2" payToPubKeyScript2V2
        ],
    testGroup "trace output" [
        goldenVsString
          "captures a trace of a wait"
          "test/Spec/golden/traceOutput - wait1.txt"
          (pure $ captureTraceOld (void $ Trace.waitNSlots 1)),
        goldenVsString
          "captures a trace of pubKeytransactions"
          "test/Spec/golden/traceOutput - pubKeyTransactions.txt"
          (pure $ captureTraceOld pubKeyTransactions),
        goldenVsString
          "captures a trace of pubKeytransactions2"
          "test/Spec/golden/traceOutput - pubKeyTransactions2.txt"
          (pure $ captureTraceOld pubKeyTransactions2),
        goldenVsString
          "captures a trace of a wait"
          "test/Spec/golden/traceOutputV2 - wait1V2.txt"
          (pure $ captureTraceV2 (void $ Trace.waitNSlots 1)),
        goldenVsString
          "captures a trace of pubKeytransactions"
          "test/Spec/golden/traceOutputV2 - pubKeyTransactionsV2.txt"
          (pure $ captureTraceV2 pubKeyTransactions),
        goldenVsString
          "captures a trace of pubKeytransactions2"
          "test/Spec/golden/traceOutputV2 - pubKeyTransactions2V2.txt"
          (pure $ captureTraceV2 pubKeyTransactions2)
    ],
    testGroup "Etc." [
        testProperty "selectCoin" selectCoinProp,
        testProperty "txnFlows" txnFlowsTestOld,
        testProperty "txnFlows" txnFlowsTestV2
        ]
    ]


-- TODO: To delete. Uses the old chain index.
captureTraceOld
    :: EmulatorTrace ()
    -> BSL.ByteString
captureTraceOld trace
  = pack $ unlines output
  where
    output = capturePrintEffect $ Trace.runEmulatorTraceEffOld def def trace

captureTraceV2
    :: EmulatorTrace ()
    -> BSL.ByteString
captureTraceV2 trace
  = pack $ unlines output
  where
    output = capturePrintEffect $ Trace.runEmulatorTraceEffV2 def def trace

capturePrintEffect
         :: Eff.Eff '[PrintEffect] r
         -> [String]
capturePrintEffect effs = snd $ Eff.run (runWriter (Eff.reinterpret f effs))
  where
    f :: PrintEffect r -> Eff.Eff '[Writer [String]] r
    f = \case
      PrintLn s -> tell [s]


wallet1, wallet2, wallet3 :: Wallet
wallet1 = Wallet 1
wallet2 = Wallet 2
wallet3 = Wallet 3

pubKey1, pubKey2, pubKey3 :: PubKey
pubKey1 = walletPubKey wallet1
pubKey2 = walletPubKey wallet2
pubKey3 = walletPubKey wallet3

utxo :: Property
utxo = property $ do
    Mockchain txPool o <- forAll Gen.genMockchain
    Hedgehog.assert (unspentOutputs [map Valid txPool] == o)

txnValid :: Property
txnValid = property $ do
    (m, txn) <- forAll genChainTxn
    Gen.assertValid txn m

-- TODO: To delete. Uses the old chain index.
txnValidFromOld :: Property
txnValidFromOld =
    let five = Ada.lovelaceValueOf 5
        -- Set the validation interval to (5, 5] for the
        -- transaction generated by payToPublicKey_
        -- so that the transaction can be validated only during slot 5
        range = W.singleton 5

    in checkPredicateGenOld Gen.generatorModel
        (walletFundsChange wallet1 (P.negate five)
        .&&. walletFundsChange wallet2 five
        )
        $ do
            Trace.liftWallet wallet1 $ payToPublicKey_ range five pubKey2
            void $ Trace.waitUntilSlot 6

txnValidFromV2 :: Property
txnValidFromV2 =
    let five = Ada.lovelaceValueOf 5
        -- Set the validation interval to (5, 5] for the
        -- transaction generated by payToPublicKey_
        -- so that the transaction can be validated only during slot 5
        range = W.singleton 5

    in checkPredicateGenV2 Gen.generatorModel
        (walletFundsChange wallet1 (P.negate five)
        .&&. walletFundsChange wallet2 five
        )
        $ do
            Trace.liftWallet wallet1 $ payToPublicKey_ range five pubKey2
            void $ Trace.waitUntilSlot 6

selectCoinProp :: Property
selectCoinProp = property $ do
    inputs <- forAll $ zip [1..] <$> Gen.list (Range.linear 1 100) Gen.genValueNonNegative
    target <- forAll Gen.genValueNonNegative
    let result = Eff.run $ E.runError @WalletAPIError (selectCoin inputs target)
    case result of
        Left _ ->
            Hedgehog.assert $ not $ foldMap snd inputs `Value.geq` target
        Right (ins, change) ->
            Hedgehog.assert $ foldMap snd ins == (target P.+ change)

-- TODO: To delete. Uses the old chain index.
txnUpdateUtxoOld :: Property
txnUpdateUtxoOld = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m

        -- submit the same txn twice, so it should be accepted the first time
        -- and rejected the second time.
        trace = do
            Trace.liftWallet wallet1 (submitTxn txn)
            Trace.liftWallet wallet1 (submitTxn txn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidate _ i1 _
                , Chain.TxnValidationFail _ _ txi (Index.TxOutRefNotFound _) _
                , Chain.SlotAdd _
                ] -> i1 == txn && txi == txn
            _ -> False
    checkPredicateInnerOld options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert
txnUpdateUtxoV2 :: Property
txnUpdateUtxoV2 = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m

        -- submit the same txn twice, so it should be accepted the first time
        -- and rejected the second time.
        trace = do
            Trace.liftWallet wallet1 (submitTxn txn)
            Trace.liftWallet wallet1 (submitTxn txn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidate _ i1 _
                , Chain.TxnValidationFail _ _ txi (Index.TxOutRefNotFound _) _
                , Chain.SlotAdd _
                ] -> i1 == txn && txi == txn
            _ -> False
    checkPredicateInnerV2 options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert

-- TODO: To delete. Uses the old chain index.
validTraceOld :: Property
validTraceOld = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = Trace.liftWallet wallet1 (submitTxn txn)
    checkPredicateInnerOld options assertNoFailedTransactions trace Hedgehog.annotate Hedgehog.assert

validTraceV2 :: Property
validTraceV2 = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = Trace.liftWallet wallet1 (submitTxn txn)
    checkPredicateInnerV2 options assertNoFailedTransactions trace Hedgehog.annotate Hedgehog.assert

-- TODO: To delete. Uses the old chain index.
validTrace2Old :: Property
validTrace2Old = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = do
            Trace.liftWallet wallet1 (submitTxn txn)
            Trace.liftWallet wallet1 (submitTxn txn)
        predicate = assertFailedTransaction (\_ _ _ -> True)
    checkPredicateInnerOld options predicate trace Hedgehog.annotate Hedgehog.assert

validTrace2V2 :: Property
validTrace2V2 = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = do
            Trace.liftWallet wallet1 (submitTxn txn)
            Trace.liftWallet wallet1 (submitTxn txn)
        predicate = assertFailedTransaction (\_ _ _ -> True)
    checkPredicateInnerV2 options predicate trace Hedgehog.annotate Hedgehog.assert

-- TODO: To delete. Uses the old chain index.
invalidTraceOld :: Property
invalidTraceOld = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let invalidTxn = txn { txMint = Ada.adaValueOf 1 }
        options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = Trace.liftWallet wallet1 (submitTxn invalidTxn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidationFail _ _ txn (Index.ValueNotPreserved _ _) _
                , Chain.SlotAdd _
                ] -> txn == invalidTxn
            _ -> False
    checkPredicateInnerOld options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert

invalidTraceV2 :: Property
invalidTraceV2 = property $ do
    (Mockchain m _, txn) <- forAll genChainTxn
    let invalidTxn = txn { txMint = Ada.adaValueOf 1 }
        options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = Trace.liftWallet wallet1 (submitTxn invalidTxn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidationFail _ _ txn (Index.ValueNotPreserved _ _) _
                , Chain.SlotAdd _
                ] -> txn == invalidTxn
            _ -> False
    checkPredicateInnerV2 options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert

-- TODO: To delete. Uses the old chain index.
invalidScriptOld :: Property
invalidScriptOld = property $ do
    (Mockchain m _, txn1) <- forAll genChainTxn

    -- modify one of the outputs to be a script output
    index <- forAll $ Gen.int (Range.linear 0 ((length $ txOutputs txn1) -1))
    let scriptTxn = txn1 & outputs . element index %~ \o -> scriptTxOut (txOutValue o) failValidator unitDatum
    Hedgehog.annotateShow scriptTxn
    let outToSpend = txOutRefs scriptTxn !! index
    let totalVal = Ada.fromValue $ txOutValue (fst outToSpend)

    -- try and spend the script output
    invalidTxn <- forAll $ Gen.genValidTransactionSpending (Set.fromList [scriptTxIn (snd outToSpend) failValidator unitRedeemer unitDatum]) totalVal
    Hedgehog.annotateShow invalidTxn

    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m


        -- we need to sign scriptTxn again because it has been modified
        -- note that although 'scriptTxn' is submitted by wallet 1, it
        -- may spend outputs belonging to one of the other two wallets.
        -- So we add all the wallets' signatures with 'signAll'.
        trace = do
            Trace.liftWallet wallet1 (submitTxn (Gen.signAll scriptTxn))
            _ <- Trace.nextSlot
            Trace.liftWallet wallet1 (submitTxn invalidTxn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidationFail _ _ txn (ScriptFailure (EvaluationError ["I always fail everything"] "CekEvaluationFailure")) _
                , Chain.SlotAdd _
                ] -> txn == invalidTxn
            _ -> False

    checkPredicateInnerOld options (assertChainEvents pred .&&. walletPaidFees wallet1 (txFee scriptTxn <> txFee invalidTxn)) trace Hedgehog.annotate Hedgehog.assert
    where
        failValidator :: Validator
        failValidator = mkValidatorScript $$(PlutusTx.compile [|| wrapValidator validator ||])
        validator :: () -> () -> ScriptContext -> Bool
        validator _ _ _ = PlutusTx.traceError "I always fail everything"
invalidScriptV2 :: Property
invalidScriptV2 = property $ do
    (Mockchain m _, txn1) <- forAll genChainTxn

    -- modify one of the outputs to be a script output
    index <- forAll $ Gen.int (Range.linear 0 ((length $ txOutputs txn1) -1))
    let scriptTxn = txn1 & outputs . element index %~ \o -> scriptTxOut (txOutValue o) failValidator unitDatum
    Hedgehog.annotateShow scriptTxn
    let outToSpend = txOutRefs scriptTxn !! index
    let totalVal = Ada.fromValue $ txOutValue (fst outToSpend)

    -- try and spend the script output
    invalidTxn <- forAll $ Gen.genValidTransactionSpending (Set.fromList [scriptTxIn (snd outToSpend) failValidator unitRedeemer unitDatum]) totalVal
    Hedgehog.annotateShow invalidTxn

    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m


        -- we need to sign scriptTxn again because it has been modified
        -- note that although 'scriptTxn' is submitted by wallet 1, it
        -- may spend outputs belonging to one of the other two wallets.
        -- So we add all the wallets' signatures with 'signAll'.
        trace = do
            Trace.liftWallet wallet1 (submitTxn (Gen.signAll scriptTxn))
            _ <- Trace.nextSlot
            Trace.liftWallet wallet1 (submitTxn invalidTxn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidationFail _ _ txn (ScriptFailure (EvaluationError ["I always fail everything"] "CekEvaluationFailure")) _
                , Chain.SlotAdd _
                ] -> txn == invalidTxn
            _ -> False

    checkPredicateInnerV2 options (assertChainEvents pred .&&. walletPaidFees wallet1 (txFee scriptTxn <> txFee invalidTxn)) trace Hedgehog.annotate Hedgehog.assert
    where
        failValidator :: Validator
        failValidator = mkValidatorScript $$(PlutusTx.compile [|| wrapValidator validator ||])
        validator :: () -> () -> ScriptContext -> Bool
        validator _ _ _ = PlutusTx.traceError "I always fail everything"


-- TODO: To delete. Uses the old chain index.
txnFlowsTestOld :: Property
txnFlowsTestOld =
    checkPredicateGenOld Gen.generatorModel
        (assertBlockchain $ \chain ->
           let numTx = length $ fold chain
               flows = Wallet.Graph.txnFlows [] chain
            in
                length flows > numTx)
        pubKeyTransactions

txnFlowsTestV2 :: Property
txnFlowsTestV2 =
    checkPredicateGenV2 Gen.generatorModel
        (assertBlockchain $ \chain ->
           let numTx = length $ fold chain
               flows = Wallet.Graph.txnFlows [] chain
            in
                length flows > numTx)
        pubKeyTransactions

-- TODO: To delete. Uses the old chain index.
notifyWalletOld :: Property
notifyWalletOld =
    checkPredicateGenOld Gen.generatorModel
    (walletFundsChange wallet1 mempty)
    (pure ())

notifyWalletV2 :: Property
notifyWalletV2 =
    checkPredicateGenV2 Gen.generatorModel
    (walletFundsChange wallet1 mempty)
    (pure ())

-- TODO: To delete. Uses the old chain index.
walletWatchinOwnAddressOld :: Property
walletWatchinOwnAddressOld =
    checkPredicateGenOld Gen.generatorModel
    (walletWatchingAddress wallet1 (Wallet.walletAddress wallet1))
    (pure ())

-- TODO: To delete. Uses the old chain index.
payToPubKeyScriptOld :: Property
payToPubKeyScriptOld =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGenOld Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions

payToPubKeyScriptV2 :: Property
payToPubKeyScriptV2 =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGenV2 Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions

-- TODO: To delete. Uses the old chain index.
payToPubKeyScript2Old :: Property
payToPubKeyScript2Old =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGenOld Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions2

payToPubKeyScript2V2 :: Property
payToPubKeyScript2V2 =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGenV2 Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions2

pubKeyTransactions :: EmulatorTrace ()
pubKeyTransactions = do
    let [w1, w2, w3] = [wallet1, wallet2, wallet3]
        five = Ada.lovelaceValueOf 5
    Trace.liftWallet wallet1 $ payToPublicKey_ W.always five pubKey2
    _ <- Trace.nextSlot
    Trace.liftWallet wallet2 $ payToPublicKey_ W.always five pubKey3
    _ <- Trace.nextSlot
    Trace.liftWallet wallet3 $ payToPublicKey_ W.always five pubKey1
    void Trace.nextSlot

pubKeyTransactions2 :: EmulatorTrace ()
pubKeyTransactions2 = do
    let [w1, w2, w3] = [wallet1, wallet2, wallet3]
        payment1 = initialBalance P.- Ada.lovelaceValueOf 100
        payment2 = initialBalance P.+ Ada.lovelaceValueOf 100
    Trace.liftWallet wallet1 $ payToPublicKey_ W.always payment1 pubKey2
    _ <- Trace.nextSlot
    Trace.liftWallet wallet2 $ payToPublicKey_ W.always payment2 pubKey3
    _ <- Trace.nextSlot
    Trace.liftWallet wallet3 $ payToPublicKey_ W.always payment2 pubKey1
    _ <- Trace.nextSlot
    Trace.liftWallet wallet1 $ payToPublicKey_ W.always (Ada.lovelaceValueOf 200) pubKey2
    void Trace.nextSlot

genChainTxn :: Hedgehog.MonadGen m => m (Mockchain, Tx)
genChainTxn = do
    m <- Gen.genMockchain
    txn <- Gen.genValidTransaction m
    pure (m, txn)

initialBalance :: Value
initialBalance = Ada.lovelaceValueOf 100000
