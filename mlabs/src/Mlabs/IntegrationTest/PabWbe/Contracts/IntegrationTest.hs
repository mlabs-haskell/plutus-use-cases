module Mlabs.IntegrationTest.PabWbe.Contracts.IntegrationTest (
  runTests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError), liftEither)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Row (Empty)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)

import Ledger (
  ChainIndexTxOut (..),
  PubKeyHash,
  TxId,
  TxOutRef (TxOutRef),
  inRef,
  pubKeyHashAddress,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints (ScriptLookups, TxConstraints, UnbalancedTx, mkTx)
import Ledger.Constraints qualified as Constraints

import Mlabs.IntegrationTest.Checks
import Mlabs.IntegrationTest.PabWbe.TxInfo
import Mlabs.IntegrationTest.PabWbe.Types
import Mlabs.IntegrationTest.Utils

import Plutus.ChainIndex (ChainIndexTx, citxInputs)
import Plutus.Contract.Wallet (ExportTx (..), export)

import Plutus.Contract (
  Contract,
  balanceTx,
  handleError,
  logError,
  logInfo,
  ownPubKeyHash,
  submitBalancedTx,
  txOutFromRef,
 )

import PlutusTx.Prelude hiding (
  Applicative (..),
  Monoid (..),
  Semigroup (..),
  check,
  mconcat,
  (%),
 )

import Prelude (Applicative (..), Monoid (..), Semigroup (..))

data TestCase = TestCase
  { description :: Text
  , test :: Test
  }

data Test = Test
  { transaction :: ExportTx
  , checks :: [AnyCheck]
  }

runTests :: Contract () Empty Text ()
runTests = do
  pkh <- ownPubKeyHash
  logInfo @Text $ "Running tests contracts with PKH: " <> tshow pkh

  handleError testCaseFailed $
    getTestCases >>= traverse_ runTestCase
  where
    testCaseFailed :: Text -> Contract () Empty Text ()
    testCaseFailed err = logError @Text $ "Test failed with: " <> err

    runTestCase :: TestCase -> Contract () Empty Text ()
    runTestCase TestCase {test = Test {..}, ..} = do
      logInfo @Text $ "Running test: " <> description
      logInfo @Text $ "Check for Tx [id]:" <> exTxId
      for_ checks $ \(AnyCheck check) -> logInfo @Text $ report check
      where
        exTxId :: Text
        exTxId = tshow $ getExportTxId transaction

getTestCases :: Contract () s Text [TestCase]
getTestCases =
  sequence
    [ wallet2Wallet
    , wallet2WalletEnoughInputs
    , wallet2WalletNotEnoughInputs
    ]

wallet2Wallet :: Contract () s Text TestCase
wallet2Wallet = do
  PabResults {..} <- pabResultsFromTx mempty txC
  pure
    . TestCase "Transaction from wallet to wallet w/o inputs"
    $ Test
      exportTx
      [ AnyCheck $ mustBeBalanced balancedInfo
      , AnyCheck $ feeMustBeAdded balancedInfo
      , AnyCheck $ witnessesMustBeAdded signedInfo
      , AnyCheck $ inputsMustBeAdded balancedInfo
      , AnyCheck $ unbalancedInsOutsShouldNotChange balancedInfo
      , AnyCheck $ witnessesMustBeAdded signedInfo
      ]
  where
    txC :: TxConstraints i o
    txC = Constraints.mustPayToPubKey toPkh $ Ada.adaValueOf 5

    toPkh :: PubKeyHash
    toPkh =
      unsafeDecodePkh
        "{\"getPubKeyHash\" : \"d19278d36a31eec98aca5d1cc226fcf5aee6451bb9d0123bb60c1b5b\"}"

wallet2WalletEnoughInputs :: Contract () s Text TestCase
wallet2WalletEnoughInputs = do
  PabResults {..} <- pabResultsFromTx withInputL withInputC
  pure
    . TestCase
      "Transaction from wallet to wallet with input that can cover outputs"
    $ Test
      exportTx
      [ AnyCheck $ mustBeBalanced balancedInfo
      , AnyCheck $ feeMustBeAdded balancedInfo
      , AnyCheck $ witnessesMustBeAdded signedInfo
      , AnyCheck . cNot $ inputsMustBeAdded balancedInfo
      , AnyCheck $ unbalancedInsOutsShouldNotChange balancedInfo
      ]
  where
    withInputC :: TxConstraints i o
    withInputC =
      mconcat
        [ Constraints.mustSpendPubKeyOutput oref
        , Constraints.mustPayToPubKey pkhTo $ Ada.adaValueOf 5
        ]

    withInputL :: ScriptLookups a
    withInputL = Constraints.unspentOutputs utxos

    txOut :: ChainIndexTxOut
    txOut = PublicKeyChainIndexTxOut (pubKeyHashAddress pkh) (Ada.adaValueOf 10)

    utxos :: Map TxOutRef ChainIndexTxOut
    utxos = Map.singleton oref txOut

    oref :: TxOutRef
    oref = TxOutRef refId 2

    pkh :: PubKeyHash
    pkh =
      unsafeDecodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    pkhTo :: PubKeyHash
    pkhTo =
      unsafeDecodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d4\"}"

    refId :: TxId
    refId =
      unsafeDecode
        "TxId"
        "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

wallet2WalletNotEnoughInputs :: Contract () s Text TestCase
wallet2WalletNotEnoughInputs = do
  PabResults {..} <- pabResultsFromTx withInputL withInputC
  pure
    . TestCase
      "Transaction from wallet to wallet with some inputs, \
      \ but not enough to cover outputs"
    $ Test
      exportTx
      [ AnyCheck $ mustBeBalanced balancedInfo
      , AnyCheck $ feeMustBeAdded balancedInfo
      , AnyCheck $ witnessesMustBeAdded signedInfo
      , AnyCheck $ inputsMustBeAdded balancedInfo
      , AnyCheck $ unbalancedInsOutsShouldNotChange balancedInfo
      ]
  where
    withInputC :: TxConstraints i o
    withInputC =
      mconcat
        [ Constraints.mustSpendPubKeyOutput oref
        , Constraints.mustPayToPubKey pkhTo $ Ada.adaValueOf 15
        ]

    withInputL :: ScriptLookups a
    withInputL = Constraints.unspentOutputs utxos

    txOut :: ChainIndexTxOut
    txOut = PublicKeyChainIndexTxOut (pubKeyHashAddress pkh) (Ada.adaValueOf 10)

    utxos :: Map TxOutRef ChainIndexTxOut
    utxos = Map.singleton oref txOut

    oref :: TxOutRef
    oref = TxOutRef refId 2

    pkh :: PubKeyHash
    pkh =
      unsafeDecodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    pkhTo :: PubKeyHash
    pkhTo =
      unsafeDecodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d4\"}"

    refId :: TxId
    refId =
      unsafeDecode
        "TxId"
        "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

pabResultsFromTx ::
  ScriptLookups Void ->
  TxConstraints Void Void ->
  Contract w s Text PabResults
pabResultsFromTx ls cs =
  getPabResults
    =<< mapTShowableErr (liftEither (mkTx @Void ls cs))

getCitxTxOuts :: ChainIndexTx -> Contract w s Text [ChainIndexTxOut]
getCitxTxOuts citx = getTxOuts insAndOuts >>= maybe getTxOutsFailed pure
  where
    getTxOuts :: [TxOutRef] -> Contract w s Text (Maybe [ChainIndexTxOut])
    getTxOuts = fmap sequence . traverse txOutFromRef

    insAndOuts :: [TxOutRef]
    insAndOuts = (^. inRef) <$> Set.toList (citx ^. citxInputs)

    getTxOutsFailed =
      throwError @Text
        "Failed to get 'ChainIndexTxOut's from 'TxOutRef's"

getPabResults :: UnbalancedTx -> Contract w s Text PabResults
getPabResults unbTx = do
  balancedTx <- getSomeCardanoApiTx =<< balanceTx unbTx
  (balancedContent, balancedCitx) <- getTxAndWitnesses balancedTx
  exportTx@(ExportTx apiTx _ _) <-
    mapTShowableErr . liftEither $
      export protoParams netId unbTx
  utxos <- getCitxTxOuts balancedCitx
  balancedInfo <- do
    initial <- mapTShowableErr . liftEither $ toChainIndexTx apiTx
    pure $
      analyzeBalanced
        initial
        balancedCitx
        utxos
        balancedContent

  signedTx <- getSomeCardanoApiTx =<< submitBalancedTx (Left balancedTx)
  (_, signedCitx) <- getTxAndWitnesses signedTx
  signedInfo <-
    mapTShowableErr . liftEither $
      analyzeSigned balancedCitx signedCitx

  pure PabResults {..}
  where
    netId :: C.NetworkId
    netId = C.Testnet $ C.NetworkMagic 8

    protoParams :: C.ProtocolParameters
    protoParams =
      C.ProtocolParameters
        { protocolParamProtocolVersion = (2, 0)
        , protocolParamDecentralization = 1 % 4
        , protocolParamExtraPraosEntropy = Nothing
        , protocolParamMaxBlockHeaderSize = 1100
        , protocolParamMaxBlockBodySize = 65536
        , protocolParamMaxTxSize = 16384
        , protocolParamTxFeeFixed = 155381
        , protocolParamTxFeePerByte = 44
        , protocolParamMinUTxOValue = Nothing
        , protocolParamStakeAddressDeposit = C.Lovelace 2000000
        , protocolParamStakePoolDeposit = C.Lovelace 500000000
        , protocolParamMinPoolCost = C.Lovelace 340000000
        , protocolParamPoolRetireMaxEpoch = C.EpochNo 18
        , protocolParamStakePoolTargetNum = 150
        , protocolParamPoolPledgeInfluence = 3 % 10
        , protocolParamMonetaryExpansion = 3 % 1000
        , protocolParamTreasuryCut = 1 % 5
        , protocolParamUTxOCostPerWord = Just $ C.Lovelace 34482
        , protocolParamPrices =
            Just
              ( C.ExecutionUnitPrices
                  { priceExecutionSteps = 721 % 10000000
                  , priceExecutionMemory = 577 % 10000
                  }
              )
        , protocolParamMaxTxExUnits =
            Just
              ( C.ExecutionUnits
                  { executionSteps = 10000000000
                  , executionMemory = 10000000
                  }
              )
        , protocolParamMaxBlockExUnits =
            Just
              ( C.ExecutionUnits
                  { executionSteps = 40000000000
                  , executionMemory = 50000000
                  }
              )
        , protocolParamMaxValueSize = Just 5000
        , protocolParamCollateralPercent = Just 150
        , protocolParamMaxCollateralInputs = Just 3
        , protocolParamCostModels =
            Map.fromList
              [
                ( C.AnyPlutusScriptVersion C.PlutusScriptV1
                , C.CostModel
                    ( Map.fromList
                        [ ("addInteger-cpu-arguments-intercept", 197209)
                        , ("addInteger-cpu-arguments-slope", 0)
                        , ("addInteger-memory-arguments-intercept", 1)
                        , ("addInteger-memory-arguments-slope", 1)
                        , ("appendByteString-cpu-arguments-intercept", 396231)
                        , ("appendByteString-cpu-arguments-slope", 621)
                        , ("appendByteString-memory-arguments-intercept", 0)
                        , ("appendByteString-memory-arguments-slope", 1)
                        , ("appendString-cpu-arguments-intercept", 150000)
                        , ("appendString-cpu-arguments-slope", 1000)
                        , ("appendString-memory-arguments-intercept", 0)
                        , ("appendString-memory-arguments-slope", 1)
                        , ("bData-cpu-arguments", 150000)
                        , ("bData-memory-arguments", 32)
                        , ("blake2b-cpu-arguments-intercept", 2477736)
                        , ("blake2b-cpu-arguments-slope", 29175)
                        , ("blake2b-memory-arguments", 4)
                        , ("cekApplyCost-exBudgetCPU", 29773)
                        , ("cekApplyCost-exBudgetMemory", 100)
                        , ("cekBuiltinCost-exBudgetCPU", 29773)
                        , ("cekBuiltinCost-exBudgetMemory", 100)
                        , ("cekConstCost-exBudgetCPU", 29773)
                        , ("cekConstCost-exBudgetMemory", 100)
                        , ("cekDelayCost-exBudgetCPU", 29773)
                        , ("cekDelayCost-exBudgetMemory", 100)
                        , ("cekForceCost-exBudgetCPU", 29773)
                        , ("cekForceCost-exBudgetMemory", 100)
                        , ("cekLamCost-exBudgetCPU", 29773)
                        , ("cekLamCost-exBudgetMemory", 100)
                        , ("cekStartupCost-exBudgetCPU", 100)
                        , ("cekStartupCost-exBudgetMemory", 100)
                        , ("cekVarCost-exBudgetCPU", 29773)
                        , ("cekVarCost-exBudgetMemory", 100)
                        , ("chooseData-cpu-arguments", 150000)
                        , ("chooseData-memory-arguments", 32)
                        , ("chooseList-cpu-arguments", 150000)
                        , ("chooseList-memory-arguments", 32)
                        , ("chooseUnit-cpu-arguments", 150000)
                        , ("chooseUnit-memory-arguments", 32)
                        , ("consByteString-cpu-arguments-intercept", 150000)
                        , ("consByteString-cpu-arguments-slope", 1000)
                        , ("consByteString-memory-arguments-intercept", 0)
                        , ("consByteString-memory-arguments-slope", 1)
                        , ("constrData-cpu-arguments", 150000)
                        , ("constrData-memory-arguments", 32)
                        , ("decodeUtf8-cpu-arguments-intercept", 150000)
                        , ("decodeUtf8-cpu-arguments-slope", 1000)
                        , ("decodeUtf8-memory-arguments-intercept", 0)
                        , ("decodeUtf8-memory-arguments-slope", 8)
                        , ("divideInteger-cpu-arguments-constant", 148000)
                        , ("divideInteger-cpu-arguments-model-arguments-intercept", 425507)
                        , ("divideInteger-cpu-arguments-model-arguments-slope", 118)
                        , ("divideInteger-memory-arguments-intercept", 0)
                        , ("divideInteger-memory-arguments-minimum", 1)
                        , ("divideInteger-memory-arguments-slope", 1)
                        , ("encodeUtf8-cpu-arguments-intercept", 150000)
                        , ("encodeUtf8-cpu-arguments-slope", 1000)
                        , ("encodeUtf8-memory-arguments-intercept", 0)
                        , ("encodeUtf8-memory-arguments-slope", 8)
                        , ("equalsByteString-cpu-arguments-constant", 150000)
                        , ("equalsByteString-cpu-arguments-intercept", 112536)
                        , ("equalsByteString-cpu-arguments-slope", 247)
                        , ("equalsByteString-memory-arguments", 1)
                        , ("equalsData-cpu-arguments-intercept", 150000)
                        , ("equalsData-cpu-arguments-slope", 10000)
                        , ("equalsData-memory-arguments", 1)
                        , ("equalsInteger-cpu-arguments-intercept", 136542)
                        , ("equalsInteger-cpu-arguments-slope", 1326)
                        , ("equalsInteger-memory-arguments", 1)
                        , ("equalsString-cpu-arguments-constant", 1000)
                        , ("equalsString-cpu-arguments-intercept", 150000)
                        , ("equalsString-cpu-arguments-slope", 1000)
                        , ("equalsString-memory-arguments", 1)
                        , ("fstPair-cpu-arguments", 150000)
                        , ("fstPair-memory-arguments", 32)
                        , ("headList-cpu-arguments", 150000)
                        , ("headList-memory-arguments", 32)
                        , ("iData-cpu-arguments", 150000)
                        , ("iData-memory-arguments", 32)
                        , ("ifThenElse-cpu-arguments", 1)
                        , ("ifThenElse-memory-arguments", 1)
                        , ("indexByteString-cpu-arguments", 150000)
                        , ("indexByteString-memory-arguments", 1)
                        , ("lengthOfByteString-cpu-arguments", 150000)
                        , ("lengthOfByteString-memory-arguments", 4)
                        , ("lessThanByteString-cpu-arguments-intercept", 103599)
                        , ("lessThanByteString-cpu-arguments-slope", 248)
                        , ("lessThanByteString-memory-arguments", 1)
                        , ("lessThanEqualsByteString-cpu-arguments-intercept", 103599)
                        , ("lessThanEqualsByteString-cpu-arguments-slope", 248)
                        , ("lessThanEqualsByteString-memory-arguments", 1)
                        , ("lessThanEqualsInteger-cpu-arguments-intercept", 145276)
                        , ("lessThanEqualsInteger-cpu-arguments-slope", 1366)
                        , ("lessThanEqualsInteger-memory-arguments", 1)
                        , ("lessThanInteger-cpu-arguments-intercept", 179690)
                        , ("lessThanInteger-cpu-arguments-slope", 497)
                        , ("lessThanInteger-memory-arguments", 1)
                        , ("listData-cpu-arguments", 150000)
                        , ("listData-memory-arguments", 32)
                        , ("mapData-cpu-arguments", 150000)
                        , ("mapData-memory-arguments", 32)
                        , ("mkCons-cpu-arguments", 150000)
                        , ("mkCons-memory-arguments", 32)
                        , ("mkNilData-cpu-arguments", 150000)
                        , ("mkNilData-memory-arguments", 32)
                        , ("mkNilPairData-cpu-arguments", 150000)
                        , ("mkNilPairData-memory-arguments", 32)
                        , ("mkPairData-cpu-arguments", 150000)
                        , ("mkPairData-memory-arguments", 32)
                        , ("modInteger-cpu-arguments-constant", 148000)
                        , ("modInteger-cpu-arguments-model-arguments-intercept", 425507)
                        , ("modInteger-cpu-arguments-model-arguments-slope", 118)
                        , ("modInteger-memory-arguments-intercept", 0)
                        , ("modInteger-memory-arguments-minimum", 1)
                        , ("modInteger-memory-arguments-slope", 1)
                        , ("multiplyInteger-cpu-arguments-intercept", 61516)
                        , ("multiplyInteger-cpu-arguments-slope", 11218)
                        , ("multiplyInteger-memory-arguments-intercept", 0)
                        , ("multiplyInteger-memory-arguments-slope", 1)
                        , ("nullList-cpu-arguments", 150000)
                        , ("nullList-memory-arguments", 32)
                        , ("quotientInteger-cpu-arguments-constant", 148000)
                        , ("quotientInteger-cpu-arguments-model-arguments-intercept", 425507)
                        , ("quotientInteger-cpu-arguments-model-arguments-slope", 118)
                        , ("quotientInteger-memory-arguments-intercept", 0)
                        , ("quotientInteger-memory-arguments-minimum", 1)
                        , ("quotientInteger-memory-arguments-slope", 1)
                        , ("remainderInteger-cpu-arguments-constant", 148000)
                        , ("remainderInteger-cpu-arguments-model-arguments-intercept", 425507)
                        , ("remainderInteger-cpu-arguments-model-arguments-slope", 118)
                        , ("remainderInteger-memory-arguments-intercept", 0)
                        , ("remainderInteger-memory-arguments-minimum", 1)
                        , ("remainderInteger-memory-arguments-slope", 1)
                        , ("sha2_256-cpu-arguments-intercept", 2477736)
                        , ("sha2_256-cpu-arguments-slope", 29175)
                        , ("sha2_256-memory-arguments", 4)
                        , ("sha3_256-cpu-arguments-intercept", 0)
                        , ("sha3_256-cpu-arguments-slope", 82363)
                        , ("sha3_256-memory-arguments", 4)
                        , ("sliceByteString-cpu-arguments-intercept", 150000)
                        , ("sliceByteString-cpu-arguments-slope", 5000)
                        , ("sliceByteString-memory-arguments-intercept", 0)
                        , ("sliceByteString-memory-arguments-slope", 1)
                        , ("sndPair-cpu-arguments", 150000)
                        , ("sndPair-memory-arguments", 32)
                        , ("subtractInteger-cpu-arguments-intercept", 197209)
                        , ("subtractInteger-cpu-arguments-slope", 0)
                        , ("subtractInteger-memory-arguments-intercept", 1)
                        , ("subtractInteger-memory-arguments-slope", 1)
                        , ("tailList-cpu-arguments", 150000)
                        , ("tailList-memory-arguments", 32)
                        , ("trace-cpu-arguments", 150000)
                        , ("trace-memory-arguments", 32)
                        , ("unBData-cpu-arguments", 150000)
                        , ("unBData-memory-arguments", 32)
                        , ("unConstrData-cpu-arguments", 150000)
                        , ("unConstrData-memory-arguments", 32)
                        , ("unIData-cpu-arguments", 150000)
                        , ("unIData-memory-arguments", 32)
                        , ("unListData-cpu-arguments", 150000)
                        , ("unListData-memory-arguments", 32)
                        , ("unMapData-cpu-arguments", 150000)
                        , ("unMapData-memory-arguments", 32)
                        , ("verifySignature-cpu-arguments-intercept", 3345831)
                        , ("verifySignature-cpu-arguments-slope", 1)
                        , ("verifySignature-memory-arguments", 1)
                        ]
                    )
                )
              ]
        }
