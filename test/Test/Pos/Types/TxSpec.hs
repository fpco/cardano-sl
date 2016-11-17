{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Specification of Pos.Types.Tx.

module Test.Pos.Types.TxSpec
       ( spec
       ) where

import           Control.Lens          (view, _2, _3)
import           Control.Monad         (join)
import           Data.List             (lookup)
import           Pos.Crypto            (verify)
import           Pos.Types             (Address (..), BadSigsTx (..), GoodTx (..),
                                        OverflowTx (..), Tx (..), TxIn (..), TxOut (..),
                                        verifyTx, verifyTxAlone)
import           Serokell.Util.Verify  (isVerFailure, isVerSuccess)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Types.Tx" $ do
    describe "verifyTxAlone" $ do
        prop description_validateGoodTxAlone validateGoodTxAlone
        prop description_invalidateBadTxAlone invalidateBadTxAlone
    describe "verifyTx" $ do
        prop description_validateGoodTx validateGoodTx
        prop description_overflowTx overflowTx
        prop description_badSigsTx badSigsTx
  where
    description_validateGoodTxAlone =
        "validates Txs with positive coins and non-empty inputs and outputs"
    description_invalidateBadTxAlone =
        "invalidates Txs with non-positive coins or empty inputs/outputs"
    description_validateGoodTx =
        "validates a transaction whose inputs and well-formed transaction outputs"
    description_overflowTx =
        "a well-formed transaction with input and output sums above maxBound :: Coin \
        \ is validated successfully"
    description_badSigsTx =
        "a transaction with inputs improperly signed is never validated"

validateGoodTxAlone :: Tx -> Bool
validateGoodTxAlone tx = isVerSuccess $ verifyTxAlone tx

invalidateBadTxAlone :: Tx -> Bool
invalidateBadTxAlone Tx {..} = all (isVerFailure . verifyTxAlone) badTxs
  where
    zeroOutputs = fmap (\(TxOut a _) -> TxOut a (negate 0)) txOutputs
    badTxs =
        map (uncurry Tx) $
        [([], txOutputs), (txInputs, []), (txInputs, zeroOutputs)]

type TxVerifyingTools = (Tx, TxIn -> Maybe TxOut, [Maybe (TxIn, TxOut)])

-- | This function takes the list inside a 'GoodTx' and related types, and
-- turns it into something 'verifyTx' can use:
--
-- ★ the transaction that the list holds
-- ★ the input resolver associated with that transaction
-- ★ the list of resolved inputs with all inputs in the transaction
getTxFromGoodTx
    :: [(Tx, TxIn, TxOut)]
    -> TxVerifyingTools
getTxFromGoodTx ls =
    let txOutputs = fmap (view _3) ls
        txInputs = fmap (view _2) ls
        inpResolver :: TxIn -> Maybe TxOut
        inpResolver = join . flip lookup (fmap (\(Tx _ o, ti, _) -> (ti, head o)) ls)
        extendInput txIn = (txIn,) <$> inpResolver txIn
        extendedInputs :: [Maybe (TxIn, TxOut)]
        extendedInputs = fmap extendInput txInputs
    in (Tx {..}, inpResolver, extendedInputs)

-- | This function takes a list of resolved inputs from a transaction, that
-- same transaction's outputs, and verifies that the input sum is greater than
-- the output sum.
txChecksum :: [Maybe (TxIn, TxOut)] -> [TxOut] -> Bool
txChecksum extendedInputs txOuts =
    let inpSum = sum $ fmap (toInteger . txOutValue . snd) $ catMaybes extendedInputs
        outSum = sum $ fmap (toInteger . txOutValue) txOuts
    in inpSum >= outSum

-- | This function, used in 'verifyGoodTx', takes a 'GoodTx' and checks that
-- each property verified by 'verifyTx' holds, meaning:
--
-- ★ sum of inputs ≥ sum of outputs;
-- ★ every input is signed properly;
-- ★ every input is a known unspent output.
-- It also checks that it has good structure w.r.t. 'verifyTxAlone'.
individualTxPropertyVerifier :: TxVerifyingTools -> Bool
individualTxPropertyVerifier (tx@Tx{..}, _, extendedInputs) =
    let hasGoodSum = txChecksum extendedInputs txOutputs
        hasGoodStructure = isVerSuccess $ verifyTxAlone tx
        mapFun =
            \maybeTxPair ->
                case maybeTxPair of
                    Nothing -> False
                    Just (TxIn{..}, TxOut{..}) ->
                        verify (getAddress txOutAddress)
                               (txInHash, txInIndex, txOutputs)
                               txInSig
        hasGoodInputs = and $ map mapFun extendedInputs
    in hasGoodSum && hasGoodStructure && hasGoodInputs

validateGoodTx :: GoodTx -> Bool
validateGoodTx (getGoodTx -> ls) =
    let triple@(tx, inpResolver, _) =
            getTxFromGoodTx ls
        transactionIsVerified = isVerSuccess $ verifyTx inpResolver tx
        transactionReallyIsGood = individualTxPropertyVerifier triple
    in  transactionIsVerified == transactionReallyIsGood

overflowTx :: OverflowTx -> Bool
overflowTx (getOverflowTx -> ls) =
    let (tx@Tx{..}, inpResolver, extendedInputs) =
            getTxFromGoodTx ls
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver tx
        inpSumLessThanOutSum = not $ txChecksum extendedInputs txOutputs
    in inpSumLessThanOutSum == transactionIsNotVerified

signatureIsNotValid :: [TxOut] -> Maybe (TxIn, TxOut) -> Bool
signatureIsNotValid txOutputs (Just (TxIn{..}, TxOut{..})) =
    not $ verify (getAddress txOutAddress)
        (txInHash, txInIndex, txOutputs)
        txInSig
signatureIsNotValid _ _ = False

badSigsTx :: BadSigsTx -> Bool
badSigsTx (getBadSigsTx -> ls) =
    let (tx@Tx{..}, inpResolver, extendedInputs) =
            getTxFromGoodTx ls
        transactionIsNotVerified = isVerFailure $ verifyTx inpResolver tx
        notAllSignaturesAreValid = any (signatureIsNotValid txOutputs) extendedInputs
    in notAllSignaturesAreValid == transactionIsNotVerified