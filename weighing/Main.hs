{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where


import qualified Crypto.Hash.SHA1
import qualified Data.Digest.SHA1

import           Crypto.Hash (Blake2b_224, Digest, SHA3_256, hashlazy, hash)
import qualified Crypto.Hash as CryptoHash
import           Data.ByteArray (ByteArrayAccess)
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet, decodeBase58,
                                         encodeBase58)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as L
import           Data.Hashable (Hashable (..))
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, later, (%))
import           Serokell.Util.Base16 (base16F)
import           Universum

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Crypto ()
import           Pos.Core.Types         (AddrPkAttrs (..), Address (..), AddressHash,
                                         Script, StakeholderId)
import           Pos.Crypto             (AbstractHash (AbstractHash), PublicKey,
                                         RedeemPublicKey, SecretKey, toPublic)
import           Pos.Crypto.HD          (HDAddressPayload, HDPassphrase,
                                         deriveHDPublicKey, deriveHDSecretKey,
                                         packHDAddressAttr)
import           Pos.Data.Attributes


import           Pos.Data.Attributes (mkAttributes)

import           Pos.Core.Types         (AddrPkAttrs (..), Address (..), AddressHash,
                                         Script, StakeholderId)

import           Control.DeepSeq
import           Data.Default (Default (..))
import           Pos.Binary.Class

import           Data.List (genericLength, genericReplicate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Formatting (int, sformat, (%))
import qualified Pos.Constants as Const
import           Pos.Core.Types (StakeholderId)
import           Pos.Crypto         (PublicKey, SecretKey, deterministicKeyGen,
                                     unsafeHash)
import           Pos.Genesis.Parser (compileGenData)
import           Pos.Genesis.Types  (GenesisData (..), StakeDistribution (..),
                                     getTotalStake)
import           Pos.Lrc.FtsPure (followTheSatoshi)
import           Pos.Txp.Core.Types (TxIn (..), TxOut (..), TxOutAux (..),
                                     TxOutDistribution)
import           Pos.Txp.Toil.Types
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Types          (Address (..), Coin, SharedSeed (SharedSeed),
                                     SlotLeaders, applyCoinPortion, coinToInteger,
                                     divCoin, makePubKeyAddress, mkCoin, unsafeAddCoin,
                                     unsafeMulCoin)
import           Prelude ()
import           Serokell.Util (enumerate)
import           Universum
import           Weigh

main =
  mainWith
    (sequence
       (concat
          [ [ func
              ("genesisUtxo: " ++ show poorPeople)
              genesisUtxo
              (stakesDistr 1 poorPeople 500000000000 0.99)
            | poorPeople <- [1, 10, 100, 1000, 10000, 100000, 1000000]
            ]
          ,  [ func
               ("genesisUtxoModified: " ++ show poorPeople)
               genesisUtxoModified
               (stakesDistr 1 poorPeople 500000000000 0.99)
             | poorPeople <- [1, 10, 100, 1000, 10000, 100000, 1000000]
             ]
          ]))
  where
    stakesDistr richs poors coins richShare =
      checkConsistency $ RichPoorStakes {..}
      where
        sdRichmen = fromIntegral richs
        sdPoor = fromIntegral poors
        totalRichStake = round $ richShare * fromIntegral coins
        totalPoorStake = coins - totalRichStake
        richStake = totalRichStake `div` fromIntegral richs
        poorStake = totalPoorStake `div` fromIntegral poors
        sdRichStake = mkCoin $ fromIntegral richStake
        sdPoorStake = mkCoin $ fromIntegral poorStake
        checkConsistency =
          if poorStake <= 0 || richStake <= 0
            then error
                   "Impossible to make RichPoorStakes with given parameters."
            else identity

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    fromMaybe (error "deterministicKeyGen failed in Genesis") .
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map generateGenesisKeyPair [0 .. Const.genesisN - 1]

-- | List of 'PublicKey's in genesis.
genesisDevPublicKeys :: [PublicKey]
genesisDevPublicKeys = map fst genesisDevKeyPairs

-- | List of 'SecretKey's in genesis.
genesisDevSecretKeys :: [SecretKey]
genesisDevSecretKeys = map snd genesisDevKeyPairs

-- | List of addresses in genesis. See 'genesisPublicKeys'.
genesisAddresses :: [Address]
genesisAddresses
    | Const.isDevelopment = map makePubKeyAddress genesisDevPublicKeys
    | otherwise           = gdAddresses compileGenData

genesisStakeDistribution :: StakeDistribution
genesisStakeDistribution
    | Const.isDevelopment = def
    | otherwise           = gdDistribution compileGenData

genesisBalances :: HashMap StakeholderId Coin
genesisBalances
    | Const.isDevelopment = mempty
    | otherwise           = gdBootstrapBalances compileGenData

instance Default StakeDistribution where
    def = FlatStakes Const.genesisN
              (mkCoin 10000 `unsafeMulCoin` (Const.genesisN :: Int))

-- 10000 coins in total. For thresholds testing.
-- 0.5,0.25,0.125,0.0625,0.0312,0.0156,0.0078,0.0039,0.0019,0.0008,0.0006,0.0004,0.0002,0.0001
expTwoDistribution :: [Coin]
expTwoDistribution =
    map mkCoin [5000,2500,1250,625,312,156,78,39,19,8,6,4,2,1]

bitcoinDistribution20 :: [Coin]
bitcoinDistribution20 = map mkCoin
    [200,163,120,105,78,76,57,50,46,31,26,13,11,11,7,4,2,0,0,0]

stakeDistribution :: StakeDistribution -> [(Coin, TxOutDistribution)]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = (coins `divCoin` stakeholders, [])
stakeDistribution (BitcoinStakes stakeholders coins) =
    map ((, []) . normalize) $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x = x `unsafeMulCoin`
                  coinToInteger (coins `divCoin` (1000 :: Int))
stakeDistribution ExponentialStakes = map (, []) expTwoDistribution
stakeDistribution ts@RichPoorStakes {..} =
    checkMpcThd (getTotalStake ts) sdRichStake $
    map (, []) basicDist
  where
    -- Node won't start if richmen cannot participate in MPC
    checkMpcThd total richs =
        if richs < applyCoinPortion Const.genesisMpcThd total
        then error "Pos.Genesis: RichPoorStakes: richmen stake \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichStake ++
                genericReplicate sdPoor sdPoorStake
stakeDistribution (ExplicitStakes balances) =
    toList balances
stakeDistribution (CombinedStakes distA distB) =
    stakeDistribution distA <> stakeDistribution distB

bitcoinDistribution1000Coins :: Word -> [Coin]
bitcoinDistribution1000Coins stakeholders
    | stakeholders < 20 = map fst $ stakeDistribution
          (FlatStakes stakeholders (mkCoin 1000))
    | stakeholders == 20 = bitcoinDistribution20
    | otherwise =
        foldl' (bitcoinDistributionImpl ratio) [] $
        enumerate bitcoinDistribution20
  where
    ratio = fromIntegral stakeholders / 20

bitcoinDistributionImpl :: Double -> [Coin] -> (Int, Coin) -> [Coin]
bitcoinDistributionImpl ratio coins (coinIdx, coin) =
    coins ++ toAddValMax : replicate (toAddNum - 1) toAddValMin
  where
    toAddNumMax = ceiling ratio
    toAddNumMin = floor ratio
    toAddNum :: Int
    toAddNum =
        if genericLength coins + realToFrac toAddNumMax >
           realToFrac (coinIdx + 1) * ratio
            then toAddNumMin
            else toAddNumMax
    toAddValMin = coin `divCoin` toAddNum
    toAddValMax = coin `unsafeAddCoin`
                  (toAddValMin `unsafeMulCoin` (toAddNum - 1))

-- | Genesis 'Utxo'.
genesisUtxo sd =
    take (length (stakeDistribution sd)) $
    tailAddresses
  where
    zipF (coin, distr) addr =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) distr
        )
    tailAddresses = map (makePubKeyAddress . fst . generateGenesisKeyPair)
        [Const.genesisN ..]

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

genesisSeed :: SharedSeed
genesisSeed = SharedSeed "vasa opasa skovoroda Ggurda boroda provoda"

-- | Leaders of genesis. See 'followTheSatoshi'.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders = followTheSatoshi genesisSeed


--------------------------------------------------------------------------------
-- Modified version of code

-- | Address is where you can send coins.
data Address'
    = PubKeyAddress'
          {
           addrKeyHash      :: !(AddressHash' PublicKey),
           addrPkAttributes :: !(Attributes AddrPkAttrs)
          }
  deriving (Generic)
instance NFData Address'

-- | Genesis 'Utxo'.
genesisUtxoModified sd =
    take (length (stakeDistribution sd)) $
    tailAddresses
  where
    zipF (coin, distr) addr =
        ( TxIn (unsafeHash addr) 0
        , TxOutAux (TxOut addr coin) distr
        )
    tailAddresses = map (makePubKeyAddress' . fst . generateGenesisKeyPair)
        [Const.genesisN ..]

-- | A function for making an address from PublicKey
makePubKeyAddress' :: Bi PublicKey => PublicKey -> Address'
makePubKeyAddress' key =
    PubKeyAddress' (dummyHash key)
                   (mkAttributes (AddrPkAttrs Nothing))

type AddressHash' = AbstractHash' Blake2b_224

newtype AbstractHash' algo a = AbstractHash' Data.Digest.SHA1.Word160
    deriving (Show, Eq, Ord, Generic, NFData)
deriving instance Ord    Data.Digest.SHA1.Word160
deriving instance NFData    Data.Digest.SHA1.Word160
deriving instance Generic    Data.Digest.SHA1.Word160

dummyHash :: Bi e => e -> (AddressHash' PublicKey)
dummyHash = AbstractHash' . Data.Digest.SHA1.hash . L.unpack . Bi.encode
