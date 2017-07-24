{-| Blockchain genesis. Not to be confused with genesis block in epoch.
    Blockchain genesis means genesis values which are hardcoded in advance
    (before system starts doing anything). Genesis block in epoch exists
    in every epoch and it's not known in advance.
-}

module Pos.Genesis
       (
         module Pos.Core.Genesis
       , module Pos.Ssc.GodTossing.Genesis

       -- * Static state
       , stakeDistribution
       , genesisUtxo
       , genesisDelegation
       , genesisSeed
       , genesisLeaders

       -- * Dev mode genesis
       , accountGenesisIndex
       , wAddressGenesisIndex
       , devStakesDistr
       , devAddrDistr
       ) where

import           Universum

import qualified Data.HashMap.Strict        as HM
import           Data.List                  (genericLength, genericReplicate)
import qualified Data.Map.Strict            as M
import           Serokell.Util              (enumerate)

import qualified Pos.Constants              as Const
import           Pos.Core                   (Address, Coin, SlotLeaders, StakeholderId,
                                             applyCoinPortionUp, coinToInteger,
                                             deriveLvl2KeyPair, divCoin,
                                             makePubKeyAddress, mkCoin, unsafeAddCoin,
                                             unsafeMulCoin)
import           Pos.Crypto                 (EncryptedSecretKey, emptyPassphrase,
                                             firstHardened, unsafeHash)
import           Pos.Lrc.FtsPure            (followTheSatoshi)
import           Pos.Lrc.Genesis            (genesisSeed)
import           Pos.Txp.Core               (TxIn (..), TxOut (..), TxOutAux (..),
                                             TxOutDistribution)
import           Pos.Txp.Toil               (Utxo, utxoToStakes)

-- reexports
import           Pos.Core.Genesis
import           Pos.Ssc.GodTossing.Genesis

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

bitcoinDistribution20 :: [Coin]
bitcoinDistribution20 = map mkCoin
    [200,163,120,105,78,76,57,50,46,31,26,13,11,11,7,4,2,0,0,0]

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

-- | Given the stake distribution, produces map from coin to txDistr
-- (which may be empty list).
stakeDistribution :: StakeDistribution -> [(Coin, TxOutDistribution)]
stakeDistribution (FlatStakes stakeholders coins) =
    genericReplicate stakeholders val
  where
    val = (coins `divCoin` stakeholders, [])
stakeDistribution (BitcoinStakes stakeholders coins) =
    map ((, []) . normalize) $ bitcoinDistribution1000Coins stakeholders
  where
    normalize x =
        x `unsafeMulCoin` coinToInteger (coins `divCoin` (1000 :: Int))
stakeDistribution (ExponentialStakes n) =
    [(mkCoin (2 ^ i), []) | i <- [n - 1, n - 2 .. 0]]
stakeDistribution ts@RichPoorStakes {..} =
    checkMpcThd (getTotalStake ts) sdRichStake $
    map (, []) basicDist
  where
    -- Node won't start if richmen cannot participate in MPC
    checkMpcThd total richs =
        if richs < applyCoinPortionUp Const.genesisMpcThd total
        then error "Pos.Genesis: RichPoorStakes: richmen stake \
                   \is less than MPC threshold"
        else identity
    basicDist = genericReplicate sdRichmen sdRichStake ++
                genericReplicate sdPoor sdPoorStake
stakeDistribution (CustomStakes coins) = map (,[]) coins

-- | Generates genesis 'Utxo' given optional boot stakeholders list
-- and address distribution. In case boot stakeholders are supplied,
-- all the balance is distributed among them. Otherwise, txDistr is
-- set to @[]@ or left as it is (in case of 'ExplicitStakes').
genesisUtxo :: Maybe (HashMap StakeholderId Word16) -> [AddrDistribution] -> Utxo
genesisUtxo bootStakeholders ad =
    M.fromList $ concatMap (uncurry toUtxo . second stakeDistribution) ad
  where
    defaultStakeDistr coin distr
        = maybe distr
                (\genDistr -> genesisSplitBoot genDistr coin)
                bootStakeholders
    toUtxo :: [Address] -> [(Coin, TxOutDistribution)] -> [(TxIn, TxOutAux)]
    toUtxo addr distrs =
        let utxoEntry a (coin,txOutDistr) =
                ( TxIn (unsafeHash a) 0
                , TxOutAux (TxOut a coin) (defaultStakeDistr coin txOutDistr)
                )
        in zipWith utxoEntry addr distrs

-- This is probably 100% useless.
genesisDelegation :: HashMap StakeholderId (HashSet StakeholderId)
genesisDelegation = mempty

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: Utxo -> SlotLeaders
genesisLeaders utxo =
    followTheSatoshi genesisSeed $ HM.toList $ utxoToStakes utxo

----------------------------------------------------------------------------
-- Development mode genesis
----------------------------------------------------------------------------

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened

-- | Chooses among common distributions for dev mode.
devStakesDistr
    :: Maybe (Int, Int)                   -- flat distr
    -> Maybe (Int, Int)                   -- bitcoin distr
    -> Maybe (Int, Int, Integer, Double)  -- rich/poor distr
    -> Maybe Int                          -- exp distr
    -> StakeDistribution
devStakesDistr Nothing Nothing Nothing Nothing = genesisDevFlatDistr
devStakesDistr (Just (nodes, coins)) Nothing Nothing Nothing =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
devStakesDistr Nothing (Just (nodes, coins)) Nothing Nothing =
    BitcoinStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
devStakesDistr Nothing Nothing (Just (richs, poors, coins, richShare)) Nothing =
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
        then error "Impossible to make RichPoorStakes with given parameters."
        else identity
devStakesDistr Nothing Nothing Nothing (Just n) =
    ExponentialStakes (fromIntegral n)
devStakesDistr _ _ _ _ =
    error "Conflicting distribution options were enabled. \
          \Choose one at most or nothing."

-- | Addresses and secret keys of genesis HD wallets' /addresses/.
-- It's important to return 'Address' here, not 'PublicKey', since valid HD
-- wallet address keeps 'HDAddressPayload' attribute which value depends on
-- secret key.
genesisDevHdwAccountKeyDatas :: [(Address, EncryptedSecretKey)]
genesisDevHdwAccountKeyDatas =
    genesisDevHdwSecretKeys <&> \key ->
        fromMaybe (error "Passphrase doesn't match in Genesis") $
        deriveLvl2KeyPair
            emptyPassphrase
            key
            accountGenesisIndex
            wAddressGenesisIndex

-- | Address distribution for dev mode. It's supposed that you pass
-- the distribution from 'devStakesDistr' here. This function will add
-- dev genesis addresses and hd addrs/distr.
devAddrDistr :: StakeDistribution -> [AddrDistribution]
devAddrDistr distr =
    [ (mainAddrs, distr)        -- Addresses from passed stake
    , (hdwAddresses, hdwDistr)  -- HDW addresses for testing
    ]
  where
    distrSize = length $ stakeDistribution distr
    mainAddrs =
        take distrSize $ genesisDevAddresses <> tailAddresses
    tailAddresses =
        map (makePubKeyAddress . fst . generateGenesisKeyPair)
            [Const.genesisKeysN ..]
    hdwSize = 2 -- should be positive
    -- 200 coins split among hdwSize users. Should be small sum enough
    -- to avoid making wallets slot leaders.
    hdwDistr = FlatStakes (fromIntegral hdwSize) (mkCoin 200)
    -- should be enough for testing.
    hdwAddresses = take hdwSize genesisDevHdwAccountAddresses

    genesisDevHdwAccountAddresses :: [Address]
    genesisDevHdwAccountAddresses = map fst genesisDevHdwAccountKeyDatas
