{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores stakes.

module Pos.Txp.DB.Balances
       (
         -- * Operations
         BalancesOp (..)

         -- * Getters
       , isBootstrapEra
       , getEffectiveTotalStake
       , getEffectiveStake
       , genesisFakeTotalStake

         -- * Initialization
       , prepareGStateBalances

         -- * Iteration
       , BalanceIter
       , balanceSource

         -- * Sanity checks
       , sanityCheckBalances
       ) where

import           Data.Conduit                 (Source, runConduit, (.|))
import qualified Data.Conduit.List            as CL
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text.Buildable
import qualified Database.RocksDB             as Rocks
import           Formatting                   (bprint, bprint, sformat, (%))
import           Serokell.Util                (Color (Red), colorize)
import           System.Wlog                  (WithLogger, logDebug, logError)
import           Universum

import           Pos.Binary.Class             (encodeStrict)
import           Pos.Core                     (Coin, StakeholderId, coinF, mkCoin,
                                               sumCoins, unsafeAddCoin,
                                               unsafeIntegerToCoin)
import qualified Pos.Core.Constants           as Const
import           Pos.Core.Genesis             (genesisBalances)
import           Pos.Crypto                   (shortHashF)
import           Pos.DB                       (DBError (..), DBTag (GStateDB), IterType,
                                               MonadDB, MonadDBRead, RocksBatchOp (..),
                                               dbIterSource)
import           Pos.DB.GState.Balances       (BalanceIter, ftsStakeKey, ftsSumKey,
                                               getRealStake, getRealStakeSumMaybe,
                                               getRealTotalStake)
import           Pos.DB.GState.Common         (gsPutBi)
import           Pos.Txp.Core                 (txOutStake)
import           Pos.Txp.Toil.Types           (Utxo)
import           Pos.Txp.Toil.Utxo            (utxoToStakes)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data BalancesOp
    = PutFtsSum !Coin
    | PutFtsStake !StakeholderId !Coin

instance Buildable BalancesOp where
    build (PutFtsSum c) = bprint ("PutFtsSum ("%coinF%")") c
    build (PutFtsStake ad c) =
        bprint ("PutFtsStake ("%shortHashF%", "%coinF%")") ad c

instance RocksBatchOp BalancesOp where
    toBatchOp (PutFtsSum c)      = [Rocks.Put ftsSumKey (encodeStrict c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkCoin 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (encodeStrict c)]

----------------------------------------------------------------------------
-- Overloaded getters (for fixed balances for bootstrap era)
----------------------------------------------------------------------------

-- TODO: provide actual implementation after corresponding
-- flag is actually stored in the DB
isBootstrapEra :: MonadDBRead m => m Bool
isBootstrapEra = pure $ not Const.isDevelopment && True

genesisFakeTotalStake :: Coin
genesisFakeTotalStake = unsafeIntegerToCoin $ sumCoins genesisBalances

getEffectiveTotalStake :: MonadDBRead m => m Coin
getEffectiveTotalStake = ifM isBootstrapEra
    (pure genesisFakeTotalStake)
    getRealTotalStake

getEffectiveStake :: MonadDBRead m => StakeholderId -> m (Maybe Coin)
getEffectiveStake id = ifM isBootstrapEra
    (pure $ HM.lookup id genesisBalances)
    (getRealStake id)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBalances
    :: forall m.
       MonadDB m
    => Utxo -> m ()
prepareGStateBalances genesisUtxo = do
    whenNothingM_ getRealStakeSumMaybe putFtsStakes
    whenNothingM_ getRealStakeSumMaybe putGenesisTotalStake
  where
    totalCoins = sumCoins $ map snd $ concatMap txOutStake $ toList genesisUtxo
    -- Will 'error' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) . HM.toList $ utxoToStakes genesisUtxo

putTotalFtsStake :: MonadDB m => Coin -> m ()
putTotalFtsStake = gsPutBi ftsSumKey

----------------------------------------------------------------------------
-- Balance
----------------------------------------------------------------------------

-- | Run iterator over effective balances.
balanceSource
    :: forall m a . (MonadDBRead m)
    => (Source m (IterType BalanceIter) -> m a)
    -> m a
balanceSource withSrc =
    ifM isBootstrapEra
        (dbIterSource GStateDB (Proxy @BalanceIter) withSrc)
        (withSrc (CL.sourceList $ HM.toList genesisBalances))

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadDBRead m, WithLogger m)
    => m ()
sanityCheckBalances = do
    logDebug "sanityCheckBalances running conduit"
    calculatedTotalStake <- do
        logDebug "We're inside runResourceT, i will run conduit"
        v <- dbIterSource GStateDB (Proxy @BalanceIter) $ \src ->
             runConduit
           $ src
          .| CL.map snd
          .| CL.fold unsafeAddCoin (mkCoin 0)
        logDebug "Conduit done, returning value"
        pure v
    logDebug "sanityCheckBalances running conduit DONE"

    totalStake <- getRealTotalStake
    let fmt =
            ("Wrong real total stake: \
             \sum of real stakes: "%coinF%
             ", but getRealTotalStake returned: "%coinF)
    let msg = sformat fmt calculatedTotalStake totalStake
    unless (calculatedTotalStake == totalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB m => StakeholderId -> Coin -> m ()
putFtsStake = gsPutBi . ftsStakeKey
