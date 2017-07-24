-- | 'Arbitrary' unsafe instances for some types from 'Pos.Core.Types'.

module Pos.Arbitrary.Core.Unsafe () where

import           Universum

import           Data.Default                (def)

import           Pos.Arbitrary.Core          ()
import           Pos.Arbitrary.Crypto.Unsafe ()
import           Pos.Binary.Crypto           ()
import           Pos.Core                    (Address (..), Coin, EpochIndex (..),
                                              LocalSlotIndex, SharedSeed (..),
                                              SlotId (..), mkCoin)
import           Pos.Data.Attributes         (mkAttributes)
import           Pos.Util.Arbitrary          (ArbitraryUnsafe (..))

deriving instance ArbitraryUnsafe SharedSeed
deriving instance ArbitraryUnsafe EpochIndex

instance ArbitraryUnsafe LocalSlotIndex where

instance ArbitraryUnsafe Coin where
    arbitraryUnsafe = mkCoin <$> arbitraryUnsafe

instance ArbitraryUnsafe Address where
    arbitraryUnsafe = PubKeyAddress <$> arbitraryUnsafe
                                    <*> pure (mkAttributes def)

instance ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe
