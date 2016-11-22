{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtPayload(..)
       , GtProof(..)
       , GtMessage(..)
       , filterGtPayload
       , mkGtProof
       , verifyGtPayload

       -- * Lenses
       -- ** GtPayload
       , mdCommitments
       , mdOpenings
       , mdShares
       , mdVssCertificates

       -- * Utilities
       , hasCommitment
       , hasOpening
       , hasShares
       ) where

import           Control.Lens                  (makeLenses, (^.))
import           Data.Binary                   (Binary)
import qualified Data.HashMap.Strict           as HM
import           Data.Ix                       (inRange)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.MessagePack              (MessagePack)
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import           Data.Text.Buildable           (Buildable (..))
import           Formatting                    (bprint, (%))
import           Serokell.Util                 (VerificationRes, isVerSuccess, listJson,
                                                verifyGeneric)
import           Universum

import           Pos.Constants                 (k)
import           Pos.Crypto                    (Hash, PublicKey, Share, hash)
import           Pos.Ssc.Class.Types           (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Base (CommitmentsMap, Opening, OpeningsMap,
                                                SharesMap, SignedCommitment,
                                                VssCertificate, VssCertificatesMap,
                                                checkCert, isCommitmentId, isOpeningId,
                                                isSharesId, verifySignedCommitment)
import           Pos.Types                     (MainBlockHeader, SlotId (..), headerSlot)

import           Control.TimeWarp.Rpc          (Message (..))

----------------------------------------------------------------------------
-- SscMessage
----------------------------------------------------------------------------

-- | @GotTossing@ messages for protocol.
data GtMessage
    = DSCommitments     !(NonEmpty (PublicKey, SignedCommitment))
    | DSOpenings        !(NonEmpty (PublicKey, Opening))

      -- | First public key (external) belongs to node who decrypted
      -- shares and sent them to us .
      --
      -- Second public key (internal) means that 'share' was generated by owner
      -- of this public key.
    | DSSharesMulti     !(NonEmpty (PublicKey, (HashMap PublicKey Share)))
    | DSVssCertificates !(NonEmpty (PublicKey, VssCertificate))
    deriving (Show, Generic)

instance Binary GtMessage

instance Message GtMessage where
    messageName _ = "GtMessage"

deriveSafeCopySimple 0 'base ''GtMessage

----------------------------------------------------------------------------
-- SscPayload
----------------------------------------------------------------------------

-- | MPC-related content of main body.
data GtPayload = GtPayload
    { -- | Commitments are added during the first phase of epoch.
      _mdCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _mdOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _mdShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _mdVssCertificates :: !VssCertificatesMap
    } deriving (Show, Generic)

deriveSafeCopySimple 0 'base ''GtPayload
makeLenses ''GtPayload

instance Binary GtPayload
instance MessagePack GtPayload

instance Buildable GtPayload where
    build GtPayload {..} =
        mconcat
            [ formatCommitments
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments =
            formatIfNotNull
                ("  commitments from: "%listJson%"\n")
                (HM.keys _mdCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _mdOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _mdShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (HM.keys _mdVssCertificates)

{-|

Verify payload using header containing this payload.

For each DS datum we check:

  1. Whether it's stored in the correct block (e.g. commitments have to be in
     first k blocks, etc.)

  2. Whether the message itself is correct (e.g. commitment signature is
     valid, etc.)

We also do some general sanity checks.
-}
verifyGtPayload
    :: (SscPayload ssc ~ GtPayload)
    => MainBlockHeader ssc -> SscPayload ssc -> VerificationRes
verifyGtPayload header GtPayload {..} =
    verifyGeneric allChecks
  where
    slotId       = header ^. headerSlot
    epochId      = siEpoch slotId
    commitments  = _mdCommitments
    openings     = _mdOpenings
    shares       = _mdShares
    certificates = _mdVssCertificates
    isComm       = isCommitmentId slotId
    isOpen       = isOpeningId slotId
    isShare      = isSharesId slotId

    -- We *forbid* blocks from having commitments/openings/shares in blocks
    -- with wrong slotId (instead of merely discarding such commitments/etc)
    -- because it's the miner's responsibility not to include them into the
    -- block if they're late.
    --
    -- For commitments specifically, we also
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves, e. g.
    --     checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    -- TODO: we might also check that all share IDs are different, because
    -- then we would be able to simplify 'calculateSeed' a bit – however,
    -- it's somewhat complicated because we have encrypted shares, shares in
    -- commitments, etc.
    commChecks =
        [ (null openings,
                "there are openings in a commitment block")
        , (null shares,
                "there are shares in a commitment block")
        , (let checkSignedComm = isVerSuccess .
                    uncurry (flip verifySignedCommitment epochId)
            in all checkSignedComm (HM.toList commitments),
                "verifySignedCommitment has failed for some commitments")
        ]

    -- For openings, we check that
    --   * there are only openings in the block
    openChecks =
        [ (null commitments,
                "there are commitments in an openings block")
        , (null shares,
                "there are shares in an openings block")
        ]

    -- For shares, we check that
    --   * there are only shares in the block
    shareChecks =
        [ (null commitments,
                "there are commitments in a shares block")
        , (null openings,
                "there are openings in a shares block")
        ]

    -- For all other blocks, we check that
    --   * there are no commitments, openings or shares
    otherBlockChecks =
        [ (null commitments,
                "there are commitments in an ordinary block")
        , (null openings,
                "there are openings in an ordinary block")
        , (null shares,
                "there are shares in an ordinary block")
        ]

    -- For all blocks (no matter the type), we check that
    --   * slot ID is in range
    --   * VSS certificates are signed properly
    otherChecks =
        [ (inRange (0, 6 * k - 1) (siSlot slotId),
                "slot id is outside of [0, 6k)")
        , (all checkCert (HM.toList certificates),
                "some VSS certificates aren't signed properly")
        ]

    allChecks = concat $ concat
        [ [ commChecks       | isComm ]
        , [ openChecks       | isOpen ]
        , [ shareChecks      | isShare ]
        , [ otherBlockChecks | all not [isComm, isOpen, isShare] ]
        , [ otherChecks ]
        ]


-- | Remove messages irrelevant to given slot id from payload.
-- E.g. commitments are irrelevant if this isn't a commitment slot, etc.
filterGtPayload :: SlotId -> GtPayload -> GtPayload
filterGtPayload slotId GtPayload {..} =
    GtPayload
    { _mdCommitments = filteredCommitments
    , _mdOpenings = filteredOpenings
    , _mdShares = filteredShares
    , ..
    }
  where
    filteredCommitments = filterDo isCommitmentId _mdCommitments
    filteredOpenings = filterDo isOpeningId _mdOpenings
    filteredShares = filterDo isSharesId _mdShares
    filterDo
        :: Monoid container
        => (SlotId -> Bool) -> container -> container
    filterDo checker container
        | checker slotId = container
        | otherwise = mempty

----------------------------------------------------------------------------
-- SscProof
----------------------------------------------------------------------------

-- | Proof of MpcData.
-- We can use ADS for commitments, opennings, shares as well,
-- if we find it necessary.
data GtProof = GtProof
    { mpCommitmentsHash     :: !(Hash CommitmentsMap)
    , mpOpeningsHash        :: !(Hash OpeningsMap)
    , mpSharesHash          :: !(Hash SharesMap)
    , mpVssCertificatesHash :: !(Hash VssCertificatesMap)
    } deriving (Show, Eq, Generic)

deriveSafeCopySimple 0 'base ''GtProof

instance Binary GtProof
instance MessagePack GtProof

-- | Smart constructor for 'GtProof' from 'GtPayload'.
mkGtProof :: GtPayload -> GtProof
mkGtProof GtPayload {..} =
    GtProof
    { mpCommitmentsHash = hash _mdCommitments
    , mpOpeningsHash = hash _mdOpenings
    , mpSharesHash = hash _mdShares
    , mpVssCertificatesHash = hash _mdVssCertificates
    }

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

-- | Checks if 'GtPayload' has commitment from given 'PublicKey'.
hasCommitment :: PublicKey -> GtPayload -> Bool
hasCommitment pk md = HM.member pk (_mdCommitments md)

-- | Checks if 'GtPayload' has opening from given 'PublicKey'.
hasOpening :: PublicKey -> GtPayload -> Bool
hasOpening pk md = HM.member pk (_mdOpenings md)

-- | Checks if 'GtPayload' has shares from given 'PublicKey'.
hasShares :: PublicKey -> GtPayload -> Bool
hasShares pk md = HM.member pk (_mdShares md)