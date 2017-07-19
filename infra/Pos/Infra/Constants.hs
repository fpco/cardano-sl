module Pos.Infra.Constants
       ( InfraConstants (..)
       , infraConstants
       , neighborsSendThreshold
       ) where

import           Data.Aeson             (FromJSON (..), genericParseJSON)
import           Data.Tagged            (Tagged (..))
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

import           Pos.Util.Config        (IsConfig (..), configParser, parseFromCslConfig)
import           Pos.Util.Util          ()

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

infraConstants :: InfraConstants
infraConstants = case parseFromCslConfig configParser of
    Left err -> error (toText ("Couldn't parse infra config: " ++ err))
    Right x  -> x

data InfraConstants = InfraConstants
    { ccNtpResponseTimeout       :: !Int
      -- ^ How often request to NTP server and response collection
    , ccNtpPollDelay             :: !Int
      -- ^ How often send request to NTP server
    , ccNtpMaxError              :: !Int
    -- ^ Max NTP error (max difference between local and global time, which is trusted)

    , ccNeighboursSendThreshold  :: !Int
      -- ^ Broadcasting threshold
    , ccKademliaDumpInterval     :: !Int
      -- ^ Interval for dumping Kademlia state in slots
    , ccEnhancedMessageTimeout   :: !Word
    -- ^ We consider node as known if it was pinged at most @ccEnhancedMessageTimeout@ sec ago
    , ccEnhancedMessageBroadcast :: !Word
      -- ^ Number of nodes from batch for enhanced bessage broadcast
    , ccNetworkWaitLogInterval   :: !Int
      -- ^ Network wait logging interval in seconds
      --   (logging that some recv/send takes significant amount of time)

    --------------------------------------------------------------------------
    -- -- Relay
    --------------------------------------------------------------------------
    , ccMaxReqSize               :: !Word32
      -- ^ Maximum `ReqMsg` size in bytes
    , ccMaxInvSize               :: !Word32
      -- ^ Maximum `InvMsg` size in bytes
    , ccMaxMempoolMsgSize        :: !Word32
      -- ^ Maximum `MempoolMsg` size in bytes
    } deriving (Show, Generic)

instance FromJSON InfraConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig InfraConstants where
    configPrefix = Tagged Nothing

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConstants
