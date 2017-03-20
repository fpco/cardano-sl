{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Types.Relay
       ( InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , InvOrData
       ) where

import           Control.Lens        (Wrapped (..), iso)
import qualified Data.Text.Buildable as B
import           Formatting          (bprint, build, (%))
import           Universum

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key tag = InvMsg
    { imTag :: !tag
    , imKey :: !key
    }

deriving instance (Show key, Show tag) => Show (InvMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (InvMsg key tag)

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key tag = ReqMsg
    { rmTag :: !tag
    , rmKey :: !key
    }

deriving instance (Show key, Show tag) => Show (ReqMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (ReqMsg key tag)

-- | Data message. Can be used to send actual data.
data DataMsg contents = DataMsg
    { dmContents :: !contents
    } deriving (Show, Eq)

type InvOrData tag key contents = Either (InvMsg key tag) (DataMsg contents)

instance (Buildable contents) =>
         Buildable (DataMsg contents) where
    build (DataMsg contents) = bprint ("Data {" %build % "}") contents

instance Wrapped (DataMsg contents) where
    type Unwrapped (DataMsg contents) = contents
    _Wrapped' = iso dmContents DataMsg
