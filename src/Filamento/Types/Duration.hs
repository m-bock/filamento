module Filamento.Types.Duration where

import Filamento.Conversions
import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

instance Convert Sec Duration where
  from (Sec d) = Duration (d * 1000)
  to (Duration ms) = Sec (ms / 1000)

instance Convert MS Duration where
  from (MS d) = Duration d
  to (Duration ms) = MS ms