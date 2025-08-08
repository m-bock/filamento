module Filamento.Types.Speed where

import Filamento.Conversions
import Relude

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

instance Convert MMPerSec Speed where
  from (MMPerSec v) = Speed v
  to (Speed v) = MMPerSec v

instance Convert MMPerMin Speed where
  from (MMPerMin v) = Speed (v / 60)
  to (Speed v) = MMPerMin (v * 60)