module Filamento.Types.Speed
  ( Speed,
    spdFromMmPerMin,
    spdToMmPerMin,
    spdFromMmPerSec,
    spdToMmPerSec,
  )
where

import Relude

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

spdFromMmPerMin :: Double -> Speed
spdFromMmPerMin d = Speed (d / 60)

spdToMmPerMin :: Speed -> Double
spdToMmPerMin (Speed s) = s * 60

spdFromMmPerSec :: Double -> Speed
spdFromMmPerSec d = Speed d

spdToMmPerSec :: Speed -> Double
spdToMmPerSec (Speed s) = s