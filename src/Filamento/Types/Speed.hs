module Filamento.Types.Speed
  ( Speed,
    speedFromMmPerMin,
    speedToMmPerMin,
    speedFromMmPerSec,
    speedToMmPerSec,
  )
where

import Relude

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

speedFromMmPerMin :: Double -> Speed
speedFromMmPerMin d = Speed (d / 60)

speedToMmPerMin :: Speed -> Double
speedToMmPerMin (Speed s) = s * 60

speedFromMmPerSec :: Double -> Speed
speedFromMmPerSec d = Speed d

speedToMmPerSec :: Speed -> Double
speedToMmPerSec (Speed s) = s