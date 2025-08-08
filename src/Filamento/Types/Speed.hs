module Filamento.Types.Speed
  ( Speed,
    fromMmPerMin,
    toMmPerMin,
    fromMmPerSec,
    toMmPerSec,
  )
where

import Relude

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

fromMmPerMin :: Double -> Speed
fromMmPerMin d = Speed (d / 60)

toMmPerMin :: Speed -> Double
toMmPerMin (Speed s) = s * 60

fromMmPerSec :: Double -> Speed
fromMmPerSec d = Speed d

toMmPerSec :: Speed -> Double
toMmPerSec (Speed s) = s