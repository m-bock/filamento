module Filamento.Types.Distance
  ( Distance,
    distanceScale,
    distanceFromMm,
    distanceToMm,
  )
where

import Relude

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

distanceToMm :: Distance -> Double
distanceToMm (Distance d) = d

distanceFromMm :: Double -> Distance
distanceFromMm d = Distance d

distanceScale :: Double -> Distance -> Distance
distanceScale factor (Distance d) = Distance (d * factor)