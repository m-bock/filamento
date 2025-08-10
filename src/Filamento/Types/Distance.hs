module Filamento.Types.Distance
  ( Distance,
    distScale,
    distFromMm,
    distToMm,
  )
where

import Relude

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

distToMm :: Distance -> Double
distToMm (Distance d) = d

distFromMm :: Double -> Distance
distFromMm d = Distance d

distScale :: Double -> Distance -> Distance
distScale factor (Distance d) = Distance (d * factor)