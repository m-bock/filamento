module Filamento.Types.Distance
  ( Distance,
    scale,
    fromMm,
    toMm,
  )
where

import Relude

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

toMm :: Distance -> Double
toMm (Distance d) = d

fromMm :: Double -> Distance
fromMm d = Distance d

scale :: Double -> Distance -> Distance
scale factor (Distance d) = Distance (d * factor)