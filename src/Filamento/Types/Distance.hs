module Filamento.Types.Distance
  ( Distance,
    scale,
  )
where

import Filamento.Conversions
import Relude

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

instance Convert MM Distance where
  from (MM v) = Distance v
  to (Distance v) = MM v

scale :: Double -> Distance -> Distance
scale factor (Distance d) = Distance (d * factor)