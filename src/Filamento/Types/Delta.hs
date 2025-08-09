module Filamento.Types.Delta
  ( Delta,
    deltaJustX,
    deltaJustY,
    deltaFromMm,
    deltaToMm,
  )
where

import Linear
import Relude

newtype Delta = Delta Double

deltaFromMm :: Double -> Delta
deltaFromMm v = Delta v

deltaToMm :: Delta -> Double
deltaToMm (Delta v) = v

deltaJustX :: Delta -> Delta
deltaJustX (Delta x) = Delta x

deltaJustY :: Delta -> Delta
deltaJustY (Delta y) = Delta y
