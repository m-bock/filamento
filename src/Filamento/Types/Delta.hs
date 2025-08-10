module Filamento.Types.Delta
  ( Delta,
    dltJustX,
    dltJustY,
    dltFromMm,
    dltToMm,
  )
where

import Linear
import Relude

newtype Delta = Delta Double

dltFromMm :: Double -> Delta
dltFromMm v = Delta v

-- | Convert delta to millimeters
dltToMm :: Delta -> Double
dltToMm (Delta v) = v

-- | Keep only X component (identity for 1D)
dltJustX :: Delta -> Delta
dltJustX (Delta x) = Delta x

-- | Keep only Y component (identity for 1D)
dltJustY :: Delta -> Delta
dltJustY (Delta y) = Delta y
