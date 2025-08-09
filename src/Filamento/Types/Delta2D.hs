module Filamento.Types.Delta2D
  ( Delta2D,
    delta2justX,
    delta2justY,
    delta2fromMm,
    delta2toMm,
  )
where

import Linear
import Relude

newtype Delta2D = Delta2D (V2 Double)

delta2fromMm :: V2 Double -> Delta2D
delta2fromMm v = Delta2D v

delta2toMm :: Delta2D -> V2 Double
delta2toMm (Delta2D v) = v

delta2justX :: Delta2D -> Delta2D
delta2justX (Delta2D (V2 x _)) = Delta2D (V2 x 0)

delta2justY :: Delta2D -> Delta2D
delta2justY (Delta2D (V2 _ y)) = Delta2D (V2 0 y)
