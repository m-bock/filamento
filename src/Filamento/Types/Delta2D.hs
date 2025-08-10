module Filamento.Types.Delta2D
  ( Delta2D,
    dlt2JustX,
    dlt2JustY,
    dlt2FromMm,
    dlt2ToMm,
  )
where

import Linear
import Relude

newtype Delta2D = Delta2D (V2 Double)

dlt2FromMm :: V2 Double -> Delta2D
dlt2FromMm v = Delta2D v

dlt2ToMm :: Delta2D -> V2 Double
dlt2ToMm (Delta2D v) = v

dlt2JustX :: Delta2D -> Delta2D
dlt2JustX (Delta2D (V2 x _)) = Delta2D (V2 x 0)

dlt2JustY :: Delta2D -> Delta2D
dlt2JustY (Delta2D (V2 _ y)) = Delta2D (V2 0 y)
