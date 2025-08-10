module Filamento.Types.Delta3D
  ( Delta3D,
    dlt3JustX,
    dlt3JustY,
    dlt3JustZ,
    dlt3FromMm,
    dlt3ToMm,
  )
where

import Linear
import Relude

newtype Delta3D = Delta3D (V3 Double)

dlt3FromMm :: V3 Double -> Delta3D
dlt3FromMm v = Delta3D v

dlt3ToMm :: Delta3D -> V3 Double
dlt3ToMm (Delta3D v) = v

dlt3JustX :: Delta3D -> Delta3D
dlt3JustX (Delta3D (V3 x _ _)) = Delta3D (V3 x 0 0)

dlt3JustY :: Delta3D -> Delta3D
dlt3JustY (Delta3D (V3 _ y _)) = Delta3D (V3 0 y 0)

dlt3JustZ :: Delta3D -> Delta3D
dlt3JustZ (Delta3D (V3 _ _ z)) = Delta3D (V3 0 0 z)
