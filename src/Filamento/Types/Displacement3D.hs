module Filamento.Types.Displacement3D
  ( Displacement3D,
    justX,
    justY,
    justZ,
    fromMm,
    toMm,
  )
where

import Linear
import Linear.V (V)
import Relude

newtype Displacement3D = Displacement3D (V3 Double)

fromMm :: V3 Double -> Displacement3D
fromMm v = Displacement3D v

toMm :: Displacement3D -> V3 Double
toMm (Displacement3D v) = v

justX :: Displacement3D -> Displacement3D
justX (Displacement3D (V3 x _ _)) = Displacement3D (V3 x 0 0)

justY :: Displacement3D -> Displacement3D
justY (Displacement3D (V3 _ y _)) = Displacement3D (V3 0 y 0)

justZ :: Displacement3D -> Displacement3D
justZ (Displacement3D (V3 _ _ z)) = Displacement3D (V3 0 0 z)
