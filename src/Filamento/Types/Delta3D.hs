module Filamento.Types.Delta3D
  ( Delta3D,
    delta3justX,
    delta3justY,
    delta3justZ,
    delta3fromMm,
    delta3toMm,
  )
where

import Linear
import Relude

newtype Delta3D = Delta3D (V3 Double)

delta3fromMm :: V3 Double -> Delta3D
delta3fromMm v = Delta3D v

delta3toMm :: Delta3D -> V3 Double
delta3toMm (Delta3D v) = v

delta3justX :: Delta3D -> Delta3D
delta3justX (Delta3D (V3 x _ _)) = Delta3D (V3 x 0 0)

delta3justY :: Delta3D -> Delta3D
delta3justY (Delta3D (V3 _ y _)) = Delta3D (V3 0 y 0)

delta3justZ :: Delta3D -> Delta3D
delta3justZ (Delta3D (V3 _ _ z)) = Delta3D (V3 0 0 z)
