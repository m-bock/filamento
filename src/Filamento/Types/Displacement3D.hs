module Filamento.Types.Displacement3D
  ( Displacement3D,
    justX,
    justY,
    justZ,
  )
where

import Filamento.Conversions
import Filamento.Types.Distance
import Linear
import Linear.V (V)
import Relude

newtype Displacement3D = Displacement3D (V3 Double)

instance Convert (V3 Distance) Displacement3D where
  from = undefined
  to = undefined

instance Convert MM Displacement3D where
  from (MM v) = Displacement3D (V3 v 0 0)
  to (Displacement3D (V3 v _ _)) = MM v

instance Convert CM Displacement3D where
  from (CM v) = Displacement3D (V3 (v * 10) 0 0)
  to (Displacement3D (V3 v _ _)) = CM (v / 10)

instance Convert (V3 MM) Displacement3D where
  from (V3 (MM x) (MM y) (MM z)) = Displacement3D (V3 x y z)
  to (Displacement3D (V3 x y z)) = V3 (MM x) (MM y) (MM z)

instance Convert (V3 CM) Displacement3D where
  from (V3 (CM x) (CM y) (CM z)) = Displacement3D (V3 (x * 10) (y * 10) (z * 10))
  to (Displacement3D (V3 x y z)) = V3 (CM (x / 10)) (CM (y / 10)) (CM (z / 10))

justX :: Displacement3D -> Displacement3D
justX (Displacement3D (V3 x _ _)) = Displacement3D (V3 x 0 0)

justY :: Displacement3D -> Displacement3D
justY (Displacement3D (V3 _ y _)) = Displacement3D (V3 0 y 0)

justZ :: Displacement3D -> Displacement3D
justZ (Displacement3D (V3 _ _ z)) = Displacement3D (V3 0 0 z)
