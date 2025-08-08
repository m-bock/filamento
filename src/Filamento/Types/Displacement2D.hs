module Filamento.Types.Displacement2D
  ( Displacement2D,
    justX,
    justY,
  )
where

import Filamento.Conversions
import Filamento.Types.Distance
import Linear
import Linear.V (V)
import Relude

newtype Displacement2D = Displacement2D (V2 Double)

instance Convert (V2 Distance) Displacement2D where
  from = undefined
  to = undefined

instance Convert MM Displacement2D where
  from (MM v) = Displacement2D (V2 v 0)
  to (Displacement2D (V2 v _)) = MM v

instance Convert CM Displacement2D where
  from (CM v) = Displacement2D (V2 (v * 10) 0)
  to (Displacement2D (V2 v _)) = CM (v / 10)

instance Convert (V2 MM) Displacement2D where
  from (V2 (MM x) (MM y)) = Displacement2D (V2 x y)
  to (Displacement2D (V2 x y)) = V2 (MM x) (MM y)

instance Convert (V2 CM) Displacement2D where
  from (V2 (CM x) (CM y)) = Displacement2D (V2 (x * 10) (y * 10))
  to (Displacement2D (V2 x y)) = V2 (CM (x / 10)) (CM (y / 10))

justX :: Displacement2D -> Displacement2D
justX (Displacement2D (V2 x _)) = Displacement2D (V2 x 0)

justY :: Displacement2D -> Displacement2D
justY (Displacement2D (V2 _ y)) = Displacement2D (V2 0 y)
