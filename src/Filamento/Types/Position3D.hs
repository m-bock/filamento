module Filamento.Types.Position3D
  ( Position3D,
    addDisplacement,
    subtractDisplacement,
  )
where

-- Pos

import Filamento.Conversions
import Filamento.Types.Displacement3D
import Linear
import Relude

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)

addDisplacement :: Position3D -> Displacement3D -> Position3D
addDisplacement = undefined

subtractDisplacement :: Position3D -> Displacement3D -> Position3D
subtractDisplacement = undefined

instance Convert MM Position3D where
  from (MM v) = Position3D (V3 v 0 0)
  to (Position3D (V3 v _ _)) = MM v

instance Convert CM Position3D where
  from (CM v) = Position3D (V3 (v * 10) 0 0)
  to (Position3D (V3 v _ _)) = CM (v / 10)

instance Convert (V3 MM) Position3D where
  from (V3 (MM x) (MM y) (MM z)) = Position3D (V3 x y z)
  to (Position3D (V3 x y z)) = V3 (MM x) (MM y) (MM z)

instance Convert (V3 CM) Position3D where
  from (V3 (CM x) (CM y) (CM z)) = Position3D (V3 (x * 10) (y * 10) (z * 10))
  to (Position3D (V3 x y z)) = V3 (CM (x / 10)) (CM (y / 10)) (CM (z / 10))
