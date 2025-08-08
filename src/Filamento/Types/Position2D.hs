module Filamento.Types.Position2D
  ( Position2D,
    addDisplacement,
    subtractDisplacement,
  )
where

-- Pos

import Filamento.Conversions
import Filamento.Types.Displacement2D
import Linear
import Relude

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

addDisplacement :: Position2D -> Displacement2D -> Position2D
addDisplacement = undefined

subtractDisplacement :: Position2D -> Displacement2D -> Position2D
subtractDisplacement = undefined

instance Convert MM Position2D where
  from (MM v) = Position2D (V2 v 0)
  to (Position2D (V2 v _)) = MM v

instance Convert CM Position2D where
  from (CM v) = Position2D (V2 (v * 10) 0)
  to (Position2D (V2 v _)) = CM (v / 10)

instance Convert (V2 MM) Position2D where
  from (V2 (MM x) (MM y)) = Position2D (V2 x y)
  to (Position2D (V2 x y)) = V2 (MM x) (MM y)

instance Convert (V2 CM) Position2D where
  from (V2 (CM x) (CM y)) = Position2D (V2 (x * 10) (y * 10))
  to (Position2D (V2 x y)) = V2 (CM (x / 10)) (CM (y / 10))
