module Filamento.Types.Position (Position) where

-- Pos

import Filamento.Conversions
import Linear
import Relude

newtype Position = Position {mm :: V3 Double}
  deriving (Show, Eq, Num)

instance Convert MM Position where
  from (MM v) = Position (V3 v 0 0)
  to (Position (V3 v _ _)) = MM v

instance Convert CM Position where
  from (CM v) = Position (V3 (v * 10) 0 0)
  to (Position (V3 v _ _)) = CM (v / 10)

instance Convert (V3 MM) Position where
  from (V3 (MM x) (MM y) (MM z)) = Position (V3 x y z)
  to (Position (V3 x y z)) = V3 (MM x) (MM y) (MM z)

instance Convert (V3 CM) Position where
  from (V3 (CM x) (CM y) (CM z)) = Position (V3 (x * 10) (y * 10) (z * 10))
  to (Position (V3 x y z)) = V3 (CM (x / 10)) (CM (y / 10)) (CM (z / 10))
