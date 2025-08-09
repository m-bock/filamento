module Filamento.Types.Position2D
  ( Position2D,
    pos2addDelta,
    pos2subDelta,
    pos2FromMm,
    pos2ToMm,
  )
where

-- Pos

import Filamento.Types.Delta2D
import Linear
import Relude

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

pos2addDelta :: Position2D -> Delta2D -> Position2D
pos2addDelta pos disp = pos2FromMm (pos2ToMm pos + delta2toMm disp)

pos2subDelta :: Position2D -> Delta2D -> Position2D
pos2subDelta pos disp = pos2FromMm (pos2ToMm pos - delta2toMm disp)

pos2FromMm :: V2 Double -> Position2D
pos2FromMm v = Position2D v

pos2ToMm :: Position2D -> V2 Double
pos2ToMm (Position2D v) = v