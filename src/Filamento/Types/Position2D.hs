module Filamento.Types.Position2D
  ( Position2D,
    pos2AddDelta,
    pos2SubDelta,
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

pos2AddDelta :: Position2D -> Delta2D -> Position2D
pos2AddDelta pos disp = pos2FromMm (pos2ToMm pos + dlt2ToMm disp)

pos2SubDelta :: Position2D -> Delta2D -> Position2D
pos2SubDelta pos disp = pos2FromMm (pos2ToMm pos - dlt2ToMm disp)

pos2FromMm :: V2 Double -> Position2D
pos2FromMm v = Position2D v

pos2ToMm :: Position2D -> V2 Double
pos2ToMm (Position2D v) = v