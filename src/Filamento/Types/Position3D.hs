module Filamento.Types.Position3D
  ( Position3D,
    pos3AddDelta,
    pos3SubDelta,
    pos3Distance,
    pos3FromMm,
    pos3ToMm,
  )
where

-- Pos

import Filamento.Types.Delta3D
import Filamento.Types.Distance (Distance)
import Filamento.Types.Distance as Distance
import Linear hiding (distance)
import qualified Linear as Lin
import Relude

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)

pos3AddDelta :: Position3D -> Delta3D -> Position3D
pos3AddDelta pos disp = pos3FromMm (pos3ToMm pos + dlt3ToMm disp)

pos3SubDelta :: Position3D -> Delta3D -> Position3D
pos3SubDelta pos disp = pos3FromMm (pos3ToMm pos - dlt3ToMm disp)

pos3FromMm :: V3 Double -> Position3D
pos3FromMm v = Position3D v

pos3ToMm :: Position3D -> V3 Double
pos3ToMm (Position3D v) = v

pos3Distance :: Position3D -> Position3D -> Distance
pos3Distance (Position3D v1) (Position3D v2) = distFromMm (Lin.distance v1 v2)
