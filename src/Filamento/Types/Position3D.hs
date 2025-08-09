module Filamento.Types.Position3D
  ( Position3D,
    pos3addDelta,
    pos3subDelta,
    pos3distance,
    pos3fromMm,
    pos3toMm,
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

pos3addDelta :: Position3D -> Delta3D -> Position3D
pos3addDelta pos disp = pos3fromMm (pos3toMm pos + delta3toMm disp)

pos3subDelta :: Position3D -> Delta3D -> Position3D
pos3subDelta pos disp = pos3fromMm (pos3toMm pos - delta3toMm disp)

pos3fromMm :: V3 Double -> Position3D
pos3fromMm v = Position3D v

pos3toMm :: Position3D -> V3 Double
pos3toMm (Position3D v) = v

pos3distance :: Position3D -> Position3D -> Distance
pos3distance (Position3D v1) (Position3D v2) = distanceFromMm (Lin.distance v1 v2)
