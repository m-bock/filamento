module Filamento.Types.Position3D
  ( Position3D,
    addDisplacement,
    subtractDisplacement,
    distance,
    fromMm,
    toMm,
  )
where

-- Pos

import Filamento.Conversions
import Filamento.Types.Displacement3D (Displacement3D)
import qualified Filamento.Types.Displacement3D as Disp3D
import Filamento.Types.Distance (Distance)
import qualified Filamento.Types.Distance as Distance
import Linear hiding (distance)
import qualified Linear as Lin
import Relude

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)

addDisplacement :: Position3D -> Displacement3D -> Position3D
addDisplacement = undefined

subtractDisplacement :: Position3D -> Displacement3D -> Position3D
subtractDisplacement = undefined

fromMm :: V3 Double -> Position3D
fromMm v = Position3D v

toMm :: Position3D -> V3 Double
toMm (Position3D v) = v

distance :: Position3D -> Position3D -> Distance
distance (Position3D v1) (Position3D v2) = Distance.fromMm (Lin.distance v1 v2)
