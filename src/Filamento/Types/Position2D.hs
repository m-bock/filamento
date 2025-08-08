module Filamento.Types.Position2D
  ( Position2D,
    addDisplacement,
    subtractDisplacement,
    fromMm,
    toMm,
    pos2FromMm,
    pos2ToMm,
  )
where

-- Pos

import Filamento.Types.Displacement2D (Displacement2D)
import qualified Filamento.Types.Displacement2D as Disp2D
import Linear
import Relude

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

addDisplacement :: Position2D -> Displacement2D -> Position2D
addDisplacement pos disp = fromMm (toMm pos + Disp2D.toMm disp)

subtractDisplacement :: Position2D -> Displacement2D -> Position2D
subtractDisplacement pos disp = fromMm (toMm pos - Disp2D.toMm disp)

fromMm :: V2 Double -> Position2D
fromMm v = Position2D v

toMm :: Position2D -> V2 Double
toMm (Position2D v) = v

pos2FromMm :: V2 Double -> Position2D
pos2FromMm v = Position2D v

pos2ToMm :: Position2D -> V2 Double
pos2ToMm (Position2D v) = v