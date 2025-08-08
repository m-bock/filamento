module Filamento.Types.Position2D
  ( Position2D,
    addDisplacement,
    subtractDisplacement,
    fromMm,
    toMm,
  )
where

-- Pos

import Filamento.Types.Displacement2D (Displacement2D)
import Linear
import Relude

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

addDisplacement :: Position2D -> Displacement2D -> Position2D
addDisplacement = undefined

subtractDisplacement :: Position2D -> Displacement2D -> Position2D
subtractDisplacement = undefined

fromMm :: V2 Double -> Position2D
fromMm v = Position2D v

toMm :: Position2D -> V2 Double
toMm (Position2D v) = v