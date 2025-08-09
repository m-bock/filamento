module Filamento.Types.Position
  ( Position,
    posFromMm,
    posToMm,
  )
where

-- Pos

import Filamento.Types.Delta2D (Delta2D)
import qualified Filamento.Types.Delta2D as Disp2D
import Linear
import Relude

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating)

posFromMm :: Double -> Position
posFromMm v = Position v

posToMm :: Position -> Double
posToMm (Position v) = v