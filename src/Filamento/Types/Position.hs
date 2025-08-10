module Filamento.Types.Position
  ( Position,
    posFromMm,
    posToMm,
    posAddDelta,
    posSubDelta,
  )
where

-- Pos

import Filamento.Types.Delta (Delta, dltToMm)
import qualified Filamento.Types.Delta2D as Disp2D
import Linear
import Relude

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating)

posFromMm :: Double -> Position
posFromMm v = Position v

posToMm :: Position -> Double
posToMm (Position v) = v

posAddDelta :: Position -> Delta -> Position
posAddDelta pos disp = posFromMm (posToMm pos + dltToMm disp)

posSubDelta :: Position -> Delta -> Position
posSubDelta pos disp = posFromMm (posToMm pos - dltToMm disp)