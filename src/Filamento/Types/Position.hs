module Filamento.Types.Position
  ( Position,
    fromMm,
    toMm,
  )
where

-- Pos

import Filamento.Types.Displacement2D (Displacement2D)
import qualified Filamento.Types.Displacement2D as Disp2D
import Linear
import Relude

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num)

fromMm :: Double -> Position
fromMm v = Position v

toMm :: Position -> Double
toMm (Position v) = v