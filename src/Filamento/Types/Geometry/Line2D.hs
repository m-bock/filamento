module Filamento.Types.Geometry.Line2D (Line2D, line2FromPoints, line2GetStart, line2GetEnd) where

import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Line2D = Line2D {start :: V2 Position, end :: V2 Position}
  deriving (Show, Eq)

line2FromPoints :: V2 Position -> V2 Position -> Line2D
line2FromPoints p1 p2 = Line2D {start = p1, end = p2}

line2GetStart :: Line2D -> V2 Position
line2GetStart (Line2D {start}) = start

line2GetEnd :: Line2D -> V2 Position
line2GetEnd (Line2D {end}) = end