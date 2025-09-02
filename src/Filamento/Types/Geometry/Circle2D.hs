module Filamento.Types.Geometry.Circle2D (Circle2D, circle2GetArea, circle2FromCenterRadius) where

import Filamento.Classes
import Filamento.Types.Geometry.Area
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Circle2D = Circle2D {center :: V2 Position, radius :: Length}

circle2GetArea :: Circle2D -> Area
circle2GetArea (Circle2D {radius}) = fromSqMm $ pi * (toMm radius ^ 2)

circle2FromCenterRadius :: V2 Position -> Length -> Circle2D
circle2FromCenterRadius center radius = Circle2D {center, radius}
