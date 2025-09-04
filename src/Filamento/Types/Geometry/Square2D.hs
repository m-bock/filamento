module Filamento.Types.Geometry.Square2D (Square2D, square2FromMinSize, square2GetMinCorner, square2GetSize) where

import Filamento.Classes
import Filamento.Types.Contexts
import Filamento.Types.Geometry.Rect2D
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Filamento.Types.Trivial
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Square2D = Square2D (Rect2D)
  deriving (Show, Eq)

square2FromMinSize :: V2 Position -> Length -> Square2D
square2FromMinSize minCorner size =
  Square2D $ rect2From (FrontLeft minCorner, Size size')
  where
    size' :: V2 Length
    size' = V2 size size

square2GetMinCorner :: Square2D -> V2 Position
square2GetMinCorner (Square2D rect) = rect2GetMinCorner rect

square2GetSize :: Square2D -> V2 Length
square2GetSize (Square2D rect) = size
  where
    Size size = rect2To rect
