module Filamento.Types.Geometry.Square2D (Square2D, square2FromMinSize, square2GetMinCorner, square2GetSize) where

import Filamento.Types.Geometry.Rect2D
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Square2D = Square2D (Rect2D)
  deriving (Show, Eq)

square2FromMinSize :: V2 Position -> Delta -> Square2D
square2FromMinSize minCorner size = Square2D (rect2FromMinSize minCorner size')
  where
    size' :: V2 Delta
    size' = V2 size size

square2GetMinCorner :: Square2D -> V2 Position
square2GetMinCorner (Square2D rect) = rect2GetMinCorner rect

square2GetSize :: Square2D -> V2 Delta
square2GetSize (Square2D rect) = rect2GetSize rect
