module Filamento.Types.Geometry.Rect2D (Rect2D, rect2FromMinSize, rect2GetMinCorner, rect2GetSize) where

import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Rect2D = Rect2D {minCorner :: V2 Position, size :: V2 Delta}
  deriving (Show, Eq)

rect2FromMinSize :: V2 Position -> V2 Delta -> Rect2D
rect2FromMinSize minCorner size = Rect2D {minCorner, size}

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetSize :: Rect2D -> V2 Delta
rect2GetSize (Rect2D {size}) = size
