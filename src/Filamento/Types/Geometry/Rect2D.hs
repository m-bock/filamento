module Filamento.Types.Geometry.Rect2D
  ( Rect2D,
    rect2GetMinCorner,
    rect2GetSize,
    Center (..),
    Size (..),
    MinCorner (..),
    MaxCorner (..),
  )
where

import Filamento.Classes
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Rect2D = Rect2D {minCorner :: V2 Position, size :: V2 Delta}
  deriving (Show, Eq)

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetSize :: Rect2D -> V2 Delta
rect2GetSize (Rect2D {size}) = size

-------------------------------------------------------------------------------

newtype Center = Center (V2 Position)

newtype Size = Size (V2 Delta)

newtype MinCorner = MinCorner (V2 Position)

newtype MaxCorner = MaxCorner (V2 Position)

instance FromTo Rect2D (MinCorner, MaxCorner) where
  to = undefined
  from = undefined

instance FromTo (MinCorner, Size) Rect2D where
  to (Rect2D {minCorner, size}) = (MinCorner minCorner, Size size)
  from ((MinCorner mc), Size s) = Rect2D {minCorner = mc, size = s}

instance FromTo (MaxCorner, Size) Rect2D where
  to = undefined
  from = undefined

instance FromTo (Center, Size) Rect2D where
  to = undefined
  from = undefined
