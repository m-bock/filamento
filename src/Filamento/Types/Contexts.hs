module Filamento.Types.Contexts where

import Filamento.Types.MeasureUnits (Mm)
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

newtype Center = Center (V2 Position)
  deriving (Show, Eq)

class FromToCenter a where
  centerFrom :: a -> Center
  centerTo :: Center -> a

instance FromToCenter (V2 Position) where
  centerFrom v = Center v
  centerTo (Center v) = v

instance FromToCenter (V2 Mm) where
  centerFrom v = Center (fmap posFrom v)
  centerTo (Center v) = fmap posTo v

centerBy :: (Double, Double) -> Center
centerBy (x, y) = Center (fmap posBy (V2 x y))

-------------------------------------------------------------------------------

newtype Size = Size (V2 Length)
  deriving (Show, Eq)

class FromToSize a where
  sizeFrom :: a -> Size
  sizeTo :: Size -> a

instance FromToSize (V2 Length) where
  sizeFrom v = Size v
  sizeTo (Size v) = v

sizeBy :: (Double, Double) -> Size
sizeBy (x, y) = Size (fmap lengthBy (V2 x y))

sizeGetWidth :: Size -> Length
sizeGetWidth (Size (V2 width _)) = width

sizeGetHeight :: Size -> Length
sizeGetHeight (Size (V2 _ height)) = height

newtype FrontLeft = FrontLeft (V2 Position)
  deriving (Show, Eq)

newtype FrontRight = FrontRight (V2 Position)
  deriving (Show, Eq)

newtype BackRight = BackRight (V2 Position)
  deriving (Show, Eq)

newtype BackLeft = BackLeft (V2 Position)
  deriving (Show, Eq)

-- newtype Radius = Radius Length

-- newtype Diameter = Diameter Length
