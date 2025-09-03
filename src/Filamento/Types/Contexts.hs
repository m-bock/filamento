module Filamento.Types.Contexts where

import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

newtype Center = Center (V2 Position)
  deriving (Show, Eq)

newtype Size = Size (V2 Length)
  deriving (Show, Eq)

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
