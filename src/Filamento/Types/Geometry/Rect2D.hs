module Filamento.Types.Geometry.Rect2D
  ( Rect2D,
    rect2GetMinCorner,
    rect2GetSize,
    Rect2FromTo (..),
  )
where

import Filamento.Classes
import Filamento.Types.Contexts
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Filamento.Types.Trivial
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Rect2D = Rect2D {minCorner :: V2 Position, size :: V2 Length}
  deriving (Show, Eq)

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetSize :: Rect2D -> V2 Length
rect2GetSize (Rect2D {size}) = size

-------------------------------------------------------------------------------

class Rect2FromTo a where
  rect2From :: a -> Rect2D
  rect2To :: Rect2D -> a

instance Rect2FromTo (FrontLeft, Size) where
  rect2From = undefined
  rect2To = undefined

instance Rect2FromTo (FrontRight, Size) where
  rect2From = undefined
  rect2To = undefined

instance Rect2FromTo (BackRight, Size) where
  rect2From = undefined
  rect2To = undefined

instance Rect2FromTo (BackLeft, Size) where
  rect2From = undefined
  rect2To = undefined

instance Rect2FromTo (Center, Size) where
  rect2From = undefined
  rect2To = undefined

-------------------------------------------------------------------------------

rect2From_FrontLeft_Size :: V2 Position -> V2 Length -> Rect2D
rect2From_FrontLeft_Size frontLeft size = Rect2D {minCorner = frontLeft, size = size}

rect2To_FrontLeft_Size :: Rect2D -> (V2 Position, V2 Length)
rect2To_FrontLeft_Size (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_FrontRight_Size :: V2 Position -> V2 Length -> Rect2D
rect2From_FrontRight_Size frontRight size = undefined

rect2To_FrontRight_Size :: Rect2D -> (V2 Position, V2 Length)
rect2To_FrontRight_Size (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_BackRight_Size :: V2 Position -> V2 Length -> Rect2D
rect2From_BackRight_Size backRight size = undefined

rect2To_BackRight_Size :: Rect2D -> (V2 Position, V2 Length)
rect2To_BackRight_Size (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_BackLeft_Size :: V2 Position -> V2 Length -> Rect2D
rect2From_BackLeft_Size backLeft size = undefined

rect2To_BackLeft_Size :: Rect2D -> (V2 Position, V2 Length)
rect2To_BackLeft_Size (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_FrontLeft_BackRight :: V2 Position -> V2 Position -> V2 Length -> Rect2D
rect2From_FrontLeft_BackRight frontLeft backRight = undefined

rect2To_FrontLeft_BackRight :: Rect2D -> (V2 Position, V2 Position, V2 Length)
rect2To_FrontLeft_BackRight (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_FrontRight_BackLeft :: V2 Position -> V2 Position -> V2 Length -> Rect2D
rect2From_FrontRight_BackLeft frontRight backLeft = undefined

rect2To_FrontRight_BackLeft :: Rect2D -> (V2 Position, V2 Position, V2 Length)
rect2To_FrontRight_BackLeft (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

rect2From_Center_Size :: V2 Position -> V2 Length -> Rect2D
rect2From_Center_Size center size = undefined

rect2To_Center_Size :: Rect2D -> (V2 Position, V2 Length)
rect2To_Center_Size (Rect2D {minCorner, size}) = undefined

-------------------------------------------------------------------------------

instance FromTo Rect2D (FrontLeft, BackRight) where
  to = undefined
  from = undefined

instance FromTo (FrontLeft, Size) Rect2D where
  to (Rect2D {minCorner, size}) = (FrontLeft minCorner, Size size)
  from ((FrontLeft mc), Size s) = Rect2D {minCorner = mc, size = s}

instance FromTo (BackRight, Size) Rect2D where
  to = undefined
  from = undefined

instance FromTo (Center, Size) Rect2D where
  to = undefined
  from = undefined
