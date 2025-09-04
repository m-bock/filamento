module Filamento.Types.Geometry.Rect2D
  ( Rect2D,
    rect2GetMinCorner,
    -- rect2GetSize,
    Rect2From (..),
    Rect2To (..),
  )
where

import Filamento.Classes
import Filamento.Classes.Abs (FromToAbs (..))
import Filamento.Classes.Distance (Distance (..))
import Filamento.Classes.Move
import Filamento.Types.Contexts
import Filamento.Types.Continous.AbsFactor (FromToAbsFactor (..))
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Rect2D = Rect2D (FrontLeft, Size)
  deriving (Show, Eq)

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D (FrontLeft frontLeft, _)) = frontLeft

rect2GetSize :: Rect2D -> V2 Length
rect2GetSize (Rect2D (_, Size size)) = size

-------------------------------------------------------------------------------

class Rect2From a where
  rect2From :: a -> Rect2D

class Rect2To a where
  rect2To :: Rect2D -> a

instance Rect2From (FrontLeft, Size) where
  rect2From = Rect2D

instance Rect2To (FrontLeft, Size) where
  rect2To (Rect2D (frontLeft, size)) =
    ( frontLeft,
      size
    )

instance Rect2From (FrontRight, Size) where
  rect2From (FrontRight frontRight, size) =
    Rect2D
      ( FrontLeft (frontRight & moveLeft width),
        size
      )
    where
      (Size (V2 width _)) = size

instance Rect2To (FrontRight, Size) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( FrontRight (frontLeft & moveRight width),
      size
    )
    where
      (Size (V2 width _)) = size

instance Rect2From (BackRight, Size) where
  rect2From (BackRight backRight, size) =
    Rect2D
      ( FrontLeft (backRight & moveLeft width & moveFront depth),
        size
      )
    where
      (Size (V2 width depth)) = size

instance Rect2To (BackRight, Size) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( BackRight (frontLeft & moveRight width & moveBack depth),
      size
    )
    where
      (Size (V2 width depth)) = size

instance Rect2From (BackLeft, Size) where
  rect2From (BackLeft backLeft, size) =
    Rect2D
      ( FrontLeft (backLeft & moveFront depth),
        size
      )
    where
      (Size (V2 _ depth)) = size

instance Rect2To (BackLeft, Size) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( BackLeft (frontLeft & moveBack depth),
      size
    )
    where
      (Size (V2 _ depth)) = size

instance Rect2From (Center, Size) where
  rect2From (Center center, size) =
    Rect2D
      ( FrontLeft (center & moveLeft halfWidth & moveFront halfDepth),
        size
      )
    where
      (Size (V2 width depth)) = size
      halfWidth = scale (toAbsFactor $ toAbs @Double 0.5) width
      halfDepth = scale (toAbsFactor $ toAbs @Double 0.5) depth

instance Rect2To (Center, Size) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( Center (frontLeft & moveRight halfWidth & moveBack halfDepth),
      size
    )
    where
      (Size (V2 width depth)) = size
      halfWidth = scale (toAbsFactor $ toAbs @Double 0.5) width
      halfDepth = scale (toAbsFactor $ toAbs @Double 0.5) depth

instance Rect2From (V2 Position, V2 Position) where
  rect2From (V2 x1 y1, V2 x2 y2) =
    rect2From
      ( FrontLeft $ V2 xFrontLeft yFrontLeft,
        Size $ V2 width depth
      )
    where
      xFrontLeft = min x1 x2
      yFrontLeft = min y1 y2
      xBackRight = max x1 x2
      yBackRight = max y1 y2
      width = getDistance xFrontLeft xBackRight
      depth = getDistance yFrontLeft yBackRight

-- fl = Position ()

instance Rect2To (Position, Position) where
  rect2To = undefined

instance Rect2To (FrontLeft, FrontRight, BackRight, BackLeft) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( FrontLeft frontLeft,
      FrontRight (frontLeft & moveRight width),
      BackRight (frontLeft & moveRight width & moveBack depth),
      BackLeft (frontLeft & moveBack depth)
    )
    where
      (Size (V2 width depth)) = size

instance Rect2To (FrontLeft, BackRight) where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( FrontLeft frontLeft,
      BackRight (frontLeft & moveRight width & moveBack depth)
    )
    where
      (Size (V2 width depth)) = size

instance Rect2To FrontLeft where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( FrontLeft frontLeft
    )

instance Rect2To BackLeft where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( BackLeft (frontLeft & moveBack depth)
    )
    where
      (Size (V2 _ depth)) = size

instance Rect2To BackRight where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( BackRight (frontLeft & moveRight width & moveBack depth)
    )
    where
      (Size (V2 width depth)) = size

instance Rect2To FrontRight where
  rect2To (Rect2D (FrontLeft frontLeft, size)) =
    ( FrontRight (frontLeft & moveRight width)
    )
    where
      (Size (V2 width _)) = size

instance Rect2To Size where
  rect2To (Rect2D (FrontLeft frontLeft, size)) = size

instance Rect2To Width where
  rect2To (Rect2D (_, Size (V2 w _))) = Width w

instance Rect2To Depth where
  rect2To (Rect2D (_, Size (V2 _ d))) = Depth d

-------------------------------------------------------------------------------

-- rect2From_FrontLeft_Size :: V2 Position -> V2 Length -> Rect2D
-- rect2From_FrontLeft_Size frontLeft size = Rect2D (frontLeft, size)

-- rect2To_FrontLeft_Size :: Rect2D -> (V2 Position, V2 Length)
-- rect2To_FrontLeft_Size (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_FrontRight_Size :: V2 Position -> V2 Length -> Rect2D
-- rect2From_FrontRight_Size frontRight size = undefined

-- rect2To_FrontRight_Size :: Rect2D -> (V2 Position, V2 Length)
-- rect2To_FrontRight_Size (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_BackRight_Size :: V2 Position -> V2 Length -> Rect2D
-- rect2From_BackRight_Size backRight size = undefined

-- rect2To_BackRight_Size :: Rect2D -> (V2 Position, V2 Length)
-- rect2To_BackRight_Size (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_BackLeft_Size :: V2 Position -> V2 Length -> Rect2D
-- rect2From_BackLeft_Size backLeft size = undefined

-- rect2To_BackLeft_Size :: Rect2D -> (V2 Position, V2 Length)
-- rect2To_BackLeft_Size (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_FrontLeft_BackRight :: V2 Position -> V2 Position -> V2 Length -> Rect2D
-- rect2From_FrontLeft_BackRight frontLeft backRight = undefined

-- rect2To_FrontLeft_BackRight :: Rect2D -> (V2 Position, V2 Position, V2 Length)
-- rect2To_FrontLeft_BackRight (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_FrontRight_BackLeft :: V2 Position -> V2 Position -> V2 Length -> Rect2D
-- rect2From_FrontRight_BackLeft frontRight backLeft = undefined

-- rect2To_FrontRight_BackLeft :: Rect2D -> (V2 Position, V2 Position, V2 Length)
-- rect2To_FrontRight_BackLeft (Rect2D (frontLeft, size)) = (frontLeft, size)

-- -------------------------------------------------------------------------------

-- rect2From_Center_Size :: V2 Position -> V2 Length -> Rect2D
-- rect2From_Center_Size center size = undefined

-- rect2To_Center_Size :: Rect2D -> (V2 Position, V2 Length)
-- rect2To_Center_Size (Rect2D (frontLeft, size)) = (frontLeft, size)

-------------------------------------------------------------------------------

-- instance FromTo Rect2D (FrontLeft, BackRight) where
--   to = undefined
--   from = undefined

-- instance FromTo (FrontLeft, Size) Rect2D where
--   to (Rect2D (frontLeft, size)) = (FrontLeft frontLeft, Size size)
--   from ((FrontLeft frontLeft), Size s) = Rect2D (frontLeft, s)

-- instance FromTo (BackRight, Size) Rect2D where
--   to = undefined
--   from = undefined

-- instance FromTo (Center, Size) Rect2D where
--   to = undefined
--   from = undefined
