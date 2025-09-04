module Filamento.Types.Geometry.Rect2D
  ( Rect2D,
    rect2GetMinCorner,
    rect2GetSize,
    Rect2From (..),
    Rect2To (..),
    RectFrontLeft (..),
    RectFrontRight (..),
    RectBackRight (..),
    RectBackLeft (..),
    RectSize (..),
    RectWidth (..),
    RectDepth (..),
    RectCenter (..),
  )
where

import Filamento.Classes
import Filamento.Classes.Abs (FromToAbs (..))
import Filamento.Classes.Distance (Distance (..))
import Filamento.Classes.Move
import Filamento.Types.Contexts ()
import Filamento.Types.Continous.AbsFactor (FromToAbsFactor (..))
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

data Rect2D = Rect2D (RectFrontLeft, RectSize)
  deriving (Show, Eq)

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D (RectFrontLeft frontLeft, _)) = frontLeft

rect2GetSize :: Rect2D -> V2 Length
rect2GetSize (Rect2D (_, RectSize size)) = size

-------------------------------------------------------------------------------

newtype RectFrontLeft = RectFrontLeft (V2 Position)
  deriving (Show, Eq)

newtype RectFrontRight = RectFrontRight (V2 Position)
  deriving (Show, Eq)

newtype RectBackRight = RectBackRight (V2 Position)
  deriving (Show, Eq)

newtype RectBackLeft = RectBackLeft (V2 Position)
  deriving (Show, Eq)

newtype RectSize = RectSize (V2 Length)
  deriving (Show, Eq)

newtype RectWidth = RectWidth Length
  deriving (Show, Eq)

newtype RectDepth = RectDepth Length
  deriving (Show, Eq)

newtype RectCenter = RectCenter (V2 Position)
  deriving (Show, Eq)

-------------------------------------------------------------------------------

class Rect2From a where
  rect2From :: a -> Rect2D

class Rect2To a where
  rect2To :: Rect2D -> a

instance Rect2From (RectFrontLeft, RectSize) where
  rect2From = Rect2D

instance Rect2To (RectFrontLeft, RectSize) where
  rect2To (Rect2D (frontLeft, size)) =
    ( frontLeft,
      size
    )

instance Rect2From (RectFrontRight, RectSize) where
  rect2From (RectFrontRight frontRight, size) =
    Rect2D
      ( RectFrontLeft (frontRight & moveLeft width),
        size
      )
    where
      (RectSize (V2 width _)) = size

instance Rect2To (RectFrontRight, RectSize) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectFrontRight (frontLeft & moveRight width),
      size
    )
    where
      (RectSize (V2 width _)) = size

instance Rect2From (RectBackRight, RectSize) where
  rect2From (RectBackRight backRight, size) =
    Rect2D
      ( RectFrontLeft (backRight & moveLeft width & moveFront depth),
        size
      )
    where
      (RectSize (V2 width depth)) = size

instance Rect2To (RectBackRight, RectSize) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectBackRight (frontLeft & moveRight width & moveBack depth),
      size
    )
    where
      (RectSize (V2 width depth)) = size

instance Rect2From (RectBackLeft, RectSize) where
  rect2From (RectBackLeft backLeft, size) =
    Rect2D
      ( RectFrontLeft (backLeft & moveFront depth),
        size
      )
    where
      (RectSize (V2 _ depth)) = size

instance Rect2To (RectBackLeft, RectSize) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectBackLeft (frontLeft & moveBack depth),
      size
    )
    where
      (RectSize (V2 _ depth)) = size

instance Rect2From (RectCenter, RectSize) where
  rect2From (RectCenter center, size) =
    Rect2D
      ( RectFrontLeft (center & moveLeft halfWidth & moveFront halfDepth),
        size
      )
    where
      (RectSize (V2 width depth)) = size
      halfWidth = scale (toAbsFactor $ toAbs @Double 0.5) width
      halfDepth = scale (toAbsFactor $ toAbs @Double 0.5) depth

instance Rect2To (RectCenter, RectSize) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectCenter (frontLeft & moveRight halfWidth & moveBack halfDepth),
      size
    )
    where
      (RectSize (V2 width depth)) = size
      halfWidth = scale (toAbsFactor $ toAbs @Double 0.5) width
      halfDepth = scale (toAbsFactor $ toAbs @Double 0.5) depth

instance Rect2From (V2 Position, V2 Position) where
  rect2From (V2 x1 y1, V2 x2 y2) =
    rect2From
      ( RectFrontLeft $ V2 xFrontLeft yFrontLeft,
        RectSize $ V2 width depth
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

instance Rect2To (RectFrontLeft, RectFrontRight, RectBackRight, RectBackLeft) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectFrontLeft frontLeft,
      RectFrontRight (frontLeft & moveRight width),
      RectBackRight (frontLeft & moveRight width & moveBack depth),
      RectBackLeft (frontLeft & moveBack depth)
    )
    where
      (RectSize (V2 width depth)) = size

instance Rect2To (RectFrontLeft, RectBackRight) where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    ( RectFrontLeft frontLeft,
      RectBackRight (frontLeft & moveRight width & moveBack depth)
    )
    where
      (RectSize (V2 width depth)) = size

instance Rect2To RectFrontLeft where
  rect2To (Rect2D (RectFrontLeft frontLeft, _)) =
    RectFrontLeft frontLeft

instance Rect2To RectBackLeft where
  rect2To (Rect2D (RectFrontLeft frontLeft, RectSize (V2 _ depth))) =
    RectBackLeft (frontLeft & moveBack depth)

instance Rect2To RectBackRight where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    RectBackRight (frontLeft & moveRight width & moveBack depth)
    where
      (RectSize (V2 width depth)) = size

instance Rect2To RectFrontRight where
  rect2To (Rect2D (RectFrontLeft frontLeft, size)) =
    RectFrontRight (frontLeft & moveRight width)
    where
      (RectSize (V2 width _)) = size

instance Rect2To RectSize where
  rect2To (Rect2D (_, size)) = size

instance Rect2To RectWidth where
  rect2To (Rect2D (_, RectSize (V2 w _))) = RectWidth w

instance Rect2To RectDepth where
  rect2To (Rect2D (_, RectSize (V2 _ d))) = RectDepth d

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
