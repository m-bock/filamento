module Filamento.TypeOps
  ( Delta,
    Position,
    Delta3D,
    Distance,
    Duration,
    Frequency,
    freqBeepLow,
    freqBeepMid,
    freqBeepHigh,
    Position2D,
    Position3D,
    Proportion,
    propMin,
    propMax,
    Speed,
    Temperature,
    deltaFromMm,
    delta3fromMm,
    pos2fromMm,
    pos3fromMm,
    delta3fromMmVec,
    delta3FromDelta,
    pos2fromMmVec,
    pos3fromMmVec,
    pos2FromPos,
    pos2From3,
    delta3From2,
    v2DeltaFrom3,
    countInc,
    v2DeltaToV3,
    v2DeltaFromMm,
    OutOf,
    Count,
    Total,
    outOfToProportion,
    outOfToFraction,
    posToMm,
    Rect2D,
    Square2D,
    Line2D,
    rect2FromCorners,
    rect2FromMinSize,
    rect2ToCenterSize,
    rect2FromCenterSize,
    square2FromCenterSize,
    square2FromMinSize,
    square2ToRect2,
    line2FromPoints,
    line2GetStart,
    line2GetEnd,
    square2GetMinCorner,
    square2GetMaxCorner,
    square2GetSize,
    rect2GetMinCorner,
    rect2GetMaxCorner,
    rect2GetSize,
    rect2GetCenter,
    posFromDelta,
    deltaFromPos,
    posFromMm,
    pos2ToVec,
    deltaFloor,
    deltaRound,
    rect2GetPoints,
    squareGetLines,
    line2FromPointsDeprec,
    angleSin,
    angleCos,
    angleCircle,
    angleFromProportion,
    module Export,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Filamento.Classes
import Filamento.Types as Export
import Linear
import qualified Linear as Lin
import Relude

deltaFromMm :: Double -> Delta
deltaFromMm = fromMm

deltaFloor :: Delta -> Count
deltaFloor = fromInt . floor . toMm

deltaRound :: Delta -> Count
deltaRound = fromInt . round . toMm

deltaFromPos :: Position -> Delta
deltaFromPos pos = getDelta (posFromMm 0) pos

-------------------------------------------------------------------------------

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating, Generic)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving (ToJSON, FromJSON) via Double

instance Millimeters Double Position where
  toMm (Position v) = v
  fromMm v = Position v

instance DeltaApplication Position Delta where
  addDelta (Position p) d = Position (p + toMm d)
  subDelta (Position p) d = Position (p - toMm d)

instance GetDelta Position Delta where
  getDelta (Position x) (Position y) = fromMm (y - x)

posToMm :: Position -> Double
posToMm (Position x) = x

posFromMm :: Double -> Position
posFromMm x = Position x

posFromDelta :: Delta -> Position
posFromDelta d = addDelta (posFromMm 0) d

-------------------------------------------------------------------------------

v2DeltaFromMm :: V2 Double -> V2 Delta
v2DeltaFromMm (V2 x y) = V2 (fromMm x) (fromMm y)

v2DeltaToV3 :: V2 Delta -> Delta -> V3 Double
v2DeltaToV3 (V2 x y) z = V3 (toMm x) (toMm y) (toMm z)

-------------------------------------------------------------------------------

newtype Delta3D = Delta3D (V3 Double)
  deriving (Show, Eq, Num)
  deriving (Semigroup, Monoid) via (Sum (V3 Double))

delta3FromDelta :: Delta -> Delta -> Delta -> Delta3D
delta3FromDelta x y z = Delta3D (V3 (toMm x) (toMm y) (toMm z))

delta3fromMm :: Double -> Double -> Double -> Delta3D
delta3fromMm x y z = Delta3D (V3 x y z)

delta3fromMmVec :: V3 Double -> Delta3D
delta3fromMmVec v = Delta3D v

delta3From2 :: V2 Delta -> Delta -> Delta3D
delta3From2 (V2 x y) z = Delta3D (V3 (toMm x) (toMm y) (toMm z))

v2DeltaFrom3 :: V3 Delta -> V2 Delta
v2DeltaFrom3 (V3 x y _) = V2 x y

instance Scalable Delta3D where
  scale factor (Delta3D v) = Delta3D (v * pure factor)

instance Millimeters (V3 Double) Delta3D where
  toMm (Delta3D v) = v
  fromMm v = Delta3D v

instance Millimeters3 Double Delta3D where
  toMm3 (Delta3D (V3 x y z)) = (x, y, z)
  fromMm3 x y z = Delta3D (V3 x y z)

instance JustX Delta3D where
  justX (Delta3D (V3 x _ _)) = Delta3D (V3 x 0 0)

instance JustY Delta3D where
  justY (Delta3D (V3 _ y _)) = Delta3D (V3 0 y 0)

instance JustZ Delta3D where
  justZ (Delta3D (V3 _ _ z)) = Delta3D (V3 0 0 z)

-------------------------------------------------------------------------------

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

instance Milliseconds Double Duration where
  toMs (Duration d) = d
  fromMs d = Duration d

instance Seconds Double Duration where
  toSecs (Duration d) = d / factorSecs
  fromSecs s = Duration (s * factorSecs)

factorSecs :: Double
factorSecs = 1000

-------------------------------------------------------------------------------

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

instance Hertz Double Frequency where
  toHz (Frequency f) = f
  fromHz f = Frequency f

freqBeepLow :: Frequency
freqBeepLow = fromHz 440 -- A4, common reference pitch

freqBeepMid :: Frequency
freqBeepMid = fromHz 880 -- A5, clear but not harsh

freqBeepHigh :: Frequency
freqBeepHigh = fromHz 1320 -- E6, attention-grabbing but not painful

-------------------------------------------------------------------------------

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)
  deriving (Semigroup, Monoid) via (Sum (V2 Double))

pos2fromMm :: Double -> Double -> Position2D
pos2fromMm x y = Position2D (V2 x y)

pos2fromMmVec :: V2 Double -> Position2D
pos2fromMmVec v = Position2D v

pos2From3 :: Position3D -> Position2D
pos2From3 (Position3D (V3 x y _)) = Position2D (V2 x y)

pos2ToVec :: Position2D -> V2 Position
pos2ToVec (Position2D (V2 x y)) = V2 (posFromMm x) (posFromMm y)

instance Millimeters (V2 Double) Position2D where
  toMm (Position2D v) = v
  fromMm v = Position2D v

instance Millimeters2 Double Position2D where
  toMm2 (Position2D (V2 x y)) = (x, y)
  fromMm2 x y = Position2D (V2 x y)

instance DeltaApplication Position2D (V2 Delta) where
  addDelta pos disp = fromMm (toMm pos + toMm disp)
  subDelta pos disp = fromMm (toMm pos - toMm disp)

instance JustX Position2D where
  justX (Position2D (V2 x _)) = Position2D (V2 x 0)

instance JustY Position2D where
  justY (Position2D (V2 _ y)) = Position2D (V2 0 y)

instance GetDelta Position2D (V2 Delta) where
  getDelta (Position2D (V2 x1 y1)) (Position2D (V2 x2 y2)) =
    V2 (fromMm (x2 - x1)) (fromMm (y2 - y1))

instance Distance Delta Position2D where
  getDistance (Position2D (V2 x1 y1)) (Position2D (V2 x2 y2)) =
    fromMm (Lin.distance (V2 x1 y1) (V2 x2 y2))

-------------------------------------------------------------------------------

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)
  deriving (Semigroup, Monoid) via (Sum (V3 Double))

pos3fromMm :: Double -> Double -> Double -> Position3D
pos3fromMm x y z = Position3D (V3 x y z)

pos3fromMmVec :: V3 Double -> Position3D
pos3fromMmVec v = Position3D v

pos2FromPos :: Position -> Position -> Position2D
pos2FromPos (Position x) (Position y) = pos2fromMm x y

instance Millimeters (V3 Double) Position3D where
  toMm (Position3D v) = v
  fromMm v = Position3D v

instance Millimeters3 Double Position3D where
  toMm3 (Position3D (V3 x y z)) = (x, y, z)
  fromMm3 x y z = Position3D (V3 x y z)

instance DeltaApplication Position3D Delta3D where
  addDelta pos disp = fromMm (toMm pos + toMm disp)
  subDelta pos disp = fromMm (toMm pos - toMm disp)

instance Distance Delta Position3D where
  getDistance (Position3D (V3 x1 y1 z1)) (Position3D (V3 x2 y2 z2)) =
    fromMm (Lin.distance (V3 x1 y1 z1) (V3 x2 y2 z2))

-------------------------------------------------------------------------------

newtype Proportion = Proportion Double
  deriving (Show, Eq, Ord)

instance FractionalValue Proportion where
  fromFraction f = Proportion f
  toFraction (Proportion f) = f
  clampFraction f = Proportion (clamp 0 1 f)

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal x = max minVal (min maxVal x)

propMin :: Proportion
propMin = Proportion 0

propMax :: Proportion
propMax = Proportion 1

-------------------------------------------------------------------------------

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

instance MillimetersPerSecond Double Speed where
  toMmPerSec (Speed s) = s
  fromMmPerSec d = Speed d

instance MillimetersPerMinute Double Speed where
  toMmPerMin (Speed s) = s * 60
  fromMmPerMin d = Speed (d / 60)

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

instance Celsius Double Temperature where
  toCelsius (Temperature t) = t
  fromCelsius t = Temperature t

-------------------------------------------------------------------------------

data OutOf = OutOf Count Total
  deriving (Show, Eq)

instance FromToInt (Int, Int) OutOf where
  fromInt (x, y) = OutOf (fromInt x) (fromInt y)
  toInt (OutOf count total) = (toInt count, toInt total)

instance FromToNatural (Natural, Natural) OutOf where
  fromNat (x, y) = OutOf (fromNat x) (fromNat y)
  toNat (OutOf count total) = (toNat count, toNat total)

outOfToProportion :: OutOf -> Proportion
outOfToProportion (OutOf count total) = fromFraction (toDouble count / toDouble total)

outOfToFraction :: OutOf -> Double
outOfToFraction (OutOf count total) = toDouble count / toDouble total

-------------------------------------------------------------------------------

newtype Count = Count Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

countInc :: Count -> Count
countInc (Count x) = Count (x + 1)

instance FromToNatural Natural Count where
  fromNat x = Count x
  toNat (Count x) = x

instance FromToInt Int Count where
  fromInt x = Count (max 0 $ fromIntegral x)
  toInt (Count x) = fromIntegral x

instance FromToDouble Double Count where
  fromDouble x = Count (max 0 $ floor x)
  toDouble (Count x) = fromIntegral x

-------------------------------------------------------------------------------

newtype Total = Total Natural
  deriving (Show, Eq)

instance FromToNatural Natural Total where
  fromNat x = Total x
  toNat (Total x) = x

instance FromToInt Int Total where
  fromInt x = Total (max 0 $ fromIntegral x)
  toInt (Total x) = fromIntegral x

instance FromToDouble Double Total where
  fromDouble x = Total (max 0 $ floor x)
  toDouble (Total x) = fromIntegral x

-------------------------------------------------------------------------------

data Rect2D = Rect2D {minCorner :: Position2D, size :: V2 Delta}
  deriving (Show, Eq)

rect2FromCorners :: Position2D -> Position2D -> Rect2D
rect2FromCorners minCorner maxCorner =
  Rect2D
    { minCorner,
      size = getDelta maxCorner minCorner
    }

rect2FromMinSize :: Position2D -> V2 Delta -> Rect2D
rect2FromMinSize minCorner size = Rect2D minCorner size

rect2FromCenterSize :: Position2D -> V2 Delta -> Rect2D
rect2FromCenterSize center size =
  Rect2D
    { minCorner = subDelta center size',
      size = size
    }
  where
    size' :: V2 Delta
    size' = scale 0.5 size

rect2ToCenterSize :: Rect2D -> (Position2D, V2 Delta)
rect2ToCenterSize rect = (rect2GetCenter rect, rect2GetSize rect)

rect2GetMinCorner :: Rect2D -> Position2D
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetMaxCorner :: Rect2D -> Position2D
rect2GetMaxCorner (Rect2D {minCorner, size}) = addDelta minCorner size

rect2GetSize :: Rect2D -> V2 Delta
rect2GetSize (Rect2D {size}) = size

rect2GetCenter :: Rect2D -> Position2D
rect2GetCenter (Rect2D {minCorner, size}) = addDelta minCorner (scale 0.5 size)

rect2GetPoints :: Rect2D -> (V2 Position, V2 Position, V2 Position, V2 Position)
rect2GetPoints (Rect2D {minCorner, size}) = (p1, p2, p3, p4)
  where
    p1 = pos2ToVec minCorner
    p2 = addDelta p1 (justX size)
    p3 = addDelta p2 (justY size)
    p4 = subDelta p3 (justX size)

-------------------------------------------------------------------------------

data Square2D = Square2D (Rect2D)
  deriving (Show, Eq)

square2FromCenterSize :: Position2D -> Delta -> Square2D
square2FromCenterSize center size = Square2D (rect2FromCenterSize center size')
  where
    size' :: V2 Delta
    size' = V2 size size

square2FromMinSize :: Position2D -> Delta -> Square2D
square2FromMinSize minCorner size = Square2D (rect2FromMinSize minCorner size')
  where
    size' :: V2 Delta
    size' = V2 size size

square2ToRect2 :: Square2D -> Rect2D
square2ToRect2 (Square2D rect) = rect

square2GetMinCorner :: Square2D -> Position2D
square2GetMinCorner (Square2D rect) = rect2GetMinCorner rect

square2GetMaxCorner :: Square2D -> Position2D
square2GetMaxCorner (Square2D rect) = rect2GetMaxCorner rect

square2GetSize :: Square2D -> V2 Delta
square2GetSize (Square2D rect) = rect2GetSize rect

squareGetLines :: Square2D -> (Line2D, Line2D, Line2D, Line2D)
squareGetLines (Square2D rect) = (line1, line2, line3, line4)
  where
    (p1, p2, p3, p4) = rect2GetPoints rect
    line1 = line2FromPoints p1 p2
    line2 = line2FromPoints p2 p3
    line3 = line2FromPoints p3 p4
    line4 = line2FromPoints p4 p1

-------------------------------------------------------------------------------

data Line2D = Line2D {start :: V2 Position, end :: V2 Position}
  deriving (Show, Eq)

line2FromPoints :: V2 Position -> V2 Position -> Line2D
line2FromPoints p1 p2 = Line2D {start = p1, end = p2}

line2FromPointsDeprec :: Position2D -> Position2D -> Line2D
line2FromPointsDeprec p1 p2 = Line2D {start = pos2ToVec p1, end = pos2ToVec p2}

line2GetStart :: Line2D -> V2 Position
line2GetStart (Line2D {start}) = start

line2GetEnd :: Line2D -> V2 Position
line2GetEnd (Line2D {end}) = end

-------------------------------------------------------------------------------

instance Millimeters (V2 Double) (V2 Position) where
  toMm (V2 x y) = V2 (toMm x) (toMm y)
  fromMm (V2 x y) = V2 (fromMm x) (fromMm y)

instance Millimeters (V3 Double) (V3 Position) where
  toMm (V3 x y z) = V3 (toMm x) (toMm y) (toMm z)
  fromMm (V3 x y z) = V3 (fromMm x) (fromMm y) (fromMm z)

instance GetDelta (V2 Position) (V2 Delta) where
  getDelta (V2 x y) (V2 x' y') = V2 (getDelta x x') (getDelta y y')

instance JustX (V2 Position) where
  justX (V2 x _) = V2 x 0

instance JustY (V2 Position) where
  justY (V2 _ y) = V2 0 y

instance DeltaApplication (V2 Position) (V2 Delta) where
  addDelta (V2 x y) (V2 dx dy) = V2 (addDelta x dx) (addDelta y dy)
  subDelta (V2 x y) (V2 dx dy) = V2 (subDelta x dx) (subDelta y dy)

instance Distance Delta (V2 Position) where
  getDistance pos1 pos2 = fromMm (Lin.distance (toMm pos1) (toMm pos2))

-------------------------------------------------------------------------------

instance IsOld Position2D (V2 Position) where
  fromOld (Position2D (V2 x y)) = V2 (posFromMm x) (posFromMm y)
  toOld (V2 x y) = Position2D (V2 (posToMm x) (posToMm y))

instance IsOld Position3D (V3 Position) where
  fromOld (Position3D (V3 x y z)) = V3 (posFromMm x) (posFromMm y) (posFromMm z)
  toOld (V3 x y z) = Position3D (V3 (posToMm x) (posToMm y) (posToMm z))

instance IsOld Delta3D (V3 Delta) where
  fromOld (Delta3D (V3 x y z)) = V3 (fromMm x) (fromMm y) (fromMm z)
  toOld (V3 x y z) = Delta3D (V3 (toMm x) (toMm y) (toMm z))

-------------------------------------------------------------------------------

angleSin :: Angle -> Position
angleSin (angleToRad -> a) = fromMm (sin a)

angleCos :: Angle -> Position
angleCos (angleToRad -> a) = fromMm (cos a)

angleCircle :: Angle -> V2 Position
angleCircle ang = V2 (angleCos ang) (angleSin ang)

angleFromProportion :: Proportion -> Angle
angleFromProportion (Proportion f) = angleFromRad (f * 2 * pi)