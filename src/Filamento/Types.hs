module Filamento.Types
  ( Delta,
    Position,
    Delta2D,
    Delta3D,
    Distance,
    Duration,
    Frequency,
    freqBeepLow,
    freqBeepMid,
    freqBeepHigh,
    Position2D,
    Position3D,
    pos3Distance,
    Proportion,
    propMin,
    propMax,
    Speed,
    Temperature,
    delta2fromMm,
    deltaFromMm,
    delta3fromMm,
    pos2fromMm,
    pos3fromMm,
    delta2fromMmVec,
    delta3fromMmVec,
    delta3FromDelta,
    pos2fromMmVec,
    pos3fromMmVec,
    pos2FromPos,
    pos2From3,
    delta3From2,
    delta2From3,
    delta2To3,
    delta2FromDelta,
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
    posFromMm,
    pos2ToVec,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Filamento.Classes
import Linear
import qualified Linear as Lin
import Relude

newtype Delta = Delta Double
  deriving (Show, Eq, Generic, Num, Ord, Fractional, RealFrac, Real)
  deriving (Semigroup, Monoid) via (Sum Double)

deltaFromMm :: Double -> Delta
deltaFromMm v = Delta v

instance ToJSON Delta

instance FromJSON Delta

instance Scalable Delta where
  scale factor (Delta v) = Delta (v * factor)

instance Millimeters Double Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

-------------------------------------------------------------------------------

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating, Generic)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving (ToJSON, FromJSON) via Double

instance Millimeters Double Position where
  toMm (Position v) = v
  fromMm v = Position v

instance DeltaApplication Position Delta where
  addDelta (Position p) (Delta d) = Position (p + d)
  subDelta (Position p) (Delta d) = Position (p - d)

instance SignedDistance Position Delta where
  signedDistance (Position x) (Position y) = Delta (y - x)

posToMm :: Position -> Double
posToMm (Position x) = x

posFromMm :: Double -> Position
posFromMm x = Position x

-------------------------------------------------------------------------------

newtype Delta2D = Delta2D (V2 Double)
  deriving (Show, Eq, Num)
  deriving (Semigroup, Monoid) via (Sum (V2 Double))

delta2FromDelta :: Delta -> Delta -> Delta2D
delta2FromDelta (Delta x) (Delta y) = Delta2D (V2 x y)

delta2fromMm :: Double -> Double -> Delta2D
delta2fromMm x y = Delta2D (V2 x y)

delta2fromMmVec :: V2 Double -> Delta2D
delta2fromMmVec v = Delta2D v

delta2To3 :: Delta2D -> Delta -> Delta3D
delta2To3 (Delta2D (V2 x y)) (Delta z) = Delta3D (V3 x y z)

instance Scalable Delta2D where
  scale factor (Delta2D v) = Delta2D (v * pure factor)

instance Millimeters (V2 Double) Delta2D where
  toMm (Delta2D v) = v
  fromMm v = Delta2D v

instance Millimeters2 Double Delta2D where
  toMm2 (Delta2D (V2 x y)) = (x, y)
  fromMm2 x y = Delta2D (V2 x y)

instance JustX Delta2D where
  justX (Delta2D (V2 x _)) = Delta2D (V2 x 0)

instance JustY Delta2D where
  justY (Delta2D (V2 _ y)) = Delta2D (V2 0 y)

-------------------------------------------------------------------------------

newtype Delta3D = Delta3D (V3 Double)
  deriving (Show, Eq, Num)
  deriving (Semigroup, Monoid) via (Sum (V3 Double))

delta3FromDelta :: Delta -> Delta -> Delta -> Delta3D
delta3FromDelta (Delta x) (Delta y) (Delta z) = Delta3D (V3 x y z)

delta3fromMm :: Double -> Double -> Double -> Delta3D
delta3fromMm x y z = Delta3D (V3 x y z)

delta3fromMmVec :: V3 Double -> Delta3D
delta3fromMmVec v = Delta3D v

delta3ToMmVec :: Delta3D -> V3 Double
delta3ToMmVec (Delta3D v) = v

delta3ToMm :: Delta3D -> (Double, Double, Double)
delta3ToMm (Delta3D (V3 x y z)) = (x, y, z)

delta3From2 :: Delta2D -> Delta -> Delta3D
delta3From2 (Delta2D (V2 x y)) (Delta z) = Delta3D (V3 x y z)

delta2From3 :: Delta3D -> Delta2D
delta2From3 (Delta3D (V3 x y _)) = Delta2D (V2 x y)

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

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

instance Millimeters Double Distance where
  toMm (Distance d) = d
  fromMm d = Distance d

instance Scalable Distance where
  scale factor (Distance d) = Distance (d * factor)

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

instance DeltaApplication Position2D Delta2D where
  addDelta pos disp = fromMm (toMm pos + toMm disp)
  subDelta pos disp = fromMm (toMm pos - toMm disp)

instance JustX Position2D where
  justX (Position2D (V2 x _)) = Position2D (V2 x 0)

instance JustY Position2D where
  justY (Position2D (V2 _ y)) = Position2D (V2 0 y)

instance SignedDistance Position2D Delta2D where
  signedDistance (Position2D (V2 x1 y1)) (Position2D (V2 x2 y2)) =
    delta2fromMmVec (V2 (x2 - x1) (y2 - y1))

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

-- Distance calculation between two 3D positions
pos3Distance :: Position3D -> Position3D -> Distance
pos3Distance (Position3D v1) (Position3D v2) = fromMm (Lin.distance v1 v2)

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
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via (Sum Natural)

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

data Rect2D = Rect2D {minCorner :: Position2D, size :: Delta2D}
  deriving (Show, Eq)

rect2FromCorners :: Position2D -> Position2D -> Rect2D
rect2FromCorners minCorner maxCorner =
  Rect2D
    { minCorner,
      size = signedDistance maxCorner minCorner
    }

rect2FromMinSize :: Position2D -> Delta2D -> Rect2D
rect2FromMinSize minCorner size = Rect2D minCorner size

rect2FromCenterSize :: Position2D -> Delta2D -> Rect2D
rect2FromCenterSize center size =
  Rect2D
    { minCorner = subDelta center size',
      size = size
    }
  where
    size' :: Delta2D
    size' = scale 0.5 size

rect2GetMinCorner :: Rect2D -> Position2D
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetMaxCorner :: Rect2D -> Position2D
rect2GetMaxCorner (Rect2D {minCorner, size}) = addDelta minCorner size

rect2GetSize :: Rect2D -> Delta2D
rect2GetSize (Rect2D {minCorner, size}) = size

rect2GetCenter :: Rect2D -> Position2D
rect2GetCenter (Rect2D {minCorner, size}) = addDelta minCorner (scale 0.5 size)

-------------------------------------------------------------------------------

data Square2D = Square2D (Rect2D)
  deriving (Show, Eq)

square2FromCenterSize :: Position2D -> Delta -> Square2D
square2FromCenterSize center size = Square2D (rect2FromCenterSize center size')
  where
    size' :: Delta2D
    size' = delta2FromDelta size size

square2FromMinSize :: Position2D -> Delta -> Square2D
square2FromMinSize minCorner size = Square2D (rect2FromMinSize minCorner size')
  where
    size' :: Delta2D
    size' = delta2FromDelta size size

square2ToRect2 :: Square2D -> Rect2D
square2ToRect2 (Square2D rect) = rect

square2GetMinCorner :: Square2D -> Position2D
square2GetMinCorner (Square2D rect) = rect2GetMinCorner rect

square2GetMaxCorner :: Square2D -> Position2D
square2GetMaxCorner (Square2D rect) = rect2GetMaxCorner rect

square2GetSize :: Square2D -> Delta2D
square2GetSize (Square2D rect) = rect2GetSize rect

-------------------------------------------------------------------------------

data Line2D = Line2D {start :: Position2D, end :: Position2D}
  deriving (Show, Eq)

line2FromPoints :: Position2D -> Position2D -> Line2D
line2FromPoints p1 p2 = Line2D {start = p1, end = p2}

line2GetStart :: Line2D -> Position2D
line2GetStart (Line2D {start}) = start

line2GetEnd :: Line2D -> Position2D
line2GetEnd (Line2D {end}) = end