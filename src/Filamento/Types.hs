module Filamento.Types
  ( Angle,
    angleFromRad,
    angleToRad,
    Delta,
    Position,
    Duration,
    Frequency,
    Rect2D,
    rect2FromMinSize,
    rect2GetMinCorner,
    rect2GetSize,
    Line2D,
    line2FromPoints,
    line2GetStart,
    line2GetEnd,
    Square2D,
    square2FromMinSize,
    square2GetMinCorner,
    square2GetSize,
    OutOf,
    Count,
    countInc,
    Total,
    Proportion,
    Temperature,
    Speed,
    propMin,
    propMax,
    outOfGetCount,
    outOfGetTotal,
    outOfFromCountTotal,
    Range,
    rangeFromPos,
    rangeToDelta,
    rangeGetFrom,
    rangeGetTo,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Filamento.Classes
import GHC.Generics
import Linear (V2 (..), V3 (..), distance)
import Relude

-------------------------------------------------------------------------------

newtype Delta = Delta Double
  deriving (Show, Eq, Generic, Num, Ord, Fractional, RealFrac, Real)
  deriving (Semigroup, Monoid) via (Sum Double)

instance ToJSON Delta

instance FromJSON Delta

instance Scalable Double Delta where
  scale factor (Delta v) = Delta (v * factor)

instance FromToMillimeters Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

instance JustX (V2 Delta) where
  justX (V2 x _) = V2 x 0

instance JustX (V3 Delta) where
  justX (V3 x _ _) = V3 x 0 0

instance JustY (V2 Delta) where
  justY (V2 _ y) = V2 0 y

instance JustY (V3 Delta) where
  justY (V3 _ y _) = V3 0 y 0

instance JustZ (V3 Delta) where
  justZ (V3 _ _ z) = V3 0 0 z

instance Scalable Double (V2 Delta) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable Double (V3 Delta) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

instance GetDelta Delta Delta where
  getDelta (Delta x) (Delta y) = Delta (y - x)

instance DeltaApplication Delta Delta where
  addDelta (Delta x) (Delta y) = Delta (x + y)
  subDelta (Delta x) (Delta y) = Delta (x - y)

-------------------------------------------------------------------------------

newtype Angle = Angle {rad :: Double}
  deriving (Show, Eq, Generic)

instance Scalable Double Angle where
  scale factor (Angle a) = Angle (a * factor)

angleFromRad :: Double -> Angle
angleFromRad = Angle

angleToRad :: Angle -> Double
angleToRad (Angle a) = a

-------------------------------------------------------------------------------

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating, Generic)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving (ToJSON, FromJSON) via Double

instance FromToMillimeters Position where
  toMm (Position v) = v
  fromMm v = Position v

instance JustX (V2 Position) where
  justX (V2 x _) = V2 x 0

instance JustY (V2 Position) where
  justY (V2 _ y) = V2 0 y

instance JustZ (V3 Position) where
  justZ (V3 _ _ z) = V3 0 0 z

instance DeltaApplication Position Delta where
  addDelta (Position p) d = Position (p + toMm d)
  subDelta (Position p) d = Position (p - toMm d)

instance DeltaApplication (V2 Position) (V2 Delta) where
  addDelta (V2 x y) (V2 x' y') = V2 (addDelta x x') (addDelta y y')
  subDelta (V2 x y) (V2 x' y') = V2 (subDelta x x') (subDelta y y')

instance DeltaApplication (V3 Position) (V3 Delta) where
  addDelta (V3 x y z) (V3 x' y' z') = V3 (addDelta x x') (addDelta y y') (addDelta z z')
  subDelta (V3 x y z) (V3 x' y' z') = V3 (subDelta x x') (subDelta y y') (subDelta z z')

instance GetDelta Position Delta where
  getDelta (Position x) (Position y) = fromMm (y - x)

instance GetDelta (V2 Position) (V2 Delta) where
  getDelta (V2 x1 y1) (V2 x2 y2) = V2 (getDelta x1 x2) (getDelta y1 y2)

instance GetDelta (V3 Position) (V3 Delta) where
  getDelta (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (getDelta x1 x2) (getDelta y1 y2) (getDelta z1 z2)

instance Distance Delta Position where
  getDistance (Position x) (Position y) = fromMm (y - x)

instance Distance Delta (V2 Position) where
  getDistance (V2 x1 y1) (V2 x2 y2) =
    fromMm
      $ distance (V2 (toMm x1) (toMm y1)) (V2 (toMm x2) (toMm y2))

instance Distance Delta (V3 Position) where
  getDistance (V3 x1 y1 z1) (V3 x2 y2 z2) =
    fromMm
      $ distance
        (V3 (toMm x1) (toMm y1) (toMm z1))
        (V3 (toMm x2) (toMm y2) (toMm z2))

instance Scalable Double Position where
  scale factor (Position p) = Position (p * factor)

instance Scalable Double (V2 Position) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable Double (V3 Position) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

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

data Rect2D = Rect2D {minCorner :: V2 Position, size :: V2 Delta}
  deriving (Show, Eq)

rect2FromMinSize :: V2 Position -> V2 Delta -> Rect2D
rect2FromMinSize minCorner size = Rect2D {minCorner, size}

rect2GetMinCorner :: Rect2D -> V2 Position
rect2GetMinCorner (Rect2D {minCorner}) = minCorner

rect2GetSize :: Rect2D -> V2 Delta
rect2GetSize (Rect2D {size}) = size

-------------------------------------------------------------------------------

data Line2D = Line2D {start :: V2 Position, end :: V2 Position}
  deriving (Show, Eq)

line2FromPoints :: V2 Position -> V2 Position -> Line2D
line2FromPoints p1 p2 = Line2D {start = p1, end = p2}

line2GetStart :: Line2D -> V2 Position
line2GetStart (Line2D {start}) = start

line2GetEnd :: Line2D -> V2 Position
line2GetEnd (Line2D {end}) = end

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

data OutOf = OutOf {count :: Count, total :: Total}
  deriving (Show, Eq)

outOfFromCountTotal :: Count -> Total -> OutOf
outOfFromCountTotal count total = OutOf {count, total}

outOfGetCount :: OutOf -> Count
outOfGetCount (OutOf {count}) = count

outOfGetTotal :: OutOf -> Total
outOfGetTotal (OutOf {total}) = total

instance FromToInt (Int, Int) OutOf where
  fromInt (x, y) = OutOf (fromInt x) (fromInt y)
  toInt (OutOf count total) = (toInt count, toInt total)

instance FromToNatural (Natural, Natural) OutOf where
  fromNat (x, y) = OutOf (fromNat x) (fromNat y)
  toNat (OutOf count total) = (toNat count, toNat total)

-------------------------------------------------------------------------------

newtype Count = Count Natural
  deriving (Show, Eq, Ord)
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

countInc :: Count -> Count
countInc (Count x) = Count (x + 1)

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

data Range = Range {from :: Position, to :: Position}
  deriving (Show, Eq, Ord)

rangeFromPos :: Position -> Position -> Range
rangeFromPos from to = Range {from, to}

rangeToDelta :: Range -> Delta
rangeToDelta (Range {from, to}) = getDelta from to

rangeGetFrom :: Range -> Position
rangeGetFrom (Range {from}) = from

rangeGetTo :: Range -> Position
rangeGetTo (Range {to}) = to