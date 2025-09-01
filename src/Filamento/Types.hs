module Filamento.Types
  ( Angle,
    angleFromRad,
    angleToRad,
    Position,
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
    volumeFromArea,
    square2GetMinCorner,
    square2GetSize,
    propFromDouble,
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
    circle2FromCenterRadius,
    circle2GetArea,
    outOfFromCountTotal,
    Range,
    rangeFromPos,
    propFromOutOf,
    rangeToDelta,
    rangeGetFrom,
    rangeGetTo,
    Area,
    areaFromVec2,
    Volume,
    volumeFromVec3,
    Circle2D,
    module Export,
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON))
import Filamento.Classes
import Filamento.Types.Continous.Factor as Export
import Filamento.Types.Continous.NonNegativeFactor as Export
import Filamento.Types.Quantities.Delta as Export
import Filamento.Types.Quantities.Duration as Export
import Filamento.Types.Quantities.Length as Export
import GHC.Generics
import Linear (V2 (..), V3 (..), distance)
import Relude

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

instance ToJSON Position where
  toJSON = genericToJSON defaultOptions

instance FromJSON Position where
  parseJSON = genericParseJSON defaultOptions

instance ToMillimeters Position where
  toMm (Position v) = v

instance FromMillimeters Position where
  fromMm v = Position v

instance JustX (V2 Position) where
  justX (V2 x _) = V2 x 0

instance JustY (V2 Position) where
  justY (V2 _ y) = V2 0 y

instance JustZ (V3 Position) where
  justZ (V3 _ _ z) = V3 0 0 z

instance Add Position Delta where
  add (Position p) d = Position (p + toMm d)

instance Sub Position Delta where
  sub (Position p) d = Position (p - toMm d)

instance Add (V2 Position) (V2 Delta) where
  add (V2 x y) (V2 x' y') = V2 (add x x') (add y y')

instance Sub (V2 Position) (V2 Delta) where
  sub (V2 x y) (V2 x' y') = V2 (sub x x') (sub y y')

instance Add (V3 Position) (V3 Delta) where
  add (V3 x y z) (V3 x' y' z') = V3 (add x x') (add y y') (add z z')

instance Sub (V3 Position) (V3 Delta) where
  sub (V3 x y z) (V3 x' y' z') = V3 (sub x x') (sub y y') (sub z z')

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

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

instance ToHertz Frequency where
  toHz (Frequency f) = f

instance FromHertz Frequency where
  fromHz f = Frequency f

-------------------------------------------------------------------------------

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

instance ToMillimetersPerSecond Speed where
  toMmPerSec (Speed s) = s

instance FromMillimetersPerSecond Speed where
  fromMmPerSec d = Speed d

instance ToMillimetersPerMinute Speed where
  toMmPerMin (Speed s) = s * 60

instance FromMillimetersPerMinute Speed where
  fromMmPerMin d = Speed (d / 60)

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

instance ToCelsius Temperature where
  toCelsius (Temperature t) = t

instance FromCelsius Temperature where
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

instance FromNatural Total where
  fromNat x = Total x

instance ToNatural Total where
  toNat (Total x) = x

instance ToDouble Total where
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

-------------------------------------------------------------------------------

newtype Count = Count Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

instance FromNatural Count where
  fromNat x = Count x

instance ToNatural Count where
  toNat (Count x) = x

instance ToDouble Count where
  toDouble (Count x) = fromIntegral x

countInc :: Count -> Count
countInc (Count x) = Count (x + 1)

-------------------------------------------------------------------------------

newtype Proportion = Proportion Double
  deriving (Show, Eq, Ord)

instance ToDouble Proportion where
  toDouble (Proportion f) = f

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal x = max minVal (min maxVal x)

propFromDouble :: Double -> Maybe Proportion
propFromDouble f = if f >= 0 && f <= 1 then Just (Proportion f) else Nothing

propFromOutOf :: OutOf -> Proportion
propFromOutOf outOF = Proportion (toDouble count / toDouble total)
  where
    count = outOfGetCount outOF
    total = outOfGetTotal outOF

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

-------------------------------------------------------------------------------

newtype Area = Area {sqMm :: Double}

areaFromVec2 :: V2 Length -> Area
areaFromVec2 (V2 x y) = Area (toMm x * toMm y)

instance FromSquareMillimeters Area where
  fromSqMm x = Area x

instance ToSquareMillimeters Area where
  toSqMm (Area x) = x

-------------------------------------------------------------------------------

newtype Volume = Volume {cuMm :: Double}

volumeFromVec3 :: V3 Length -> Volume
volumeFromVec3 (V3 x y z) = Volume (toMm x * toMm y * toMm z)

instance FromCubicMillimeters Volume where
  fromCuMm x = Volume x

instance ToCubicMillimeters Volume where
  toCuMm (Volume x) = x

volumeFromArea :: Area -> Length -> Volume
volumeFromArea (Area a) d = Volume (a * toMm d)

-------------------------------------------------------------------------------

data Circle2D = Circle2D {center :: V2 Position, radius :: Length}

circle2GetArea :: Circle2D -> Area
circle2GetArea (Circle2D {radius}) = fromSqMm $ pi * (toMm radius ^ 2)

circle2FromCenterRadius :: V2 Position -> Length -> Circle2D
circle2FromCenterRadius center radius = Circle2D {center, radius}
