module Filamento.Types
  ( Angle,
    angleFromRad,
    angleToRad,
    Position,
    Frequency,
    Rect2D,
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
    outOfFromCountTotal,
    Range,
    rangeFromPos,
    propFromOutOf,
    rangeToDelta,
    rangeGetFrom,
    rangeGetTo,
    Area,
    areaFromVec2,
    module Export,
  )
where

import Filamento.Classes
import Filamento.Types.Continous.Factor as Export
import Filamento.Types.Continous.NonNegativeFactor as Export
import Filamento.Types.Geometry as Export
import Filamento.Types.Quantities as Export
import GHC.Generics
import Linear (V2 (..), V3 (..))
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
