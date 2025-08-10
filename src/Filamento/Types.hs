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
    delta3fromMm,
    pos2fromMm,
    pos3fromMm,
    delta2fromMmVec,
    delta3fromMmVec,
    pos2fromMmVec,
    pos3fromMmVec,
  )
where

import Filamento.Classes
import Linear
import qualified Linear as Lin
import Relude

newtype Delta = Delta Double deriving (Show, Eq, Generic, Num, Ord)

instance Millimeters Double Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating)

instance Millimeters Double Position where
  toMm (Position v) = v
  fromMm v = Position v

instance DeltaApplication Position Delta where
  addDelta (Position p) (Delta d) = Position (p + d)
  subDelta (Position p) (Delta d) = Position (p - d)

newtype Delta2D = Delta2D (V2 Double)

delta2fromMm :: Double -> Double -> Delta2D
delta2fromMm x y = Delta2D (V2 x y)

delta2fromMmVec :: V2 Double -> Delta2D
delta2fromMmVec v = Delta2D v

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

newtype Delta3D = Delta3D (V3 Double)

delta3fromMm :: Double -> Double -> Double -> Delta3D
delta3fromMm x y z = Delta3D (V3 x y z)

delta3fromMmVec :: V3 Double -> Delta3D
delta3fromMmVec v = Delta3D v

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

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

instance Millimeters Double Distance where
  toMm (Distance d) = d
  fromMm d = Distance d

instance Scalable Distance where
  scale factor (Distance d) = Distance (d * factor)

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

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

pos2fromMm :: Double -> Double -> Position2D
pos2fromMm x y = Position2D (V2 x y)

pos2fromMmVec :: V2 Double -> Position2D
pos2fromMmVec v = Position2D v

instance Millimeters (V2 Double) Position2D where
  toMm (Position2D v) = v
  fromMm v = Position2D v

instance Millimeters2 Double Position2D where
  toMm2 (Position2D (V2 x y)) = (x, y)
  fromMm2 x y = Position2D (V2 x y)

instance DeltaApplication Position2D Delta2D where
  addDelta pos disp = fromMm (toMm pos + toMm disp)
  subDelta pos disp = fromMm (toMm pos - toMm disp)

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)

pos3fromMm :: Double -> Double -> Double -> Position3D
pos3fromMm x y z = Position3D (V3 x y z)

pos3fromMmVec :: V3 Double -> Position3D
pos3fromMmVec v = Position3D v

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