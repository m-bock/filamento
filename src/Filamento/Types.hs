module Filamento.Types
  ( Delta,
    dltJustX,
    dltJustY,
    Position,
    posFromMm,
    posToMm,
    posAddDelta,
    posSubDelta,
    Delta2D,
    dlt2JustX,
    dlt2JustY,
    dlt2FromMm,
    dlt2ToMm,
    Delta3D,
    dlt3JustX,
    dlt3JustY,
    dlt3JustZ,
    dlt3FromMm,
    dlt3ToMm,
    Distance,
    distScale,
    distFromMm,
    distToMm,
    Duration,
    durFromMs,
    durToMs,
    durFromSecs,
    durToSecs,
    Frequency,
    freqFromHz,
    freqToHz,
    freqBeepLow,
    freqBeepMid,
    freqBeepHigh,
    Position2D,
    pos2AddDelta,
    pos2SubDelta,
    pos2FromMm,
    pos2ToMm,
    Position3D,
    pos3AddDelta,
    pos3SubDelta,
    pos3Distance,
    pos3FromMm,
    pos3ToMm,
    Proportion,
    propFromFractionClamped,
    propToFraction,
    propMin,
    propMax,
    Speed,
    spdFromMmPerMin,
    spdToMmPerMin,
    spdFromMmPerSec,
    spdToMmPerSec,
    Temperature,
    tempFromCelsius,
    tempToCelsius,
  )
where

import Filamento.Classes (Millimeters (..))
import Linear
import qualified Linear as Lin
import Relude

newtype Delta = Delta Double deriving (Show, Eq, Generic, Num, Ord)

instance Millimeters Double Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

-- | Keep only X component (identity for 1D)
dltJustX :: Delta -> Delta
dltJustX (Delta x) = Delta x

-- | Keep only Y component (identity for 1D)
dltJustY :: Delta -> Delta
dltJustY (Delta y) = Delta y

newtype Position = Position {mm :: Double}
  deriving (Show, Eq, Num, Fractional, Ord, Real, Enum, Floating)

posFromMm :: Double -> Position
posFromMm v = Position v

posToMm :: Position -> Double
posToMm (Position v) = v

posAddDelta :: Position -> Delta -> Position
posAddDelta pos disp = posFromMm (posToMm pos + toMm disp)

posSubDelta :: Position -> Delta -> Position
posSubDelta pos disp = posFromMm (posToMm pos - toMm disp)

newtype Delta2D = Delta2D (V2 Double)

dlt2FromMm :: V2 Double -> Delta2D
dlt2FromMm v = Delta2D v

dlt2ToMm :: Delta2D -> V2 Double
dlt2ToMm (Delta2D v) = v

dlt2JustX :: Delta2D -> Delta2D
dlt2JustX (Delta2D (V2 x _)) = Delta2D (V2 x 0)

dlt2JustY :: Delta2D -> Delta2D
dlt2JustY (Delta2D (V2 _ y)) = Delta2D (V2 0 y)

newtype Delta3D = Delta3D (V3 Double)

dlt3FromMm :: V3 Double -> Delta3D
dlt3FromMm v = Delta3D v

dlt3ToMm :: Delta3D -> V3 Double
dlt3ToMm (Delta3D v) = v

dlt3JustX :: Delta3D -> Delta3D
dlt3JustX (Delta3D (V3 x _ _)) = Delta3D (V3 x 0 0)

dlt3JustY :: Delta3D -> Delta3D
dlt3JustY (Delta3D (V3 _ y _)) = Delta3D (V3 0 y 0)

dlt3JustZ :: Delta3D -> Delta3D
dlt3JustZ (Delta3D (V3 _ _ z)) = Delta3D (V3 0 0 z)

newtype Distance = Distance {mm :: Double}
  deriving (Show, Eq, Generic, Num, Ord)

distToMm :: Distance -> Double
distToMm (Distance d) = d

distFromMm :: Double -> Distance
distFromMm d = Distance d

distScale :: Double -> Distance -> Distance
distScale factor (Distance d) = Distance (d * factor)

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

durFromMs :: Double -> Duration
durFromMs d = Duration d

durToMs :: Duration -> Double
durToMs (Duration d) = d

durToSecs :: Duration -> Double
durToSecs (Duration d) = d / factorSecs

durFromSecs :: Double -> Duration
durFromSecs s = Duration (s * factorSecs)

factorSecs :: Double
factorSecs = 1000

newtype Frequency = Frequency {hz :: Double}
  deriving (Show, Eq, Generic)

freqFromHz :: Double -> Frequency
freqFromHz f = Frequency f

freqToHz :: Frequency -> Double
freqToHz (Frequency f) = f

freqBeepLow :: Frequency
freqBeepLow = freqFromHz 440 -- A4, common reference pitch

freqBeepMid :: Frequency
freqBeepMid = freqFromHz 880 -- A5, clear but not harsh

freqBeepHigh :: Frequency
freqBeepHigh = freqFromHz 1320 -- E6, attention-grabbing but not painful

newtype Position2D = Position2D {mm :: V2 Double}
  deriving (Show, Eq, Num)

pos2AddDelta :: Position2D -> Delta2D -> Position2D
pos2AddDelta pos disp = pos2FromMm (pos2ToMm pos + dlt2ToMm disp)

pos2SubDelta :: Position2D -> Delta2D -> Position2D
pos2SubDelta pos disp = pos2FromMm (pos2ToMm pos - dlt2ToMm disp)

pos2FromMm :: V2 Double -> Position2D
pos2FromMm v = Position2D v

pos2ToMm :: Position2D -> V2 Double
pos2ToMm (Position2D v) = v

newtype Position3D = Position3D {mm :: V3 Double}
  deriving (Show, Eq, Num)

pos3AddDelta :: Position3D -> Delta3D -> Position3D
pos3AddDelta pos disp = pos3FromMm (pos3ToMm pos + dlt3ToMm disp)

pos3SubDelta :: Position3D -> Delta3D -> Position3D
pos3SubDelta pos disp = pos3FromMm (pos3ToMm pos - dlt3ToMm disp)

pos3FromMm :: V3 Double -> Position3D
pos3FromMm v = Position3D v

pos3ToMm :: Position3D -> V3 Double
pos3ToMm (Position3D v) = v

pos3Distance :: Position3D -> Position3D -> Distance
pos3Distance (Position3D v1) (Position3D v2) = distFromMm (Lin.distance v1 v2)

newtype Proportion = Proportion Double
  deriving (Show, Eq, Ord)

propFromFractionClamped :: Double -> Proportion
propFromFractionClamped f = Proportion (clamp 0 1 f)

clamp :: Double -> Double -> Double -> Double
clamp minVal maxVal x = max minVal (min maxVal x)

propToFraction :: Proportion -> Double
propToFraction (Proportion f) = f

propMin :: Proportion
propMin = Proportion 0

propMax :: Proportion
propMax = Proportion 1

newtype Speed = Speed {mmPerSec :: Double}
  deriving (Show, Eq, Ord)

spdFromMmPerMin :: Double -> Speed
spdFromMmPerMin d = Speed (d / 60)

spdToMmPerMin :: Speed -> Double
spdToMmPerMin (Speed s) = s * 60

spdFromMmPerSec :: Double -> Speed
spdFromMmPerSec d = Speed d

spdToMmPerSec :: Speed -> Double
spdToMmPerSec (Speed s) = s

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

tempFromCelsius :: Double -> Temperature
tempFromCelsius t = Temperature t

tempToCelsius :: Temperature -> Double
tempToCelsius (Temperature t) = t