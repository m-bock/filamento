module Filamento.Types.Quantities.Position (Position, positionPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.Factor (Factor)
import Filamento.Types.Continous.NonNegativeFactor (NonNegativeFactor)
import Filamento.Types.Quantities.Delta (Delta)
import Filamento.Types.Quantities.Length (Length)
import Fmt
import GHC.Generics
import Linear (V2 (V2), V3 (V3))
import qualified Linear.Metric as L
import Relude

newtype Position = Position {mm :: Double} -- may be negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (Fractional, RealFrac, Real, Num)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

positionPretty :: Position -> Text
positionPretty (Position mm) = fixedF 2 mm |+ "mm"

instance ToMillimeters Position where
  toMm (Position v) = v

instance FromMillimeters Position where
  fromMm v = Position v

instance ToCentimeters Position where
  toCm (Position v) = v

instance FromCentimeters Position where
  fromCm v = Position v

instance Scalable Factor Position where
  scale factor (Position v) = Position (v * toDouble factor)

instance Scalable NonNegativeFactor Position where
  scale factor (Position v) = Position (v * toDouble factor)

instance Add Position Delta where
  add pos del = fromMm (toMm pos + toMm del)

instance Sub Position Delta where
  sub pos del = fromMm (toMm pos - toMm del)

instance GetDelta Position Delta where
  getDelta (Position x) (Position y) = fromMm (y - x)

instance Distance Length Position where
  getDistance (Position x) (Position y) = unsafeFromMm $ abs (y - x)

-- Vector instances

instance Scalable Factor (V2 Position) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable NonNegativeFactor (V2 Position) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable Factor (V3 Position) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

instance Scalable NonNegativeFactor (V3 Position) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

instance Distance Delta (V2 Position) where
  getDistance (V2 x1 y1) (V2 x2 y2) =
    fromMm $
      L.distance
        (V2 (toMm x1) (toMm y1))
        (V2 (toMm x2) (toMm y2))

instance Distance Delta (V3 Position) where
  getDistance (V3 x1 y1 z1) (V3 x2 y2 z2) =
    fromMm $
      L.distance
        (V3 (toMm x1) (toMm y1) (toMm z1))
        (V3 (toMm x2) (toMm y2) (toMm z2))
