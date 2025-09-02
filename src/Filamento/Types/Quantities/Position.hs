module Filamento.Types.Quantities.Position (Position, positionPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Classes.Distance
import Filamento.Types.Continous.Factor (Factor)
import Filamento.Types.Continous.NonNegativeFactor (NonNegativeFactor)
import Filamento.Types.Quantities.Delta (Delta)
import Fmt
import GHC.Generics
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

instance Distance Position where
  getDistance (Position x) (Position y) = unsafeFromMm $ abs (y - x)
