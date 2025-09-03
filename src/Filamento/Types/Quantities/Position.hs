module Filamento.Types.Quantities.Position
  ( Position,
    positionPretty,
    PositionFromTo (..),
  )
where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Classes.Distance
import Filamento.Types.Continous.AbsFactor (AbsFactor)
import Filamento.Types.Continous.Factor (Factor)
import Filamento.Types.MeasureUnits
import Filamento.Types.Quantities.Delta (Delta)
import Filamento.Types.Quantities.Length (Length)
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

-------------------------------------------------------------------------------

class PositionFromTo a where
  posFrom :: a -> Position
  posTo :: Position -> a

instance PositionFromTo Millimeter where
  posFrom (Mm mm) = Position mm
  posTo (Position mm) = Mm mm

instance PositionFromTo Centimeter where
  posFrom (Cm cm) = Position (cm * 10)
  posTo (Position mm) = Cm (mm / 10)

---

instance ToMillimeters Position where
  toMm (Position v) = v

instance FromMillimeters Position where
  fromMm v = Position v

instance ToCentimeters Position where
  toCm (Position v) = v / 10

instance FromCentimeters Position where
  fromCm v = Position (v * 10)

-------------------------------------------------------------------------------

instance Scalable Factor Position where
  scale factor (Position v) = Position (v * toDouble factor)

instance Scalable AbsFactor Position where
  scale factor (Position v) = Position (v * toDouble factor)

instance Add Position Delta where
  add pos del = fromMm (toMm pos + toMm del)

instance Add Position Length where
  add pos len = fromMm (toMm pos + toMm len)

instance Sub Position Length where
  sub pos len = fromMm (toMm pos - toMm len)

instance Sub Position Delta where
  sub pos del = fromMm (toMm pos - toMm del)

instance GetDelta Position Delta where
  getDelta (Position x) (Position y) = fromMm (y - x)

instance Distance Position where
  getDistance (Position x) (Position y) = unsafeFromMm $ abs (y - x)
