module Filamento.Types.Quantities.Delta (Delta, deltaPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.Factor (Factor)
import Filamento.Types.Continous.NonNegativeFactor (NonNegativeFactor)
import Fmt
import GHC.Generics
import Linear (V2 (V2))
import Linear.V3 (V3 (..))
import Relude

newtype Delta = Delta {mm :: Double} -- may be negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (Fractional, RealFrac, Real, Num)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

instance ToMillimeters Delta where
  toMm (Delta v) = v

instance FromMillimeters Delta where
  fromMm v = Delta v

instance ToCentimeters Delta where
  toCm (Delta v) = v

instance FromCentimeters Delta where
  fromCm v = Delta v

instance Scalable Factor Delta where
  scale factor (Delta v) = Delta (v * toDouble factor)

instance Scalable NonNegativeFactor Delta where
  scale factor (Delta v) = Delta (v * toDouble factor)

instance Add Delta Delta where
  add (Delta x) (Delta y) = Delta (x + y)

instance Sub Delta Delta where
  sub (Delta x) (Delta y) = Delta (x - y)

deltaPretty :: Delta -> Text
deltaPretty (Delta d) = fixedF 2 d |+ "mm"

-- Vector instances

instance Scalable Factor (V2 Delta) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable NonNegativeFactor (V2 Delta) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable Factor (V3 Delta) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

instance Scalable NonNegativeFactor (V3 Delta) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)
