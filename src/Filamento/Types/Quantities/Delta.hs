module Filamento.Types.Quantities.Delta (Delta, deltaPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.AbsFactor (AbsFactor)
import Filamento.Types.Continous.Factor (Factor)
import Filamento.Types.Quantities.Length (Length)
import Fmt
import GHC.Generics
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

instance Scalable AbsFactor Delta where
  scale factor (Delta v) = Delta (v * toDouble factor)

instance Add Delta Delta where
  add d1 d2 = fromMm (toMm d1 + toMm d2)

instance Add Delta Length where
  add d l = fromMm (toMm d + toMm l)

instance Sub Delta Delta where
  sub d l = fromMm (toMm d - toMm l)

instance Sub Delta Length where
  sub d l = fromMm (toMm d + toMm l)

deltaPretty :: Delta -> Text
deltaPretty (Delta d) = fixedF 2 d |+ "mm"
