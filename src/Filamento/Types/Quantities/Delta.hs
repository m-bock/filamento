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

newtype Delta = Delta {mm :: Double}
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (Fractional, RealFrac, Real, Num)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

instance FromToMillimeters Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

instance FromToCentimeters Delta where
  toCm (Delta v) = v
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

instance Add (V2 Delta) (V2 Delta) where
  add (V2 x y) (V2 x' y') = V2 (add x x') (add y y')

instance Sub (V2 Delta) (V2 Delta) where
  sub (V2 x y) (V2 x' y') = V2 (sub x x') (sub y y')

instance Scalable Factor (V2 Delta) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Scalable NonNegativeFactor (V2 Delta) where
  scale factor (V2 x y) = V2 (scale factor x) (scale factor y)

instance Add (V3 Delta) (V3 Delta) where
  add (V3 x y z) (V3 x' y' z') = V3 (add x x') (add y y') (add z z')

instance Sub (V3 Delta) (V3 Delta) where
  sub (V3 x y z) (V3 x' y' z') = V3 (sub x x') (sub y y') (sub z z')

instance Scalable Factor (V3 Delta) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

instance Scalable NonNegativeFactor (V3 Delta) where
  scale factor (V3 x y z) = V3 (scale factor x) (scale factor y) (scale factor z)

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