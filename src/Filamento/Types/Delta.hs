module Filamento.Types.Delta
  ( Delta,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Filamento.Classes
import GHC.Generics
import Linear (V2 (..), V3 (..))
import Relude

newtype Delta = Delta Double
  deriving (Show, Eq, Generic, Num, Ord, Fractional, RealFrac, Real)
  deriving (Semigroup, Monoid) via (Sum Double)

instance ToJSON Delta

instance FromJSON Delta

instance Scalable Delta where
  scale factor (Delta v) = Delta (v * factor)

instance Millimeters Double Delta where
  toMm (Delta v) = v
  fromMm v = Delta v

instance Millimeters (V2 Double) (V2 Delta) where
  toMm (V2 x y) = V2 (toMm x) (toMm y)
  fromMm (V2 x y) = V2 (fromMm x) (fromMm y)

instance Millimeters (V3 Double) (V3 Delta) where
  toMm (V3 x y z) = V3 (toMm x) (toMm y) (toMm z)
  fromMm (V3 x y z) = V3 (fromMm x) (fromMm y) (fromMm z)

instance JustX (V2 Delta) where
  justX (V2 x _) = V2 x 0

instance JustY (V2 Delta) where
  justY (V2 _ y) = V2 0 y
