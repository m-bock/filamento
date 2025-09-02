module Filamento.Types.Geometry.Volume (Volume, volumeFromVec3, volumeFromArea) where

import Filamento.Classes
import Filamento.Types.Geometry.Area
import Filamento.Types.Quantities.Length
import Linear (V3 (V3))
import Relude

-------------------------------------------------------------------------------

newtype Volume = Volume {cuMm :: Double}

volumeFromVec3 :: V3 Length -> Volume
volumeFromVec3 (V3 x y z) = Volume (toMm x * toMm y * toMm z)

instance FromCubicMillimeters Volume where
  fromCuMm x = Volume x

instance ToCubicMillimeters Volume where
  toCuMm (Volume x) = x

volumeFromArea :: Area -> Length -> Volume
volumeFromArea area d = Volume (toSqMm area * toMm d)
