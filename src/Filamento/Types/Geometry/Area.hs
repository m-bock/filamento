module Filamento.Types.Geometry.Area (Area, areaFromVec2) where

import Filamento.Classes
import Filamento.Types.Quantities.Length
import Linear (V2 (V2))
import Relude

-------------------------------------------------------------------------------

newtype Area = Area {sqMm :: Double}

areaFromVec2 :: V2 Length -> Area
areaFromVec2 (V2 x y) = Area (toMm x * toMm y)

instance FromSquareMillimeters Area where
  fromSqMm x = Area x

instance ToSquareMillimeters Area where
  toSqMm (Area x) = x