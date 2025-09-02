module Filamento.Classes.Distance where

import Filamento.Classes
import Filamento.Types.Quantities.Length
import Linear (V2 (V2), V3 (V3))
import Relude

class Distance a where
  getDistance :: a -> a -> Length

instance (ToMillimeters a, Distance a) => Distance (V2 a) where
  getDistance (V2 x1 y1) (V2 x2 y2) =
    let res = sqrt $ (toMm x2 - toMm x1) ^ 2 + (toMm y2 - toMm y1) ^ 2
     in unsafeFromMm res

instance (ToMillimeters a, Distance a) => Distance (V3 a) where
  getDistance (V3 x1 y1 z1) (V3 x2 y2 z2) =
    let res = sqrt $ (toMm x2 - toMm x1) ^ 2 + (toMm y2 - toMm y1) ^ 2 + (toMm z2 - toMm z1) ^ 2
     in unsafeFromMm res
