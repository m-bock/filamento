module Filamento.Types.MeasureUnits where

import Relude

newtype Millimeter = Mm {mm :: Double}

newtype Centimeter = Cm {cm :: Double}

newtype Hertz = Hz {hz :: Double}

newtype Millisecond = Ms {ms :: Double}

newtype Second = Secs {secs :: Double}

newtype Celsius = Celsius {degrees :: Double}