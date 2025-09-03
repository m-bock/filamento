module Filamento.Types.MeasureUnits where

import Filamento.Classes.Abs (Abs, FromToAbs (..), unsafeMkAbs)
import Relude

newtype Millimeter = Mm {mm :: Double}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Num, Fractional)

instance FromToAbs Millimeter where
  toAbs mm = unsafeMkAbs mm
  fromAbs mm = fromAbs mm

newtype Centimeter = Cm {cm :: Double}

newtype Hertz = Hz {hz :: Double}

newtype Millisecond = Ms {ms :: Double}

newtype Second = Secs {secs :: Double}

newtype Celsius = Celsius {degrees :: Double}