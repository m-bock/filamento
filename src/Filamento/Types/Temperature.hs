module Filamento.Types.Temperature (Temperature) where

import Filamento.Conversions
import Relude

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

instance Convert Celsius Temperature where
  from (Celsius t) = Temperature t
  to (Temperature t) = Celsius t