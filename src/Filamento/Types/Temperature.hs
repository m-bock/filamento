module Filamento.Types.Temperature
  ( Temperature,
    fromCelsius,
    toCelsius,
  )
where

import Filamento.Conversions
import Relude

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

fromCelsius :: Double -> Temperature
fromCelsius t = Temperature t

toCelsius :: Temperature -> Double
toCelsius (Temperature t) = t