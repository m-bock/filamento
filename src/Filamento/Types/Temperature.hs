module Filamento.Types.Temperature
  ( Temperature,
    temperatureFromCelsius,
    temperatureToCelsius,
  )
where

import Relude

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

temperatureFromCelsius :: Double -> Temperature
temperatureFromCelsius t = Temperature t

temperatureToCelsius :: Temperature -> Double
temperatureToCelsius (Temperature t) = t