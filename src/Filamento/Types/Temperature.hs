module Filamento.Types.Temperature
  ( Temperature,
    tempFromCelsius,
    tempToCelsius,
  )
where

import Relude

newtype Temperature = Temperature {degrees :: Double}
  deriving (Show, Eq, Num)

tempFromCelsius :: Double -> Temperature
tempFromCelsius t = Temperature t

tempToCelsius :: Temperature -> Double
tempToCelsius (Temperature t) = t