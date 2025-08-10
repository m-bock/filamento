module Filamento.Types.Duration
  ( Duration,
    durFromMs,
    durToMs,
    durFromSecs,
    durToSecs,
  )
where

import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

durFromMs :: Double -> Duration
durFromMs d = Duration d

durToMs :: Duration -> Double
durToMs (Duration d) = d

durToSecs :: Duration -> Double
durToSecs (Duration d) = d / factorSecs

durFromSecs :: Double -> Duration
durFromSecs s = Duration (s * factorSecs)

factorSecs :: Double
factorSecs = 1000