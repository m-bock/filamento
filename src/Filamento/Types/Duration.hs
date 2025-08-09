module Filamento.Types.Duration
  ( Duration,
    durationFromMs,
    durationToMs,
  )
where

import Relude

newtype Duration = Duration {ms :: Double}
  deriving (Show, Eq, Generic)

durationFromMs :: Double -> Duration
durationFromMs d = Duration d

durationToMs :: Duration -> Double
durationToMs (Duration d) = d