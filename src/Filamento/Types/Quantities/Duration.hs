module Filamento.Types.Quantities.Duration (Duration, durationPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.AbsFactor (AbsFactor)
import Fmt
import GHC.Generics
import Relude

newtype Duration = Duration {ms :: Double} -- non-negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

instance MaybeFromMilliseconds Duration where
  maybeFromMs v = if v >= 0 then Just (Duration v) else Nothing

instance MaybeFromSeconds Duration where
  maybeFromSecs v = if v >= 0 then Just (Duration (v * 1000)) else Nothing

instance ToMilliseconds Duration where
  toMs (Duration d) = d

instance ToSeconds Duration where
  toSecs (Duration d) = d / 1000

instance Scalable AbsFactor Duration where
  scale factor (Duration d) = Duration (d * toDouble factor)

instance Add Duration Duration where
  add (Duration d1) (Duration d2) = Duration (d1 + d2)

durationPretty :: Duration -> Text
durationPretty (Duration ms) = fixedF 2 ms |+ "ms"
