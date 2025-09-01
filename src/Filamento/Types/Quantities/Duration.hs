module Filamento.Types.Quantities.Duration (Duration, durationPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.NonNegativeFactor (NonNegativeFactor)
import Fmt
import GHC.Generics
import Relude

newtype Duration = Duration {ms :: Double} -- non-negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

isValidDuration :: Double -> Bool
isValidDuration d = d >= 0

instance MaybeFromMilliseconds Duration where
  maybeFromMs v = if isValidDuration v then Just (Duration v) else Nothing
  unsafeFromMs v = if isValidDuration v then Duration v else error "Duration must be non-negative"

instance MaybeFromSeconds Duration where
  maybeFromSecs v = if isValidDuration v then Just (Duration (v * 1000)) else Nothing
  unsafeFromSecs v = if isValidDuration v then Duration (v * 1000) else error "Duration must be non-negative"

instance ToMilliseconds Duration where
  toMs (Duration d) = d

instance ToSeconds Duration where
  toSecs (Duration d) = d / 1000

instance Scalable NonNegativeFactor Duration where
  scale factor (Duration d) = Duration (d * toDouble factor)

instance Add Duration Duration where
  add (Duration d1) (Duration d2) = Duration (d1 + d2)

durationPretty :: Duration -> Text
durationPretty (Duration ms) = fixedF 2 ms |+ "ms"
