module Filamento.Types.Quantities.Length (Length, lengthPretty) where

import Data.Aeson.Types
import Filamento.Classes
import Filamento.Types.Continous.NonNegativeFactor (NonNegativeFactor)
import Fmt
import GHC.Generics
import Relude

newtype Length = Length {mm :: Double} -- non-negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving (Semigroup, Monoid) via (Sum Double)
  deriving anyclass (ToJSON, FromJSON)

instance MaybeFromMillimeters Length where
  maybeFromMm v = if isLengthValid v then Just (Length v) else Nothing
  unsafeFromMm v = if isLengthValid v then Length v else error "Length must be non-negative"

isLengthValid :: Double -> Bool
isLengthValid l = l >= 0

instance FromMillimeters Length where
  fromMm v = Length v

instance ToMillimeters Length where
  toMm (Length l) = l

instance ToCentimeters Length where
  toCm (Length l) = l

instance Scalable NonNegativeFactor Length where
  scale factor (Length l) = Length (l * toDouble factor)

instance Add Length Length where
  add (Length l1) (Length l2) = Length (l1 + l2)

lengthPretty :: Length -> Text
lengthPretty (Length l) = fixedF 2 l |+ "mm"