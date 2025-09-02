module Filamento.Types.Continous.NonNegativeFactor
  ( NonNegativeFactor,
    prettyNonNegativeFactor,
  )
where

import Data.Aeson.Types
import Filamento.Classes (MaybeFromDouble (..), ToDouble (..))
import Fmt
import GHC.Generics
import Relude

newtype NonNegativeFactor = NonNegativeFactor Double -- non-negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sum Double)

instance MaybeFromDouble NonNegativeFactor where
  maybeFromDouble x = if x >= 0 then Just (NonNegativeFactor x) else Nothing

instance ToDouble NonNegativeFactor where
  toDouble (NonNegativeFactor x) = x

prettyNonNegativeFactor :: NonNegativeFactor -> Text
prettyNonNegativeFactor (NonNegativeFactor x) = fixedF 2 x |+ ""