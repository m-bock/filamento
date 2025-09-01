module Filamento.Types.Continous.NonNegativeFactor
  ( NonNegativeFactor,
    mkNonNegativeFactor,
  )
where

import Data.Aeson.Types
import Filamento.Classes (ToDouble (..))
import Fmt
import GHC.Generics
import Relude

newtype NonNegativeFactor = NonNegativeFactor Double
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sum Double)

instance ToDouble NonNegativeFactor where
  toDouble (NonNegativeFactor x) = x

mkNonNegativeFactor :: Double -> Maybe NonNegativeFactor
mkNonNegativeFactor x = if x >= 0 then Just (NonNegativeFactor x) else Nothing

prettyNonNegativeFactor :: NonNegativeFactor -> Text
prettyNonNegativeFactor (NonNegativeFactor x) = fixedF 2 x |+ ""