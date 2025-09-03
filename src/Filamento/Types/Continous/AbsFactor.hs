module Filamento.Types.Continous.AbsFactor
  ( AbsFactor,
    prettyAbsFactor,
    FromToAbsFactor (..),
  )
where

import Data.Aeson.Types
import Filamento.Classes (Abs, FromAbs (..), MaybeFromDouble (..), ToDouble (..))
import Fmt
import GHC.Generics
import Relude

newtype AbsFactor = AbsFactor Double -- non-negative
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via (Sum Double)

instance MaybeFromDouble AbsFactor where
  maybeFromDouble x = if x >= 0 then Just (AbsFactor x) else Nothing

instance ToDouble AbsFactor where
  toDouble (AbsFactor x) = x

prettyAbsFactor :: AbsFactor -> Text
prettyAbsFactor (AbsFactor x) = fixedF 2 x |+ ""

--------------------------------------------------------------------------------

class FromToAbsFactor a where
  toAbsFactor :: a -> AbsFactor
  fromAbsFactor :: AbsFactor -> a

instance FromToAbsFactor Double where
  toAbsFactor x = AbsFactor (abs x)
  fromAbsFactor (AbsFactor x) = x
