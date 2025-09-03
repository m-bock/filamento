module Filamento.Classes.Abs (Abs, FromToAbs (..), unsafeMkAbs) where

import Relude

newtype Abs a = Abs a

unsafeMkAbs :: a -> Abs a
unsafeMkAbs = Abs

class FromToAbs a where
  toAbs :: a -> Abs a
  fromAbs :: Abs a -> a

instance FromToAbs Double where
  toAbs n = Abs (abs n)
  fromAbs (Abs n) = abs n

instance FromToAbs Int where
  toAbs n = Abs (abs n)
  fromAbs (Abs n) = abs n
