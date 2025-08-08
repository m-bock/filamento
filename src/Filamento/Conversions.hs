module Filamento.Conversions where

import Control.Newtype
import Relude

class Convert lo hi where
  to :: hi -> lo
  from :: lo -> hi

-- to' :: forall b b' a. (Convert b a, Newtype b b') => b' -> a
-- to' = to . (pack :: b' -> b)

-- from' :: forall b b' a. (Convert b a, Newtype b b') => a -> b'
-- from' = (unpack :: b -> b') . from

-- fromF :: (Convert a (f b'), Functor f) => (b -> b') -> f b -> a
-- fromF f val = from $ fmap f val

newtype MM = MM Double
  deriving (Show, Eq, Num)

newtype MMPerSec = MMPerSec Double
  deriving (Show, Eq, Num)

newtype MMPerMin = MMPerMin Double
  deriving (Show, Eq, Num)

newtype CM = CM Double
  deriving (Show, Eq, Num)

newtype Sec = Sec Double
  deriving (Show, Eq, Num)

newtype MS = MS Double
  deriving (Show, Eq, Num)

newtype Celsius = Celsius Double
  deriving (Show, Eq, Num)

newtype Hz = Hz Double
  deriving (Show, Eq, Num)