module Filamento.Classes where

import Relude

class Millimeters lo hi | hi -> lo where
  fromMm :: lo -> hi
  toMm :: hi -> lo

class Millimeters3 lo hi | hi -> lo where
  fromMm3 :: lo -> lo -> lo -> hi
  toMm3 :: hi -> (lo, lo, lo)

class Millimeters2 lo hi | hi -> lo where
  fromMm2 :: lo -> lo -> hi
  toMm2 :: hi -> (lo, lo)

class Seconds lo hi | hi -> lo where
  fromSecs :: lo -> hi
  toSecs :: hi -> lo

class Celsius lo hi | hi -> lo where
  fromCelsius :: lo -> hi
  toCelsius :: hi -> lo

class MillimetersPerSecond lo hi | hi -> lo where
  fromMmPerSec :: lo -> hi
  toMmPerSec :: hi -> lo

class MillimetersPerMinute lo hi | hi -> lo where
  fromMmPerMin :: lo -> hi
  toMmPerMin :: hi -> lo

class Hertz lo hi | hi -> lo where
  fromHz :: lo -> hi
  toHz :: hi -> lo

class Scalable a where
  scale :: Double -> a -> a

class JustX a where
  justX :: a -> a

class JustY a where
  justY :: a -> a

class JustZ a where
  justZ :: a -> a

class DeltaApplication abs rel | abs -> rel where
  addDelta :: abs -> rel -> abs
  subDelta :: abs -> rel -> abs

class Milliseconds lo hi | hi -> lo where
  fromMs :: lo -> hi
  toMs :: hi -> lo

class FractionalValue a where
  fromFraction :: Double -> a
  toFraction :: a -> Double
  clampFraction :: Double -> a

class FromToNatural lo hi | hi -> lo where
  fromNat :: lo -> hi
  toNat :: hi -> lo

class FromToInt lo hi | hi -> lo where
  fromInt :: lo -> hi
  toInt :: hi -> lo

class FromToDouble lo hi | hi -> lo where
  fromDouble :: lo -> hi
  toDouble :: hi -> lo

class GetDelta abs rel | abs -> rel where
  getDelta :: abs -> abs -> rel

class Distance lo hi | hi -> lo where
  getDistance :: hi -> hi -> lo

class IsOld old new | old -> new where
  fromOld :: old -> new
  toOld :: new -> old