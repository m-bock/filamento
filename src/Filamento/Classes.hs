module Filamento.Classes where

import Linear
import Relude

class FromToMillimeters a where
  fromMm :: Double -> a
  toMm :: a -> Double

fromMm3 :: (FromToMillimeters a) => V3 Double -> V3 a
fromMm3 (V3 x y z) = V3 (fromMm x) (fromMm y) (fromMm z)

fromMm2 :: (FromToMillimeters a) => V2 Double -> V2 a
fromMm2 (V2 x y) = V2 (fromMm x) (fromMm y)

toMm3 :: (FromToMillimeters a) => V3 a -> V3 Double
toMm3 (V3 x y z) = V3 (toMm x) (toMm y) (toMm z)

toMm2 :: (FromToMillimeters a) => V2 a -> V2 Double
toMm2 (V2 x y) = V2 (toMm x) (toMm y)

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

class Scalable factor a where
  scale :: factor -> a -> a

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
