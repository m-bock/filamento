module Filamento.Classes where

import Relude

class FromToMillimeters a where
  fromMm :: Double -> a
  toMm :: a -> Double

fromMmF :: (FromToMillimeters a, Functor f) => f Double -> f a
fromMmF = fmap fromMm

toMmF :: (FromToMillimeters a, Functor f) => f a -> f Double
toMmF = fmap toMm

class FromToSeconds a where
  fromSecs :: Double -> a
  toSecs :: a -> Double

class FromToCelsius a where
  fromCelsius :: Double -> a
  toCelsius :: a -> Double

class FromToMillimetersPerSecond a where
  fromMmPerSec :: Double -> a
  toMmPerSec :: a -> Double

class FromToMillimetersPerMinute a where
  fromMmPerMin :: Double -> a
  toMmPerMin :: a -> Double

class FromToHertz a where
  fromHz :: Double -> a
  toHz :: a -> Double

class FromToMilliseconds a where
  fromMs :: Double -> a
  toMs :: a -> Double

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
