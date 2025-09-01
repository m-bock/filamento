module Filamento.Classes where

import Relude

class FromToNatural a where
  fromNat :: Natural -> a
  toNat :: a -> Natural

class FromToInt a where
  fromInt :: Int -> a
  toInt :: a -> Int

class FromDouble a where
  fromDouble :: Double -> a

class ToDouble a where
  toDouble :: a -> Double

viaDouble :: (ToDouble a, FromDouble b) => a -> b
viaDouble = fromDouble . toDouble

instance ToDouble Nat where
  toDouble = fromIntegral

class FromToMillimeters a where
  fromMm :: Double -> a
  toMm :: a -> Double

class FromToCentimeters a where
  fromCm :: Double -> a
  toCm :: a -> Double

class FromToSquareMillimeters a where
  fromSqMm :: Double -> a
  toSqMm :: a -> Double

class FromToCubicMillimeters a where
  fromCuMm :: Double -> a
  toCuMm :: a -> Double

fromMmF :: (FromToMillimeters a, Functor f) => f Double -> f a
fromMmF = fmap fromMm

toMmF :: (FromToMillimeters a, Functor f) => f a -> f Double
toMmF = fmap toMm

viaMm :: (FromToMillimeters a, FromToMillimeters b) => a -> b
viaMm = fromMm . toMm

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

class JustX a where
  justX :: a -> a

class JustY a where
  justY :: a -> a

class JustZ a where
  justZ :: a -> a

class Scalable factor a | a -> factor where
  scale :: factor -> a -> a

class Add abs rel | abs -> rel where
  add :: abs -> rel -> abs

class Sub abs rel | abs -> rel where
  sub :: abs -> rel -> abs

class GetDelta abs rel | abs -> rel where
  getDelta :: abs -> abs -> rel

class Distance lo hi | hi -> lo where
  getDistance :: hi -> hi -> lo
