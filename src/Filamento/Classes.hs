module Filamento.Classes where

import Relude

class MaybeFromNatural a where
  maybeFromNat :: Natural -> Maybe a
  unsafeFromNat :: Natural -> a

class FromNatural a where
  fromNat :: Natural -> a

class ToNatural a where
  toNat :: a -> Natural

--------------------------------------------------------------------------------

class MaybeFromInt a where
  maybeFromInt :: Int -> Maybe a
  unsafeFromInt :: Int -> a

class FromInt a where
  fromInt :: Int -> a

class ToInt a where
  toInt :: a -> Int

--------------------------------------------------------------------------------

class MaybeFromDouble a where
  maybeFromDouble :: Double -> Maybe a
  unsafeFromDouble :: Double -> a

class FromDouble a where
  fromDouble :: Double -> a

class ToDouble a where
  toDouble :: a -> Double

viaDouble :: (ToDouble a, FromDouble b) => a -> b
viaDouble = fromDouble . toDouble

instance ToDouble Nat where
  toDouble = fromIntegral

--------------------------------------------------------------------------------

class MaybeFromMillimeters a where
  maybeFromMm :: Double -> Maybe a
  unsafeFromMm :: Double -> a

class FromMillimeters a where
  fromMm :: Double -> a

class ToMillimeters a where
  toMm :: a -> Double

fromMmF :: (FromMillimeters a, Functor f) => f Double -> f a
fromMmF = fmap fromMm

toMmF :: (ToMillimeters a, Functor f) => f a -> f Double
toMmF = fmap toMm

viaMm :: (ToMillimeters a, FromMillimeters b) => a -> b
viaMm = fromMm . toMm

--------------------------------------------------------------------------------

class MaybeFromCentimeters a where
  maybeFromCm :: Double -> Maybe a
  unsafeFromCm :: Double -> a

class FromCentimeters a where
  fromCm :: Double -> a

class ToCentimeters a where
  toCm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromSquareMillimeters a where
  maybeFromSqMm :: Double -> Maybe a
  unsafeFromSqMm :: Double -> a

class FromSquareMillimeters a where
  fromSqMm :: Double -> a

class ToSquareMillimeters a where
  toSqMm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromCubicMillimeters a where
  maybeFromCuMm :: Double -> Maybe a
  unsafeFromCuMm :: Double -> a

class FromCubicMillimeters a where
  fromCuMm :: Double -> a

class ToCubicMillimeters a where
  toCuMm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromSeconds a where
  maybeFromSecs :: Double -> Maybe a
  unsafeFromSecs :: Double -> a

class FromSeconds a where
  fromSecs :: Double -> a

class ToSeconds a where
  toSecs :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromCelsius a where
  maybeFromCelsius :: Double -> Maybe a
  unsafeFromCelsius :: Double -> a

class FromCelsius a where
  fromCelsius :: Double -> a

class ToCelsius a where
  toCelsius :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromMillimetersPerSecond a where
  maybeFromMmPerSec :: Double -> Maybe a
  unsafeFromMmPerSec :: Double -> a

class FromMillimetersPerSecond a where
  fromMmPerSec :: Double -> a

class ToMillimetersPerSecond a where
  toMmPerSec :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromMillimetersPerMinute a where
  maybeFromMmPerMin :: Double -> Maybe a
  unsafeFromMmPerMin :: Double -> a

class FromMillimetersPerMinute a where
  fromMmPerMin :: Double -> a

class ToMillimetersPerMinute a where
  toMmPerMin :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromHertz a where
  maybeFromHz :: Double -> Maybe a
  unsafeFromHz :: Double -> a

class FromHertz a where
  fromHz :: Double -> a

class ToHertz a where
  toHz :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromMilliseconds a where
  maybeFromMs :: Double -> Maybe a
  unsafeFromMs :: Double -> a

class FromMilliseconds a where
  fromMs :: Double -> a

class ToMilliseconds a where
  toMs :: a -> Double

--------------------------------------------------------------------------------

class JustX a where
  justX :: a -> a

class JustY a where
  justY :: a -> a

class JustZ a where
  justZ :: a -> a

class Scalable factor a where
  scale :: factor -> a -> a

class Add abs rel where
  add :: abs -> rel -> abs

class Sub abs rel | abs -> rel where
  sub :: abs -> rel -> abs

neg :: (Sub a a) => a -> a
neg val = sub val $ sub val val

class GetDelta abs rel | abs -> rel where
  getDelta :: abs -> abs -> rel

class Distance lo hi | hi -> lo where
  getDistance :: hi -> hi -> lo
