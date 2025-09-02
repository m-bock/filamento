{-# LANGUAGE UndecidableInstances #-}

module Filamento.Classes where

import Linear (V2 (V2), V3 (V3))
import Linear.V3 (V3)
import Relude
import Relude.Unsafe (fromJust)

class MaybeFromNatural a where
  maybeFromNat :: Natural -> Maybe a

unsafeFromNat :: (HasCallStack, MaybeFromNatural a) => Natural -> a
unsafeFromNat = fromJust . maybeFromNat

class FromNatural a where
  fromNat :: Natural -> a

class ToNatural a where
  toNat :: a -> Natural

--------------------------------------------------------------------------------

class MaybeFromInt a where
  maybeFromInt :: Int -> Maybe a

unsafeFromInt :: (HasCallStack, MaybeFromInt a) => Int -> a
unsafeFromInt = fromJust . maybeFromInt

class FromInt a where
  fromInt :: Int -> a

class ToInt a where
  toInt :: a -> Int

--------------------------------------------------------------------------------

class MaybeFromDouble a where
  maybeFromDouble :: Double -> Maybe a

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

unsafeFromMm :: (HasCallStack, MaybeFromMillimeters a) => Double -> a
unsafeFromMm = fromJust . maybeFromMm

absFromMm :: (FromMillimeters a) => Double -> a
absFromMm = fromMm . abs

class FromMillimeters a where
  fromMm :: Double -> a

class ToMillimeters a where
  toMm :: a -> Double

fromMmF :: (FromMillimeters a, Functor f) => f Double -> f a
fromMmF = fmap fromMm

toMmF :: (ToMillimeters a, Functor f) => f a -> f Double
toMmF = fmap toMm

maybeFromMmF :: (MaybeFromMillimeters a, Functor f) => f Double -> f (Maybe a)
maybeFromMmF = fmap maybeFromMm

unsafeFromMmF :: (MaybeFromMillimeters a, Functor f) => f Double -> f a
unsafeFromMmF = fmap unsafeFromMm

viaMm :: (ToMillimeters a, FromMillimeters b) => a -> b
viaMm = fromMm . toMm

maybeViaMm :: (ToMillimeters a, MaybeFromMillimeters b) => a -> Maybe b
maybeViaMm = maybeFromMm . toMm

--------------------------------------------------------------------------------

class MaybeFromCentimeters a where
  maybeFromCm :: Double -> Maybe a

unsafeFromCm :: (HasCallStack, MaybeFromCentimeters a) => Double -> a
unsafeFromCm = fromJust . maybeFromCm

class FromCentimeters a where
  fromCm :: Double -> a

class ToCentimeters a where
  toCm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromSquareMillimeters a where
  maybeFromSqMm :: Double -> Maybe a

unsafeFromSqMm :: (HasCallStack, MaybeFromSquareMillimeters a) => Double -> a
unsafeFromSqMm = fromJust . maybeFromSqMm

class FromSquareMillimeters a where
  fromSqMm :: Double -> a

class ToSquareMillimeters a where
  toSqMm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromCubicMillimeters a where
  maybeFromCuMm :: Double -> Maybe a

unsafeFromCuMm :: (HasCallStack, MaybeFromCubicMillimeters a) => Double -> a
unsafeFromCuMm = fromJust . maybeFromCuMm

class FromCubicMillimeters a where
  fromCuMm :: Double -> a

class ToCubicMillimeters a where
  toCuMm :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromSeconds a where
  maybeFromSecs :: Double -> Maybe a

unsafeFromSecs :: (HasCallStack, MaybeFromSeconds a) => Double -> a
unsafeFromSecs = fromJust . maybeFromSecs

class FromSeconds a where
  fromSecs :: Double -> a

class ToSeconds a where
  toSecs :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromCelsius a where
  maybeFromCelsius :: Double -> Maybe a

unsafeFromCelsius :: (HasCallStack, MaybeFromCelsius a) => Double -> a
unsafeFromCelsius = fromJust . maybeFromCelsius

class FromCelsius a where
  fromCelsius :: Double -> a

class ToCelsius a where
  toCelsius :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromMillimetersPerSecond a where
  maybeFromMmPerSec :: Double -> Maybe a

unsafeFromMmPerSec :: (HasCallStack, MaybeFromMillimetersPerSecond a) => Double -> a
unsafeFromMmPerSec = fromJust . maybeFromMmPerSec

class FromMillimetersPerSecond a where
  fromMmPerSec :: Double -> a

class ToMillimetersPerSecond a where
  toMmPerSec :: a -> Double

--------------------------------------------------------------------------------

class MaybeFromMillimetersPerMinute a where
  maybeFromMmPerMin :: Double -> Maybe a

unsafeFromMmPerMin :: (HasCallStack, MaybeFromMillimetersPerMinute a) => Double -> a
unsafeFromMmPerMin = fromJust . maybeFromMmPerMin

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

instance (Monoid a) => JustX (V2 a) where
  justX (V2 x _) = V2 x mempty

instance (Monoid a) => JustX (V3 a) where
  justX (V3 x _ _) = V3 x mempty mempty

--------------------------------------------------------------------------------

class JustY a where
  justY :: a -> a

instance (Monoid a) => JustY (V2 a) where
  justY (V2 _ y) = V2 mempty y

instance (Monoid a) => JustY (V3 a) where
  justY (V3 _ y _) = V3 mempty y mempty

--------------------------------------------------------------------------------

class JustZ a where
  justZ :: a -> a

instance (Monoid a) => JustZ (V3 a) where
  justZ (V3 _ _ z) = V3 mempty mempty z

--------------------------------------------------------------------------------

class Scalable factor a where
  scale :: factor -> a -> a

class Add abs rel where
  add :: abs -> rel -> abs

instance (Add abs rel) => Add (V2 abs) (V2 rel) where
  add a b = add <$> a <*> b

instance (Add abs rel) => Add (V3 abs) (V3 rel) where
  add a b = add <$> a <*> b

--------------------------------------------------------------------------------

class Sub abs rel | abs -> rel where
  sub :: abs -> rel -> abs

instance (Sub abs rel) => Sub (V2 abs) (V2 rel) where
  sub a b = sub <$> a <*> b

instance (Sub abs rel) => Sub (V3 abs) (V3 rel) where
  sub a b = sub <$> a <*> b

neg :: (Sub a a) => a -> a
neg val = sub val $ sub val val

--------------------------------------------------------------------------------

class GetDelta abs rel | abs -> rel where
  getDelta :: abs -> abs -> rel

instance (GetDelta abs rel) => GetDelta (V2 abs) (V2 rel) where
  getDelta a b = getDelta <$> a <*> b

instance (GetDelta abs rel) => GetDelta (V3 abs) (V3 rel) where
  getDelta a b = getDelta <$> a <*> b

--------------------------------------------------------------------------------

class Distance lo hi | hi -> lo where
  getDistance :: hi -> hi -> lo
