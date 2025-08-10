module Filamento.Classes where

import Relude

class Millimeters lo hi | hi -> lo where
  fromMm :: lo -> hi
  toMm :: hi -> lo

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