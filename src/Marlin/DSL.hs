module Marlin.DSL where

import Marlin.Core
import Relude

linearMove :: LinearMove
linearMove =
  LinearMove
    { _x = Nothing,
      _y = Nothing,
      _z = Nothing,
      _e = Nothing,
      _f = Nothing
    }

class HasX a where
  setX :: Double -> a -> a

class HasY a where
  setY :: Double -> a -> a

class HasZ a where
  setZ :: Double -> a -> a

class HasE a where
  setE :: Double -> a -> a

class HasF a where
  setF :: Int -> a -> a

instance HasX LinearMove where
  setX x' obj = obj {_x = Just x'}

class IsGCode a where
  run :: a -> GCode

instance IsGCode LinearMove where
  run = GLinearMove

render :: [GCode] -> Text
render = unlines . map (toText . toGCodeLine)