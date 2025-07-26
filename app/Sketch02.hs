{-# OPTIONS_GHC -Wno-type-defaults #-}

module Sketch02 where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

sketch :: GCode ()
sketch = do
  setUnits Millimeter
  setExtruderRelative

  initPrinter

  moveTo3d (V3 0 0 0.2)

  printTestStripes

  printSquare (V2 50 50) (V2 50 50)

  finalPark

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr