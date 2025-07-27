{-# OPTIONS_GHC -Wno-type-defaults #-}

module Sketch02 where

import qualified Data.Text as T
import Linear (V3 (..))
import Linear.V2 (V2 (..))
import Marlin.DSL
import Marlin.Lib
import Relude

changeEnv :: GCodeEnv -> GCodeEnv
changeEnv env =
  env

printLayer :: Int -> GCode ()
printLayer i = section ("Layer " <> show i) $ do
  let countLayers = 200
  let baseRadius = 20
  let pct = fromIntegral i / fromIntegral countLayers
  let pctRad = pct * 2 * pi

  let radius = baseRadius + 5 * cos pctRad

  printPolygon 10 (V2 100 100) radius

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  env <- ask
  forM_ [1 .. 200] \i -> do
    nextLayer
    printLayer i

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr