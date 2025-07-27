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
    { startLayer = 0
    }

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  env <- ask
  forM_ [env.startLayer .. 50] $ \i -> do
    section ("Layer " <> show i) $ do
      moveZ (0.2 + fromIntegral i * 0.2)
      printSquare (V2 100 100) (V2 100 100)

main :: IO ()
main = do
  let codeStr = toText sketch
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr