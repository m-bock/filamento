{-# OPTIONS_GHC -Wno-type-defaults #-}

module Sketch03 where

import qualified Data.Text as T
import Filamento
-- import Filamento.Lib
-- import Linear (V3 (..))
-- import Linear.V2 (V2 (..))

import Linear (V2 (..))
import Relude

changeEnv :: GCodeEnv -> GCodeEnv
changeEnv env =
  env

printLayer :: Int -> GCode ()
printLayer n = section ("Layer " <> T.pack (show n)) $ do
  printRect (v3PosFromMm 100 100 0) (fromMmF $ V2 50 50)

sketch :: GCode ()
sketch = local changeEnv $ initPrinter $ do
  -- raw "M106 S255" "Turn on fan"

  forM_ [1 .. 200] \i -> do
    -- playTone & setFrequency 1000 & setDuration 100 & toGCode
    nextLayer

    printLayer i

main :: IO ()
main = pure ()