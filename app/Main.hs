module Main where

import Filamento
import Filamento.IO
import Filamento.Lib
import Filamento.Types.Displacement2D as Disp2D
import Filamento.Types.Position2D as Pos2D
import Linear
import Relude

printSketch :: GCode ()
printSketch = do
  initPrinter do
    printRect2d (pos2FromMm $ V2 0 0) (delta2FromMm $ V2 100 100)
    pure ()

{-local (\env -> env {transpose = \(V3 x y z) -> V3 x (y + 0) z})-}

main :: IO ()
main =
  run \_ ->
    ( "out/myprint.gcode",
      local mkEnv printSketch
    )
  where
    mkEnv :: GCodeEnv -> GCodeEnv
    mkEnv env = env
