module Main where

import Filamento
import Filamento.IO
import Filamento.Lib (initPrinter, printRect)
import Linear
import Relude

printSketch :: GCode ()
printSketch = do
  initPrinter do
    local (\env -> env {transpose = \(V3 x y z) -> V3 x (y + 0) z}) do
      printRect (V2 0 0) (V2 100 100)
      pure ()

main :: IO ()
main =
  run \_ ->
    ( "out/myprint.gcode",
      local mkEnv printSketch
    )
  where
    mkEnv :: GCodeEnv -> GCodeEnv
    mkEnv env = env
