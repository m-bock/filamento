{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified Data.Text as T
import Marlin.DSL
import Relude

heatup :: Int -> Int -> GCode a -> GCode a
heatup bedTemp hotendTemp inner = do
  section "Heatup" $ do
    setBedTemperature
      & setTargetTemperature bedTemp
      & toGCode

    setHotendTemperature
      & setTargetTemperature hotendTemp
      & toGCode

  ret <- inner

  section "Wait for temperatures" $ do
    waitForBedTemperature
      & setTargetTemperature bedTemp
      & toGCode

    waitForHotendTemperature
      & setTargetTemperature hotendTemp
      & toGCode

  pure ret

coolDown :: GCode ()
coolDown =
  section "Cool down" $ do
    raw "M104 S0" "turn off nozzle"
    raw "M140 S0" "turn off bed"
    raw "M107" "turn off fan"
    raw "M84" "motors off"

sampleProgram :: GCode ()
sampleProgram = do
  setUnits Millimeter
  heatup 60 200 (pure ())
  autoHome_

  raw "M83" "set extruder to relative mode"

  let step = 10.0

  for_ (enumFromTo 10 20) $ \x' -> do
    linearMove
      & x (fromIntegral x' * step)
      & y 0.0
      & z 0.2
      & extrude 0
      & toGCode

    linearMove
      & x (fromIntegral x' * step)
      & y 100.0
      & z 0.2
      & extrude 10
      & toGCode

main :: IO ()
main = do
  let codeStr = toText sampleProgram
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr