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
    setHotendOff
    setBedOff
    setFanOff
    motorsOff

sampleProgram :: GCode ()
sampleProgram = do
  setUnits Millimeter
  setExtruderRelative

  heatup 60 200 $ do
    autoHome_

  let step = 10.0

  for_ (enumFromTo 10 20) $ \x' -> do
    linearMove
      & setX (fromIntegral x' * step)
      & setY 0.0
      & setZ 0.2
      & setExtrude 0
      & toGCode

    linearMove
      & setX (fromIntegral x' * step)
      & setY 100.0
      & setZ 0.2
      & setExtrude 10
      & toGCode

  pause 30 -- instead of raw "G4 S30" "wait 30 seconds"
  coolDown

main :: IO ()
main = do
  let codeStr = toText sampleProgram
  writeFileText "out/myprint.gcode" codeStr
  putStrLn $ T.unpack codeStr