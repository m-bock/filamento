module Main where

import Control.Monad.RWS (MonadWriter (..))
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Data.Map.Strict as Map
import Filamento
import Relude

-- Generate a random polygon with n points that fills most of the bed
generateRandomPolygon :: Int -> Double -> [Position2D]
generateRandomPolygon n radius =
  let -- Generate random angles and radii
      angles = [2 * pi * fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]
      -- Add some randomness to angles to make it less regular
      randomAngles = zipWith (\angle i -> angle + 0.3 * sin (fromIntegral i * pi / 3)) angles [0 .. n - 1]
      -- Generate random radii between 0.6 and 1.0 of the given radius
      randomRadii = [radius * (0.6 + 0.4 * sin (fromIntegral i * pi / 2)) | i <- [0 .. n - 1]]
      -- Create points from polar coordinates
      points = zipWith (\angle r -> pos2fromMm (r * cos angle) (r * sin angle)) randomAngles randomRadii
   in points

-- initPrinter $
printSketch :: GCode ()
printSketch = initPrinter do
  printSketchFrame

  withSketchTranspose $ do
    printLayers $ \_outOf -> do
      withColors \color -> do
        forM_ [0 .. 3] $ \x -> do
          forM_ [0 .. 3] $ \y -> do
            let d = 30
            let p1 = pos2fromMm (x * d) (y * d)
                p2 = pos2fromMm (x * d + 10) (y * d)
                p3 = pos2fromMm (x * d + 15) (y * d + 15)
                p4 = pos2fromMm (x * d) (y * d + 20)

            color "red" do
              moveTo p1
              extrudeTo p2

            color "yellow" do
              moveTo p2
              extrudeTo p3

            color "red" do
              moveTo p3
              extrudeTo p4

            color "yellow" do
              moveTo p4
              extrudeTo p1

main :: IO ()
main = do
  saveGCodeToFile
    "out/myprint.gcode"
    printSketch
    \env ->
      env
        { lineWidth = fromMm 0.6,
          layerHeight = fromMm 0.3,
          hotendTemperature = fromCelsius 205,
          bedTemperature = fromCelsius 65,
          retractLength = fromMm 1.5,
          colors = "red" :| ["yellow"]
        }
