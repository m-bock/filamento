module Try where

-- import Data.Foldable (Foldable (..))
import Relude

-- import Prelude (Floating (log))

-- Original function (distances grow)
fnByAngle :: Double -> Double -> (Double, Double)
fnByAngle angle spiralConstant =
  let radius = spiralConstant * angle
      x = radius * cos angle
      y = radius * sin angle
   in (x, y)

-- Simpler version using linear approximation
fnApprox :: Double -> Double -> Double -> (Double, Double)
fnApprox arcLength spiralConstant circleRadius =
  let angle = arcLength / spiralConstant -- Simple approximation
      radius = circleRadius + spiralConstant * angle
      x = radius * cos angle
      y = radius * sin angle
   in (x, y)

-- Calculate distance between two points
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- Generate consecutive distances for a list of points
consecutiveDistances :: [(Double, Double)] -> [Double]
consecutiveDistances points =
  case viaNonEmpty tail points of
    Just ptsTail -> zipWith distance points ptsTail
    Nothing -> []

-- -- Test function to show distances are roughly equal
-- testEqualDistances :: IO ()
-- testEqualDistances = do
--   let stepSize = 0.1
--       spiralConstant = 1.0
--       points = [fnApprox (fromIntegral i * stepSize) spiralConstant | i <- [0 .. 20]]
--       distances = consecutiveDistances points
--       avgDistance = Data.Foldable.sum distances / fromIntegral (length distances)
--       maxDist = maximum distances
--       minDist = minimum distances
--       variance = Data.Foldable.sum [(d - avgDistance) ^ 2 | d <- distances] / fromIntegral (length distances)

--   putStrLn "=== Testing fnApprox Equal Distances ==="
--   putStrLn $ "Points generated: " <> show (length points)
--   putStrLn $ "Step size (target): " <> show stepSize
--   putStrLn $ "Average distance: " <> show avgDistance
--   putStrLn $ "Min distance: " <> show minDist
--   putStrLn $ "Max distance: " <> show maxDist
--   putStrLn $ "Variance: " <> show variance
--   putStrLn $ "Deviation from target: " <> show (abs (avgDistance - stepSize))
--   putStrLn "\nFirst 10 distances:"
--   mapM_ (putStrLn . show) (take 10 distances)