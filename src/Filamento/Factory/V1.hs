module Filamento.Factory.V1
  ( printFilament,
    FilamentConfig (..),
    printFilament_,
  )
where

import Data.Foldable (Foldable (maximum))
import Data.List (elemIndex, nub)
import qualified Data.Text as Text
import Filamento
import Filamento.Math (itemsWithNext, linspace2ByStep, linspaceByStep)
import Linear
import Relude
import Text.Printf (printf)

data ProfileType = Hill | Valley deriving (Show)

-------------------------------------------------------------------------------

data FilamentConfig = FilamentConfig
  { overlap :: Delta,
    filamentDia :: Delta,
    splitLinesEvery :: Maybe Delta,
    spiralCenter :: V3 Position,
    spiralRadius :: Delta,
    spiralConstant :: Delta,
    disableSpiral :: Bool
  }

data FilamentSegment = FilamentSegment
  { profileType :: ProfileType,
    pos :: Position,
    depth :: Delta
  }

-------------------------------------------------------------------------------

filamentConfigDefault :: FilamentConfig
filamentConfigDefault =
  FilamentConfig
    { overlap = fromMm 2,
      filamentDia = fromMm 1.75,
      splitLinesEvery = Just $ fromMm 3,
      spiralCenter = v3PosFromMm 110 110 0,
      spiralRadius = fromMm 100,
      spiralConstant = fromMm (-0.5),
      disableSpiral = False
    }

-------------------------------------------------------------------------------

printSolidRect :: (Line2D -> GCode ()) -> (Rect2D -> GCode ()) -> Rect2D -> GCode ()
printSolidRect innerPrintLine innerPrintRect frame = do
  let V2 w _ = rect2GetSize frame

  env <- ask
  let V2 frameX2 frameY2 = rect2GetMaxCorner frame
      V2 frameX1 frameY1 = rect2GetMinCorner frame
      spansX = getSpans env.lineWidth frameX1 frameX2
      spansY = getSpans env.lineWidth frameY1 frameY2
      spans = reverse $ zip spansX spansY

  section ("w = " <> Text.pack (printf "%.3f" (toMm w))) $ do
    forM_ spans $ \((startX, endX), (startY, endY)) -> do
      let start = V2 startX startY
          end = V2 endX endY
      if (startX == endX) || (startY == endY)
        then
          innerPrintLine (line2FromPoints end start)
        else
          innerPrintRect (rect2FromCorners start end)
  where
    getSpans lineWidth start end =
      let ticks = linspaceByStep start end lineWidth deltaFloor
          ticksCentered = map (\(pos1, pos2) -> posMiddle pos1 pos2) $ itemsWithNext ticks
          count = ceiling @Double (fromIntegral (length ticksCentered) / 2)
       in take count $ zip ticksCentered (reverse ticksCentered)

printRectFilament :: FilamentConfig -> Rect2D -> GCode ()
printRectFilament config rect = do
  let (frontLeft, frontRight, backRight, backLeft) = rect2GetPoints rect

      rectLines = [(frontLeft, frontRight), (frontRight, backRight), (backRight, backLeft), (backLeft, frontLeft)]

  forM_ rectLines $ \(p1, p2) -> do
    printLineFilament config (line2FromPoints p1 p2)

printLineFilament :: FilamentConfig -> Line2D -> GCode ()
printLineFilament config line = do
  let start = line2GetStart line
      end = line2GetEnd line
      pts = case config.splitLinesEvery of
        Nothing -> [start, end]
        Just val -> case linspace2ByStep start end val deltaRound of
          [] -> [start, end]
          vals -> vals

  forM_ (itemsWithNext pts) $ \(p1, p2) -> do
    moveTo p1
    extrudeTo p2

getLength :: FilamentConfig -> Proportion -> ProfileType -> Delta -> Delta
getLength config prop profile len =
  let frac = case profile of
        Hill -> 1 - (toFraction prop)
        Valley -> toFraction prop

      ov = config.overlap
      ramp = scale (frac + frac) ov
   in ramp + (-ov) + len + (-ov) + ramp

getWidth :: FilamentConfig -> Proportion -> Delta
getWidth config propY =
  let y = toFraction propY
      w = sqrt (1 - 4 * (y - 0.5) ^ 2)
   in scale w config.filamentDia

printFilamentSegment :: FilamentConfig -> (Rect2D -> GCode ()) -> FilamentSegment -> GCode ()
printFilamentSegment config printPlane profile = do
  let rectCenter = addDelta (V2 0 profile.pos) (V2 20 (scale 0.5 profile.depth))

  resetLayers

  local (\env -> env {zHop = scale 1.2 config.filamentDia}) do
    withRetract $ withZHop $ moveTo rectCenter

  moveToZ (fromMm 0.0)

  whileSketchZ do
    oldProp <- getZProgress

    let z =
          let isInner = fromFraction 0.25 < oldProp && oldProp < fromFraction 0.75
           in if isInner then fromMm 0.2 else fromMm 0.1

    moveByZ z

    prop <- getZProgress

    let rectWidth = max 0 $ getWidth config prop
        rectLength = getLength config prop profile.profileType profile.depth
        rectSize = V2 rectWidth rectLength
        rect = rect2FromCenterSize rectCenter rectSize

    comment ("prop = " <> Text.pack (printf "%.3f" (toFraction prop)))
    section (Text.pack $ printf "%.3f" (toFraction prop)) do
      local (\env -> env {layerHeight = z}) do
        printPlane rect

printFilamentChain :: (FilamentSegment -> GCode ()) -> [FilamentSection] -> GCode ()
printFilamentChain innerPrintSegment secs = do
  let secsWithIndex = zip [0 ..] secs
      evens = filter (even . fst) secsWithIndex
      odds = filter (odd . fst) secsWithIndex

  let colors = map (\x -> x.color) secs & nub

  forM_ [(Hill, evens), (Valley, odds)] $ \(profileType, items) -> do
    section (show profileType) do
      forM_ items $ \(i, sec) -> do
        section (show i) do
          let secPrev = secs !!? (i - 1)
              pos = maybe 0 (\x -> x.endPosMm) secPrev
              end = sec.endPosMm
              depth = getDelta pos end
              colorIndex = fromMaybe 0 (elemIndex sec.color colors)
              segment = FilamentSegment profileType pos depth
          setTool colorIndex

          innerPrintSegment segment
      filamentChange

translateSpiral :: FilamentConfig -> V3 Position -> V3 Position
translateSpiral config pos = v3PosFromMm x' y' z
  where
    V3 centerX centerY _ = toMm config.spiralCenter
    V3 x y z = toMm pos

    arcLength = y
    spiralConstant = toMm config.spiralConstant
    baseRadius = toMm config.spiralRadius

    averageRadius = baseRadius + spiralConstant * (arcLength / (2 * baseRadius))
    angle' = arcLength / averageRadius

    spiralRadius = baseRadius + spiralConstant * angle'

    m = 3 * (pi / 2)
    finalAngle = m + angle'

    x' = centerX + (spiralRadius + x) * cos finalAngle
    y' = centerY + (spiralRadius + x) * sin finalAngle

printFilament :: (FilamentConfig -> FilamentConfig) -> [FilamentSection] -> GCode ()
printFilament mkConfig secs = section "filament" do
  let config = mkConfig filamentConfigDefault
  local
    ( \env ->
        env
          { sketchSize = fromMm $ V3 10 10 (toMm config.filamentDia),
            lineWidth = fromMm 0.4,
            layerHeight = fromMm 0.2,
            hotendTemperature = fromCelsius 205,
            bedTemperature = fromCelsius 65,
            moveSpeed = fromMmPerSec 2000,
            extrudeSpeed = fromMmPerSec 2500,
            retractLength = fromMm 1.5,
            transpose = if config.disableSpiral then id else translateSpiral config
          }
    )
    do
      printFilamentChain
        ( printFilamentSegment
            config
            ( printSolidRect
                (printLineFilament config)
                (printRectFilament config)
            )
        )
        secs

printFilament_ :: [FilamentSection] -> GCode ()
printFilament_ = printFilament id