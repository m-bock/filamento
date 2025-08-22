module Filamento.Filament
  ( printFilament,
    FilamentConfig (..),
    printFilament_,
  )
where

import Data.List (elemIndex, nub)
import qualified Data.Text as Text
import Filamento
import Filamento.Debug
import Filamento.Math (itemsWithNext, linspace2ByStep, linspaceByStep)
import GHC.List ((!!))
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
      splitLinesEvery = Just $ fromMm 6,
      spiralCenter = v3PosFromMm 110 110 0,
      spiralRadius = fromMm 95,
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

      rectLines = [(frontRight, backRight), (backRight, backLeft), (backLeft, frontLeft), (frontLeft, frontRight)]

  forM_ rectLines $ \(p1, p2) -> do
    printLineFilament config (line2FromPoints p1 p2)

printLineFilament :: FilamentConfig -> Line2D -> GCode ()
printLineFilament config line = do
  let start = line2GetStart line

  do
    st <- gcodeStateGet
    let V3 curX curY _ = st.currentPosition
        arrivalLine = line2FromPoints (V2 curX curY) start
        arrivalLines = getLines config arrivalLine

        isFar = getDistance start (V2 curX curY) > 10

    if isFar
      then withRetract $ withZHop $ do
        forM_ arrivalLines $ \lineSeq -> do
          moveTo (line2GetEnd lineSeq)
      else forM_ arrivalLines $ \lineSeq -> do
        moveTo (line2GetEnd lineSeq)

  let actualLines = getLines config line

  forM_ actualLines $ \lineSeq -> do
    moveTo (line2GetStart lineSeq)
    extrudeTo (line2GetEnd lineSeq)

getLines :: FilamentConfig -> Line2D -> [Line2D]
getLines config line = do
  let start = line2GetStart line
      end = line2GetEnd line
      pts = case config.splitLinesEvery of
        Nothing -> [start, end]
        Just val -> case linspace2ByStep start end val deltaRound of
          [] -> [start, end]
          vals -> vals

  map (\(p1, p2) -> line2FromPoints p1 p2) (itemsWithNext pts)

getLength :: FilamentConfig -> Proportion -> ProfileType -> Delta -> Delta
getLength config prop profile len =
  let frac = case profile of
        Hill -> 1 - (toFraction prop)
        Valley -> toFraction prop

      ov = config.overlap
      ramp = scale (frac + frac) ov
   in ramp + (-ov) + len + (-ov) + ramp

getWidth :: Delta -> Proportion -> Delta
getWidth filamentDia propY =
  let y = toFraction propY
      w = sqrt (1 - 4 * (y - 0.5) ^ 2)
   in scale w filamentDia

printFilamentSegment :: FilamentConfig -> (Rect2D -> GCode ()) -> FilamentSegment -> GCode ()
printFilamentSegment config printPlane profile = do
  let rectCenter = V2 0 (addDelta profile.pos (scale @Double 0.5 profile.depth))

  resetLayers

  local (\env -> env {zHop = scale @Double 1.2 config.filamentDia}) do
    withRetract $ withZHop $ moveTo (V2 0 profile.pos)

  let layerHeight = fromMm 0.2 :: Delta

  moveToZ (posFromDelta layerHeight)
  incLayers

  whileSketchZ_
    -- ( do
    --     st <- gcodeStateGet
    --     pure (st.currentLayer == 0)
    -- )
    do
      st <- gcodeStateGet

      if st.currentLayer == 1
        then do
          setFanOff
        else do
          setFanSpeedFull

      prop <- getZProgress

      let rectWidth = max 0 $ getWidth config.filamentDia prop
          rectLength = getLength config prop profile.profileType profile.depth
          rectSize = V2 rectWidth rectLength
          rect = rect2FromCenterSize rectCenter rectSize

      comment ("prop = " <> Text.pack (printf "%.3f" (toFraction prop)))
      section (Text.pack $ printf "%.3f" (toFraction prop)) do
        local (\env -> env {layerHeight = layerHeight}) do
          printPlane rect

      moveByZ layerHeight
      incLayers

ironFinish :: FilamentConfig -> [FilamentSection] -> GCode ()
ironFinish config secs = section "ironFinish" do
  env <- ask

  moveByZ (-env.layerHeight)
  decLayers

  outOf <- getLayerProgress
  let prop = outOfToProportion (outOf |> "outOf") |> "prop"

  let width = getWidth config.filamentDia prop |> "width"
      depth = case viaNonEmpty last secs of
        Just val -> val.endPosMm |> "depth"
        Nothing -> 0 |> "depth"

      startX = subDelta 0 $ scale @Double 0.5 width
      endX = addDelta 0 $ scale @Double 0.5 width
      step = env.lineWidth / 2

      xs = linspaceByStep startX endX step deltaRound

  forM_ (zip xs [0 ..]) $ \(x, i) -> do
    let vec1 = V2 x 0
        vec2 = V2 x depth
        line =
          if even i
            then line2FromPoints vec2 vec1
            else line2FromPoints vec1 vec2

        lineSegs = getLines config line

    forM_ lineSegs $ \lineSeq -> do
      moveTo (line2GetStart lineSeq)
      moveTo (line2GetEnd lineSeq)

printFilamentChain :: (FilamentSegment -> GCode ()) -> [FilamentSection] -> GCode ()
printFilamentChain innerPrintSegment secs = do
  let secsWithIndex = zip [0 ..] secs
      evens = filter (even . fst) secsWithIndex
      odds = filter (odd . fst) secsWithIndex

  let colors = map (\x -> x.color) secs & nub

  forM_ [(Hill, evens), (Valley, odds)] $ \(profileType, items) -> do
    local (\env -> env {transpose = id}) do
      filamentChange
      printTestStripes

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
            firstLayerHeight = fromMm 0.2,
            hotendTemperature = fromCelsius 205,
            bedTemperature = fromCelsius 65,
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
      ironFinish config secs

printFilament_ :: [FilamentSection] -> GCode ()
printFilament_ = printFilament id

-------------------------------------------------------------------------------

getLayerHeight :: FilamentConfig -> Count -> Delta
getLayerHeight config layerIndex =
  if layerIndex == fromInt 0
    then 0.2
    else 0.2