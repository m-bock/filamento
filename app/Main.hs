module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Env
import Filamento
import Filamento.Filament
import Filamento.Math
import qualified Filamento.Octo as Octo
import GHC.Conc
import Linear
import Network.HTTP.Client
import Network.URI (URI, parseURI)
import Octo.API (OctoHttpCfg (..))
import Relude

printStripesAlongX :: Square2D -> Count -> [Line2D]
printStripesAlongX square count = do
  let V2 x1 y1 = square2GetMinCorner square
      V2 x2 y2 = square2GetMaxCorner square

      ys = linspace y1 y2 count

  map (\y -> line2FromPoints (V2 x1 y) (V2 x2 y)) ys

printStripesAlongY :: Position -> Rect2D -> Count -> [Line2D]
printStripesAlongY z rect count = do
  let V2 x1 y1 = rect2GetMinCorner rect
      V2 x2 y2 = rect2GetMaxCorner rect

      xs = linspace x1 x2 count

      shift = 0 -- z * 8
  map (\x -> line2FromPoints (V2 x (y1 + shift)) (V2 x (y2 + shift))) xs

printPurgeTower :: Rect2D -> Count -> GCode ()
printPurgeTower rect count = do
  st <- gcodeStateGet
  let V3 _ _ curZ = st.currentPosition
  let linesToPrint = printStripesAlongY curZ rect count

  forM_ (zip [0 ..] linesToPrint) $ \(i, line) -> do
    let (p1, p2) =
          if odd i
            then
              (line2GetStart line, line2GetEnd line)
            else
              (line2GetEnd line, line2GetStart line)

    if i == 0
      then do
        withRetract $ withZHop $ moveTo p1
      else do
        moveTo p1

    st <- gcodeStateGet
    let V2 x y = p2
        V3 _ _ z = st.currentPosition

    withFixedRegister (V3 x y z) do
      let y' = y + fromMm (st.flowCorrection * 5)
      extrudeTo (V2 x y')

data Colors = Colors
  { red :: Text,
    yellow :: Text
  }

colors :: Colors
colors = Colors {red = "red", yellow = "yellow"}

allColors :: NonEmpty Text
allColors = colors.red :| [colors.yellow]

nextColor :: Text -> Text
nextColor c = case c of
  "red" -> "yellow"
  "yellow" -> "red"
  _ -> c

prevColor :: Text -> Text
prevColor c = case c of
  "red" -> "yellow"
  "yellow" -> "red"
  _ -> c

printSketch :: GCode ()
printSketch = withSketchTranspose do
  resetLayers
  printLayers_ do
    st <- gcodeStateGet
    hook ("layer " <> show st.currentLayer) do
      if st.currentLayer == 1
        then do
          setFanOff
        else do
          setFanSpeedFull

      let rect = rect2FromCenterSize (v2PosFromMm 50 50) (fromMm $ V2 50 30)
          (p1, p2, p3, p4) = rect2GetPoints rect
      withColors
        \color -> do
          color colors.red do
            printPurgeTower (rect2FromCenterSize (v2PosFromMm (-20) (-55)) (fromMm $ V2 12.5 30)) (fromInt 20)

          color colors.yellow do
            printPurgeTower (rect2FromCenterSize (v2PosFromMm (-6.5) (-55)) (fromMm $ V2 12.5 30)) (fromInt 20)

          color colors.red do
            withRetract $ withZHop $ moveTo p1
            extrudeTo p2

          color colors.yellow do
            withRetract $ withZHop $ moveTo p2
            extrudeTo p3

          color colors.red do
            withRetract $ withZHop $ moveTo p3
            extrudeTo p4

          color colors.yellow do
            withRetract $ withZHop $ moveTo p4
            extrudeTo p1

      env <- ask
      st <- gcodeStateGet
      pure ()

printAll :: GCode ()
printAll = do
  env <- ask

  initPrinter do
    -- printSketchFrame

    -- moveToZ (fromMm 0.2)
    -- testCode

    ret <- hook "start" do
      env <- ask
      st <- gcodeStateGet
      liftIO $ getFilamentDef env st printSketch

    -- filamentChange

    dia <- hook "printFilament" do
      resetLayers
      printFilament
        (\cfg -> cfg {disableSpiral = False})
        ( case (viaNonEmpty head ret, viaNonEmpty last ret) of
            (Just fi, Just la) ->
              [FilamentSection (prevColor fi.color) (fromMm (70))]
                ++ map
                  (\v -> v {endPosMm = 70 + (v.endPosMm * 1.00)})
                  ret
                ++ [FilamentSection (nextColor la.color) (fromMm (70 + 150) + la.endPosMm)]
            _ -> []
        )

    filamentChange

    hook "printSketch" do
      local (\env -> env {filamentDia = dia} :: GCodeEnv) do
        printSketch

mainGen :: IO ()
mainGen = do
  envVars <- parseEnvVars

  preHook <-
    fold
      [ mkPreHookUserInput
      ]

  postHook <-
    fold
      [ mkPostHookFileAppender "out/myprint.gcode",
        mkPostHookOcto envVars
      ]

  _ <-
    gcodeRun
      printAll
      ( gcodeEnvDefault
          { lineWidth = fromMm 0.6,
            layerHeight = fromMm 0.3,
            hotendTemperature = fromCelsius 205,
            bedTemperature = fromCelsius 65,
            retractLength = fromMm 1.5,
            colors = allColors,
            sketchSize = fromMm $ V3 100 100 10,
            parkingPosition = v3PosFromMm 0 0 20,
            preHook,
            postHook
          }
      )
      (gcodeStateInit gcodeEnvDefault)
  pure ()

mainTry :: IO ()
mainTry = do
  pure ()

main :: IO ()
main = mainGen