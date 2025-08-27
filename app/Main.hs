module Main where

import qualified Configuration.Dotenv as Dotenv
import Control.Concurrent (threadDelay)
import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString (findIndex)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Env
import Filamento
import Filamento.Filament
import Filamento.Math
import GHC.Conc
import Graphics.Gnuplot.Simple
import Linear
import Relude

data EnvVars = EnvVars
  { dryRun :: Bool
  }

parseEnvVars :: IO EnvVars
parseEnvVars =
  Env.parse (Env.header "envparse example")
    $ EnvVars
    <$> switch "DRY" (help "Dry run")

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
    env.emitGCode $ "layer " <> show st.currentLayer

printAll :: GCode ()
printAll = do
  env <- ask

  initPrinter do
    -- printSketchFrame

    -- moveToZ (fromMm 0.2)
    -- testCode

    env.emitGCode "start"

    env <- ask
    st <- gcodeStateGet
    ret <- liftIO $ getFilamentDef env st printSketch

    -- filamentChange

    resetLayers
    dia <-
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

    env.emitGCode "printFilament"

    filamentChange

    local (\env -> env {filamentDia = dia} :: GCodeEnv) do
      printSketch

  env.emitGCode "final"

data UserInput = UserInput
  { flowCorrection :: Maybe Double
  }
  deriving (Show)

-- e.g. "c.7", "c-9.15", "c-.15"

data UserInputRaw = UserInputRaw
  { cmds :: Map Char ArgVal
  }
  deriving (Show)

data ArgVal = ArgValDouble Double
  deriving (Show)

userInputParseArgVal :: Text -> Either Text ArgVal
userInputParseArgVal str = case parseDoubleWithShorthand str of
  Just d -> Right $ ArgValDouble d
  Nothing -> Left $ "Invalid double: " <> str

userInputParseArg :: Text -> Either Text (Char, ArgVal)
userInputParseArg str = case toString str of
  'c' : rest -> do
    argVal <- userInputParseArgVal (toText rest)
    pure ('c', argVal)
  _ -> Left $ "Invalid argument: " <> str

userInputParseRaw :: Text -> Either Text UserInputRaw
userInputParseRaw str = do
  let parts = T.splitOn " " str & filter (not . T.null) :: [Text]
  ret <- forM parts $ \part -> do
    case userInputParseArg part of
      Right (c, argVal) -> pure (c, argVal)
      Left err -> Left err
  pure $ UserInputRaw {cmds = Map.fromList ret}

userInputFromRaw :: UserInputRaw -> Either Text UserInput
userInputFromRaw (UserInputRaw {cmds}) = do
  flowCorrection <- case Map.lookup 'c' cmds of
    Nothing -> Right Nothing
    Just (ArgValDouble d) -> Right (Just d)
    _ -> Left "No flow correction"
  pure $ UserInput {flowCorrection = flowCorrection}

-- Accepts ".23" -> 0.23 and "-.13" -> -0.13 as well.
parseDoubleWithShorthand :: Text -> Maybe Double
parseDoubleWithShorthand =
  readMaybe . normalize . toString
  where
    normalize ('.' : xs) = '0' : '.' : xs
    normalize ('-' : '.' : xs) = '-' : '0' : '.' : xs
    normalize s = s

parseUserInput :: Text -> Either Text UserInput
parseUserInput str = do
  raw <- userInputParseRaw str
  userInputFromRaw raw

mainGen :: IO ()
mainGen = do
  Dotenv.loadFile Dotenv.defaultConfig

  let gCodeFile = "out/myprint.gcode"
  writeFileText gCodeFile ""

  envVars <- parseEnvVars
  refCounter <- newIORef 0

  let emitGCode tag = do
        putTextLn $ "emitGCode " <> tag
        gcl <- readAndDropGCodeLines
        appendFileText gCodeFile $ toText gcl
        unless envVars.dryRun do
          userInput <- getLine
          let res = parseUserInput userInput
          putTextLn (show res)

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
            emitGCode
          }
      )
      (gcodeStateInit gcodeEnvDefault)
  pure ()

-- sendGCode :: [Text] -> IO ()
-- sendGCode cmds = runReq defaultHttpConfig $ do
--   liftIO $ putStrLn $ "Sending G-code: " <> T.unpack (T.intercalate ", " cmds)
--   _ <-
--     req
--       POST
--       (host /: "printer" /: "command")
--       (ReqBodyJson $ object ["commands" .= cmds])
--       ignoreResponse
--       (headers <> octoPort)
--   pure ()

mainPlot :: IO ()
mainPlot = do
  pure ()

-- let out = toMm <$> getLayerHeights
-- putStrLn $ show out
-- plotList [] out
-- threadDelay 5000000 -- wait 5s

mainTry :: IO ()
mainTry = do
  putStrLn "Hello, World!"

main :: IO ()
main = mainGen