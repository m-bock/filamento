module Filamento.Core
  ( gCodeFromCmd,
    GCode,
    GCodeEnv (..),
    PrintState (..),
    initPrintState,
    defaultGCodeEnv,
    section,
    newline,
    comment,
    raw,
    rand,
    setUnits,
    Units (..),
    autoHome,
    setExtruderRelative,
    setExtruderAbsolute,
    setHotendOff,
    setBedOff,
    setFanOff,
    motorsOff,
    pause,
    extrudeXY,
    moveXY,
    moveXYZ,
    moveZ,
    moveToX,
    moveToY,
    moveToZ,
    moveToXY,
    moveToXYZ,
    moveX,
    moveY,
    extrudeX,
    extrudeY,
    extrudeZ,
    extrudeToX,
    extrudeToY,
    extrudeToZ,
    extrudeToXY,
    extrudeToXYZ,
    extrude,
    playTone,
    playTone_,
    setBedTemperature,
    setHotendTemperature,
    waitForBedTemperature,
    waitForHotendTemperature,
    setPositionXYZ,
    setPositionXY,
    getCurrentPosition,
  )
where

import Control.Monad.Writer
import qualified Data.Text as Text
import Data.Typeable (gcast1)
import Filamento.Types
import Linear (V2 (..), V3 (..))
import Marlin.Comment (gcodeToComment)
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Speed,
    extrudeSpeed :: Speed,
    moveSpeedFirstLayer :: Speed,
    extrudeSpeedFirstLayer :: Speed,
    bedTemperature :: Temperature,
    hotendTemperature :: Temperature,
    printSize :: V3 Double,
    parkingPosition :: Position3D,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: Position3D -> Position3D,
    retractLength :: Distance,
    retractSpeed :: Speed,
    zHop :: Double,
    sectionPath :: [Text]
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: Position3D,
    stdgen :: StdGen,
    filament :: [(Text, Double)]
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = pos3fromMm $ V3 145.50 94.00 1.56,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = speedFromMmPerSec 10000,
      extrudeSpeed = speedFromMmPerSec 2000,
      moveSpeedFirstLayer = speedFromMmPerSec 1000,
      extrudeSpeedFirstLayer = speedFromMmPerSec 800,
      bedTemperature = temperatureFromCelsius 60,
      hotendTemperature = temperatureFromCelsius 210,
      printSize = V3 225 225 280,
      parkingPosition = pos3fromMm $ V3 0 225 120,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = id,
      retractLength = 1,
      retractSpeed = speedFromMmPerMin 1800,
      zHop = 0.4,
      sectionPath = []
    }

newtype GCode a = GCode
  { runGCode :: StateT PrintState (ReaderT GCodeEnv (Writer [GCodeLine])) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader GCodeEnv,
      MonadState PrintState
    )

instance ToText (GCode a) where
  toText (GCode m) =
    evalStateT m initPrintState
      & (`runReaderT` defaultGCodeEnv)
      & execWriter
      & map gcodeLineToRaw
      & toText

-- | Random value for any type that implements `Random`
rand :: (Random a) => GCode a
rand = GCode $ state $ \st ->
  let (value, newGen) = random st.stdgen
      st' = st {stdgen = newGen}
   in (value, st')

-- updatePos :: V3 (Maybe Double) -> GCode ()
-- updatePos (V3 mx my mz) =
--   GCode $ do
--     st <- get
--     let (V3 x y z) = st.currentPosition
--         newPos = V3 (fromMaybe x mx) (fromMaybe y my) (fromMaybe z mz)
--     put $ st {currentPosition = newPos}

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  local (\env -> env {sectionPath = env.sectionPath <> [caption]}) gc

toTextSectionPath :: [Text] -> Text
toTextSectionPath [] = ""
toTextSectionPath path =
  "[ " <> Text.intercalate " / " path <> " ] "

newline :: GCode ()
newline = GCode $ tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Nothing}]

comment :: Text -> GCode ()
comment c = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = "", comment = Just c}]

raw :: Text -> Text -> GCode ()
raw extra comm = GCode $ do
  tell [GCodeLine {cmd = Nothing, rawExtra = extra, comment = Just comm}]

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode ()
setUnits u = gCodeFromCmd $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

autoHome :: GCode ()
autoHome = gCodeFromCmd $ GAutoHome gcodeDef {skipIfTrusted = False}

-------------------------------------------------------------------------------

setExtruderRelative :: GCode ()
setExtruderRelative = gCodeFromCmd MSetExtruderRelative

setExtruderAbsolute :: GCode ()
setExtruderAbsolute = gCodeFromCmd MSetExtruderAbsolute

setHotendOff :: GCode ()
setHotendOff = gCodeFromCmd MSetHotendOff

setBedOff :: GCode ()
setBedOff = gCodeFromCmd MSetBedOff

setFanOff :: GCode ()
setFanOff = gCodeFromCmd MSetFanOff

motorsOff :: GCode ()
motorsOff = gCodeFromCmd MMotorsOff

pause :: Int -> GCode ()
pause seconds = gCodeFromCmd $ GDwell (gcodeDef {seconds = Just seconds})

--------------------------------------------------------------------------------

operateTool :: Position3D -> Speed -> Distance -> GCode ()
operateTool v_ speed extr = do
  env <- ask

  modify \st -> st {currentPosition = v_}

  let V3 mx my mz = pos3toMm $ env.transpose v_

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just mx,
          y = Just my,
          z = Just mz,
          feedrate = Just $ round $ speedToMmPerSec speed,
          extrude = Just $ coerce $ distanceToMm extr
        }

--------------------------------------------------------------------------------

moveToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveToImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let V3 x y z = pos3toMm cur
      newX = fromMaybe x mx
      newY = fromMaybe y my
      newZ = fromMaybe z mz
  operateTool (pos3fromMm $ V3 newX newY newZ) speed 0

moveToXYZ :: Position3D -> GCode ()
moveToXYZ (pos3toMm -> V3 x y z) =
  moveToImpl (Just x) (Just y) (Just z)

moveToXY :: Position2D -> GCode ()
moveToXY (pos2ToMm -> V2 x y) =
  moveToImpl (Just x) (Just y) Nothing

moveToX :: Position -> GCode ()
moveToX (posToMm -> x) =
  moveToImpl (Just x) Nothing Nothing

moveToY :: Position -> GCode ()
moveToY (posToMm -> y) =
  moveToImpl Nothing (Just y) Nothing

moveToZ :: Position -> GCode ()
moveToZ (posToMm -> z) =
  moveToImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

moveImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
moveImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let v = pos3fromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  operateTool (v + cur) speed 0

moveXYZ :: Delta3D -> GCode ()
moveXYZ (delta3toMm -> V3 dx dy dz) =
  moveImpl (Just dx) (Just dy) (Just dz)

moveXY :: Delta2D -> GCode ()
moveXY (delta2toMm -> V2 dx dy) =
  moveImpl (Just dx) (Just dy) Nothing

moveX :: Delta -> GCode ()
moveX (deltaToMm -> x) =
  moveImpl (Just x) Nothing Nothing

moveY :: Delta -> GCode ()
moveY (deltaToMm -> y) =
  moveImpl Nothing (Just y) Nothing

moveZ :: Delta -> GCode ()
moveZ (deltaToMm -> z) =
  moveImpl Nothing Nothing (Just z)

--------------------------------------------------------------------------------

extrudeToImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeToImpl mx my mz = do
  speed <- getSpeed
  (pos3toMm -> V3 curX curY curZ) <- getCurrentPosition
  let v = pos3fromMm (V3 (fromMaybe curX mx) (fromMaybe curY my) (fromMaybe curZ mz))
  extr <- getExtrudeLength v
  operateTool v speed (distanceFromMm extr)

extrudeToXY :: Position2D -> GCode ()
extrudeToXY (pos2ToMm -> V2 dx dy) =
  extrudeToImpl (Just dx) (Just dy) Nothing

extrudeToXYZ :: Position3D -> GCode ()
extrudeToXYZ (pos3toMm -> V3 dx dy dz) =
  extrudeToImpl (Just dx) (Just dy) (Just dz)

extrudeToX :: Position -> GCode ()
extrudeToX (posToMm -> dx) =
  extrudeToImpl (Just dx) Nothing Nothing

extrudeToY :: Position -> GCode ()
extrudeToY (posToMm -> dy) =
  extrudeToImpl Nothing (Just dy) Nothing

extrudeToZ :: Position -> GCode ()
extrudeToZ (posToMm -> dz) =
  extrudeToImpl Nothing Nothing (Just dz)

-------------------------------------------------------------------------------

extrudeImpl :: Maybe Double -> Maybe Double -> Maybe Double -> GCode ()
extrudeImpl mx my mz = do
  speed <- getSpeed
  cur <- getCurrentPosition
  let v = pos3fromMm (V3 (fromMaybe 0 mx) (fromMaybe 0 my) (fromMaybe 0 mz))
  let v' = v + cur
  extr <- getExtrudeLength v'
  operateTool v' speed (distanceFromMm extr)

extrudeXYZ :: Delta3D -> GCode ()
extrudeXYZ (delta3toMm -> V3 dx dy dz) = do
  extrudeImpl (Just dx) (Just dy) (Just dz)

extrudeXY :: Delta2D -> GCode ()
extrudeXY (delta2toMm -> V2 dx dy) = do
  extrudeImpl (Just dx) (Just dy) Nothing

extrudeX :: Delta -> GCode ()
extrudeX (deltaToMm -> x) = extrudeXYZ (delta3fromMm $ V3 x 0 0)

extrudeY :: Delta -> GCode ()
extrudeY (deltaToMm -> y) = extrudeXYZ (delta3fromMm $ V3 0 y 0)

extrudeZ :: Delta -> GCode ()
extrudeZ (deltaToMm -> z) = extrudeXYZ (delta3fromMm $ V3 0 0 z)

-------------------------------------------------------------------------------

extrude :: Speed -> Distance -> GCode ()
extrude speed extr = do
  v <- getCurrentPosition
  operateTool v speed extr

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

getSpeed :: GCode Speed
getSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeLength :: Position3D -> GCode Double
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- get
  let lineLength = distanceToMm $ pos3distance st.currentPosition target
  pure (lineLength * extrudeMM)

getExtrudeMM :: GCode Double
getExtrudeMM = do
  env <- ask
  let vPerMm = env.layerHeight * env.lineWidth
      aFil = pi * (env.filamentDia ^ 2) / 4
  pure (vPerMm / aFil)

isFirstLayers :: GCode Bool
isFirstLayers = do
  st <- get
  let (V3 _ _ z) = pos3toMm st.currentPosition
  pure (z <= 0.4)

getExtrudeSpeed :: GCode Speed
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure
    $ if b
      then env.extrudeSpeedFirstLayer
      else env.extrudeSpeed

stateUpdateExtrude :: Double -> PrintState -> PrintState
stateUpdateExtrude len st = case st.filament of
  [] -> st {filament = [("default", len)]}
  (color, used) : xs -> st {filament = (color, used + len) : xs}

gCodeFromCmd :: GCodeCmd -> GCode ()
gCodeFromCmd cmd = do
  case cmd of
    GMillimeterUnits -> pure ()
    GInchUnits -> pure ()
    GLinearMove opt -> pure ()
    -- updatePos (V3 opt.x opt.y opt.z)
    -- forM_ opt.extrude $ \e -> modify (stateUpdateExtrude e)
    GAutoHome _ -> pure ()
    -- env <- ask
    -- updatePos (fmap Just env.autoHomePosition)
    GSetPosition opt -> pure ()
    -- updatePos (V3 opt.x opt.y opt.z)
    -- setExtruded opt.extrude
    MSetBedTemperature _ -> pure ()
    MWaitForBedTemperature _ -> pure ()
    MSSetHotendTemperature _ -> pure ()
    MWaitForHotendTemperature _ -> pure ()
    MSetExtruderRelative -> pure ()
    MSetExtruderAbsolute -> pure ()
    MSetHotendOff -> pure ()
    MSetBedOff -> pure ()
    MSetFanOff -> pure ()
    MMotorsOff -> pure ()
    MPlayTone _ -> pure ()
    GDwell _ -> pure ()

  let cmd' = case cmd of
        GLinearMove opt -> GLinearMove opt
        _ -> cmd

  env <- ask

  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = Just cmd',
        rawExtra = "",
        comment = Just (toTextSectionPath env.sectionPath <> gcodeToComment cmd)
      }

playTone :: Frequency -> Duration -> GCode ()
playTone freq dur =
  gCodeFromCmd
    $ MPlayTone
      gcodeDef
        { frequency = Just $ round $ frequencyToHz freq,
          milliseconds = Just $ round $ durationToMs dur
        }

playTone_ :: GCode ()
playTone_ = playTone (frequencyFromHz 2600) (durationFromMs 1)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature degrees = do
  gCodeFromCmd
    $ MSetBedTemperature
      gcodeDef
        { degrees = Just $ round $ temperatureToCelsius degrees
        }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature temp = do
  gCodeFromCmd
    $ MSSetHotendTemperature
      gcodeDef
        { degrees = Just $ round $ temperatureToCelsius temp
        }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature temp = do
  gCodeFromCmd
    $ MWaitForBedTemperature
      gcodeDef
        { degrees = Just $ round $ temperatureToCelsius temp
        }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature temp = do
  gCodeFromCmd
    $ MWaitForHotendTemperature
      gcodeDef
        { degrees = Just $ round $ temperatureToCelsius temp
        }

setPositionXYZ :: V3 Double -> GCode ()
setPositionXYZ (V3 x y z) = do
  gCodeFromCmd
    $ GSetPosition
      gcodeDef
        { x = Just x,
          y = Just y,
          z = Just z
        }

setPositionXY :: V2 Double -> GCode ()
setPositionXY (V2 x y) = do
  gCodeFromCmd
    $ GSetPosition
      gcodeDef
        { x = Just x,
          y = Just y
        }

changeColor :: Text -> GCode ()
changeColor colorName = do
  comment ("Change color to: " <> colorName)
  modify \st -> st {filament = (colorName, 0) : st.filament}

data FilamentStrategy = FilamentChange | PreparedFilament
  deriving (Show, Eq)

getCurrentPosition :: GCode Position3D
getCurrentPosition = do
  st <- get
  pure st.currentPosition