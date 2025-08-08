module Filamento
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
    Temperature (..),
    Frequency (..),
    Duration (..),
    setPositionXYZ,
    setPositionXY,
  )
where

import Control.Monad.Writer
import Control.Newtype
import Data.Aeson.Encoding (double)
import Filamento.Conversions
import Filamento.Types.Distance
import Filamento.Types.Duration
import Filamento.Types.Frequency
import Filamento.Types.Position
import Filamento.Types.Speed
import Filamento.Types.Temperature
import Linear (V2 (..), V3 (..))
import Linear.Metric (Metric (..))
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
    parkingPosition :: V3 Double,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: V3 Double -> V3 Double,
    retractLength :: Double,
    retractSpeed :: Speed,
    zHop :: Double
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: V3 Double,
    stdgen :: StdGen,
    filament :: [(Text, Double)]
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = V3 145.50 94.00 1.56,
      stdgen = mkStdGen 0,
      currentLayer = 0,
      filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = from @MMPerMin 10000,
      extrudeSpeed = from @MMPerMin 2000,
      moveSpeedFirstLayer = from @MMPerMin 1000,
      extrudeSpeedFirstLayer = from @MMPerMin 800,
      bedTemperature = from @Celsius 60,
      hotendTemperature = from @Celsius 210,
      printSize = V3 225 225 280,
      parkingPosition = V3 0 225 120,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = id,
      retractLength = 1,
      retractSpeed = from @MMPerMin 1800,
      zHop = 0.4
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

updatePos :: V3 (Maybe Double) -> GCode ()
updatePos (V3 mx my mz) = GCode $ do
  st <- get
  let (V3 x y z) = st.currentPosition
      newPos = V3 (fromMaybe x mx) (fromMaybe y my) (fromMaybe z mz)
  put $ st {currentPosition = newPos}

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  newline
  comment caption
  ret <- gc
  newline
  pure ret

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

operateTool :: V3 Double -> Speed -> Double -> GCode ()
operateTool v_ speed extr = do
  env <- ask
  let V3 mx my mz = env.transpose v_

  gCodeFromCmd
    $ GLinearMove
      gcodeDef
        { x = Just mx,
          y = Just my,
          z = Just mz,
          feedrate = Just $ doubleToInt $ coerce $ to @MMPerMin speed,
          extrude = Just extr
        }

--------------------------------------------------------------------------------

moveTo :: Position -> GCode ()
moveTo pos = do
  speed <- getSpeed
  operateTool (coerce $ to @(V3 MM) pos) speed 0

moveToXYZ :: V3 Double -> GCode ()
moveToXYZ v = do
  speed <- getSpeed
  operateTool v speed 0

moveToXY :: V2 Double -> GCode ()
moveToXY (V2 x y) = moveToXYZ (V3 x y 0)

moveToX :: Double -> GCode ()
moveToX x = moveToXYZ (V3 x 0 0)

moveToY :: Double -> GCode ()
moveToY y = moveToXYZ (V3 0 y 0)

moveToZ :: Double -> GCode ()
moveToZ z = moveToXYZ (V3 0 0 z)

--------------------------------------------------------------------------------

move :: Distance -> GCode ()
move = undefined

moveXYZ :: V3 Double -> GCode ()
moveXYZ v = do
  v' <- getCurrentPosition
  moveToXYZ (v + v')

moveXY :: V2 Double -> GCode ()
moveXY (V2 x y) = moveXYZ (V3 x y 0)

moveX :: Double -> GCode ()
moveX x = moveXYZ (V3 x 0 0)

moveY :: Double -> GCode ()
moveY y = moveXYZ (V3 0 y 0)

moveZ :: Double -> GCode ()
moveZ z = moveXYZ (V3 0 0 z)

--------------------------------------------------------------------------------

extrudeToXYZ :: V3 Double -> GCode ()
extrudeToXYZ v = do
  speed <- getExtrudeSpeed
  extrudeLength <- getExtrudeLength v
  operateTool v speed extrudeLength

extrudeToXY :: V2 Double -> GCode ()
extrudeToXY (V2 x y) = extrudeToXYZ (V3 x y 0)

extrudeToX :: Double -> GCode ()
extrudeToX x = extrudeToXYZ (V3 x 0 0)

extrudeToY :: Double -> GCode ()
extrudeToY y = extrudeToXYZ (V3 0 y 0)

extrudeToZ :: Double -> GCode ()
extrudeToZ z = extrudeToXYZ (V3 0 0 z)

-------------------------------------------------------------------------------

extrudeXYZ :: V3 Double -> GCode ()
extrudeXYZ v = do
  v' <- getCurrentPosition
  extrudeToXYZ (v + v')

extrudeXY :: V2 Double -> GCode ()
extrudeXY (V2 x y) = extrudeXYZ (V3 x y 0)

extrudeX :: Double -> GCode ()
extrudeX x = extrudeXYZ (V3 x 0 0)

extrudeY :: Double -> GCode ()
extrudeY y = extrudeXYZ (V3 0 y 0)

extrudeZ :: Double -> GCode ()
extrudeZ z = extrudeXYZ (V3 0 0 z)

-------------------------------------------------------------------------------

extrude :: Speed -> Double -> GCode ()
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

getExtrudeLength :: V3 Double -> GCode Double
getExtrudeLength target = do
  extrudeMM <- getExtrudeMM
  st <- get
  let lineLength = distance st.currentPosition target
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
  let (V3 _ _ z) = st.currentPosition
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
    GLinearMove opt -> do
      updatePos (V3 opt.x opt.y opt.z)
      forM_ opt.extrude $ \e -> modify (stateUpdateExtrude e)
    GAutoHome _ -> do
      env <- ask
      updatePos (fmap Just env.autoHomePosition)
    GSetPosition opt -> do
      updatePos (V3 opt.x opt.y opt.z)
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

  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = Just cmd',
        rawExtra = "",
        comment = Just (gcodeToComment cmd)
      }

playTone :: Frequency -> Duration -> GCode ()
playTone freq dur =
  gCodeFromCmd
    $ MPlayTone
      gcodeDef
        { frequency = Just $ doubleToInt $ coerce $ to @Hz freq,
          milliseconds = Just $ doubleToInt $ coerce $ to @MS dur
        }

doubleToInt :: Double -> Int
doubleToInt = round

playTone_ :: GCode ()
playTone_ = playTone (Frequency 2600) (Duration 1)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature degrees = do
  gCodeFromCmd
    $ MSetBedTemperature
      gcodeDef
        { degrees = Just $ doubleToInt $ coerce $ to @Celsius degrees
        }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature temp = do
  gCodeFromCmd
    $ MSSetHotendTemperature
      gcodeDef
        { degrees = Just $ doubleToInt $ coerce $ to @Celsius temp
        }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature temp = do
  gCodeFromCmd
    $ MWaitForBedTemperature
      gcodeDef
        { degrees = Just $ doubleToInt $ coerce $ to @Celsius temp
        }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature temp = do
  gCodeFromCmd
    $ MWaitForHotendTemperature
      gcodeDef
        { degrees = Just $ doubleToInt $ coerce $ to @Celsius temp
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

getCurrentPosition :: GCode (V3 Double)
getCurrentPosition = do
  st <- get
  pure st.currentPosition