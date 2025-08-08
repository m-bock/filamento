module Filamento (
  gCodeFromCmd,
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
  extrude,
  moveXY,
  moveXYZ,
  moveZ,
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
import Linear (V2 (..), V3 (..))
import Linear.Metric (Metric (..))
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Int
  , extrudeSpeed :: Int
  , moveSpeedFirstLayer :: Int
  , extrudeSpeedFirstLayer :: Int
  , bedTemperature :: Temperature
  , hotendTemperature :: Temperature
  , printSize :: V3 Double
  , parkingPosition :: V3 Double
  , autoHomePosition :: V3 Double
  , layerHeight :: Double
  , firstLayerHeight :: Double
  , lineWidth :: Double
  , filamentDia :: Double
  , transpose :: V3 Double -> V3 Double
  , retractLength :: Double
  , retractSpeed :: Int
  , zHop :: Double
  }

data PrintState = PrintState
  { currentLayer :: Int
  , currentPosition :: V3 Double
  , stdgen :: StdGen
  , filament :: [(Text, Double)]
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = V3 145.50 94.00 1.56
    , stdgen = mkStdGen 0
    , currentLayer = 0
    , filament = []
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = 10000
    , extrudeSpeed = 2000
    , moveSpeedFirstLayer = 1000
    , extrudeSpeedFirstLayer = 800
    , bedTemperature = Temperature 60
    , hotendTemperature = Temperature 210
    , printSize = V3 225 225 280
    , parkingPosition = V3 0 225 120
    , autoHomePosition = V3 145.50 94.00 1.56
    , layerHeight = 0.4
    , firstLayerHeight = 0.2
    , lineWidth = 0.4
    , filamentDia = 1.75
    , transpose = id
    , retractLength = 1
    , retractSpeed = 1800
    , zHop = 0.4
    }

newtype GCode a = GCode
  { runGCode :: StateT PrintState (ReaderT GCodeEnv (Writer [GCodeLine])) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader GCodeEnv
    , MonadState PrintState
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
      st' = st{stdgen = newGen}
   in (value, st')

updatePos :: V3 (Maybe Double) -> GCode ()
updatePos (V3 mx my mz) = GCode $ do
  st <- get
  let (V3 x y z) = st.currentPosition
      newPos = V3 (fromMaybe x mx) (fromMaybe y my) (fromMaybe z mz)
  put $ st{currentPosition = newPos}

-------------------------------------------------------------------------------

section :: Text -> GCode a -> GCode a
section caption gc = do
  newline
  comment caption
  ret <- gc
  newline
  pure ret

newline :: GCode ()
newline = GCode $ tell [GCodeLine{cmd = Nothing, rawExtra = "", comment = Nothing}]

comment :: Text -> GCode ()
comment c = GCode $ do
  tell [GCodeLine{cmd = Nothing, rawExtra = "", comment = Just c}]

raw :: Text -> Text -> GCode ()
raw extra comm = GCode $ do
  tell [GCodeLine{cmd = Nothing, rawExtra = extra, comment = Just comm}]

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode ()
setUnits u = gCodeFromCmd $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

autoHome :: GCode ()
autoHome = gCodeFromCmd $ GAutoHome gcodeDef{skipIfTrusted = False}

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
pause seconds = gCodeFromCmd $ GDwell (gcodeDef{seconds = Just seconds})

--------------------------------------------------------------------------------

extrudeXY :: V2 Double -> GCode ()
extrudeXY v@(V2 x y) = do
  extrudeSpeed <- getExtrudeSpeed

  extrudeLength <- getExtrudeLength v

  gCodeFromCmd $
    GLinearMove
      gcodeDef
        { x = Just x
        , y = Just y
        , extrude = Just extrudeLength
        , feedrate = Just extrudeSpeed
        }

extrude :: Double -> GCode ()
extrude s = do
  extrudeSpeed <- getExtrudeSpeed
  gCodeFromCmd $
    GLinearMove
      gcodeDef
        { extrude = Just s
        , feedrate = Just extrudeSpeed
        }

moveXY :: V2 Double -> GCode ()
moveXY (V2 x y) = do
  speed <- getSpeed

  gCodeFromCmd $
    GLinearMove
      gcodeDef
        { x = Just x
        , y = Just y
        , feedrate = Just speed
        }

moveXYZ :: V3 Double -> GCode ()
moveXYZ (V3 x y z) = do
  speed <- getSpeed

  gCodeFromCmd $
    GLinearMove
      gcodeDef
        { x = Just x
        , y = Just y
        , z = Just z
        , feedrate = Just speed
        }

moveZ :: Double -> GCode ()
moveZ z = do
  speed <- getSpeed

  gCodeFromCmd $
    GLinearMove
      gcodeDef
        { z = Just z
        , feedrate = Just speed
        }

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

getSpeed :: GCode Int
getSpeed = do
  b <- isFirstLayers
  env <- ask
  pure $
    if b
      then env.moveSpeedFirstLayer
      else env.moveSpeed

getExtrudeLength :: V2 Double -> GCode Double
getExtrudeLength v = do
  extrudeMM <- getExtrudeMM
  st <- get
  let V3 curX curY _ = st.currentPosition
  let lineLength = distance (V2 curX curY) v
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

getExtrudeSpeed :: GCode Int
getExtrudeSpeed = do
  b <- isFirstLayers
  env <- ask
  pure $
    if b
      then env.extrudeSpeedFirstLayer
      else env.extrudeSpeed

stateUpdateExtrude :: Double -> PrintState -> PrintState
stateUpdateExtrude len st = case st.filament of
  [] -> st{filament = [("default", len)]}
  (color, used) : xs -> st{filament = (color, used + len) : xs}

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

  GCode $
    tell $
      pure $
        GCodeLine
          { cmd = Just cmd
          , rawExtra = ""
          , comment = Just (gcodeToComment cmd)
          }

newtype Frequency = Frequency {hz :: Int}
  deriving (Show, Eq, Generic)

newtype Duration = Duration {ms :: Int}
  deriving (Show, Eq, Generic)

newtype Temperature = Temperature {degrees :: Int}
  deriving (Show, Eq, Generic)

playTone :: Frequency -> Duration -> GCode ()
playTone (Frequency freq) (Duration dur) = do
  gCodeFromCmd $
    MPlayTone
      gcodeDef
        { frequency = Just freq
        , milliseconds = Just dur
        }

playTone_ :: GCode ()
playTone_ = playTone (Frequency 2600) (Duration 1)

setBedTemperature :: Temperature -> GCode ()
setBedTemperature (Temperature degrees) = do
  gCodeFromCmd $
    MSetBedTemperature
      gcodeDef
        { degrees = Just degrees
        }

setHotendTemperature :: Temperature -> GCode ()
setHotendTemperature (Temperature temp) = do
  gCodeFromCmd $
    MSSetHotendTemperature
      gcodeDef
        { degrees = Just temp
        }

waitForBedTemperature :: Temperature -> GCode ()
waitForBedTemperature (Temperature temp) = do
  gCodeFromCmd $
    MWaitForBedTemperature
      gcodeDef
        { degrees = Just temp
        }

waitForHotendTemperature :: Temperature -> GCode ()
waitForHotendTemperature (Temperature temp) = do
  gCodeFromCmd $
    MWaitForHotendTemperature
      gcodeDef
        { degrees = Just temp
        }

setPositionXYZ :: V3 Double -> GCode ()
setPositionXYZ (V3 x y z) = do
  gCodeFromCmd $
    GSetPosition
      gcodeDef
        { x = Just x
        , y = Just y
        , z = Just z
        }

setPositionXY :: V2 Double -> GCode ()
setPositionXY (V2 x y) = do
  gCodeFromCmd $
    GSetPosition
      gcodeDef
        { x = Just x
        , y = Just y
        }

changeColor :: Text -> GCode ()
changeColor colorName = do
  comment ("Change color to: " <> colorName)
  modify \st -> st{filament = (colorName, 0) : st.filament}

data FilamentStrategy = FilamentChange | PreparedFilament
  deriving (Show, Eq)