{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marlin.DSL where

import Control.Monad.Writer
import Linear (V2 (..), V3 (..))
import Marlin.Core
import Relude
import System.Random

-------------------------------------------------------------------------------
--- GCode
-------------------------------------------------------------------------------

data GCodeEnv = Env
  { moveSpeed :: Int,
    extrudeSpeed :: Int,
    moveSpeedFirstLayer :: Int,
    extrudeSpeedFirstLayer :: Int,
    bedTemperature :: Int,
    hotendTemperature :: Int,
    printSize :: V3 Double,
    parkingPosition :: V3 Double,
    sketchSize :: V3 Double,
    autoHomePosition :: V3 Double,
    layerHeight :: Double,
    firstLayerHeight :: Double,
    lineWidth :: Double,
    filamentDia :: Double,
    transpose :: V2 Double,
    retractLength :: Double,
    retractSpeed :: Int,
    zHop :: Double
  }

data PrintState = PrintState
  { currentLayer :: Int,
    currentPosition :: V3 Double,
    stdgen :: StdGen,
    filamentExtruded :: Double
  }
  deriving (Show, Eq)

initPrintState :: PrintState
initPrintState =
  PrintState
    { currentPosition = V3 145.50 94.00 1.56,
      filamentExtruded = 0,
      stdgen = mkStdGen 0,
      currentLayer = 0
    }

defaultGCodeEnv :: GCodeEnv
defaultGCodeEnv =
  Env
    { moveSpeed = 10000,
      extrudeSpeed = 2000,
      moveSpeedFirstLayer = 1000,
      extrudeSpeedFirstLayer = 800,
      bedTemperature = 60,
      hotendTemperature = 210,
      printSize = V3 225 225 280,
      parkingPosition = V3 0 225 120,
      sketchSize = V3 100 100 100,
      autoHomePosition = V3 145.50 94.00 1.56,
      layerHeight = 0.4,
      firstLayerHeight = 0.2,
      lineWidth = 0.4,
      filamentDia = 1.75,
      transpose = V2 0 0,
      retractLength = 1,
      retractSpeed = 1800,
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

updateExtruded :: Maybe Double -> GCode ()
updateExtruded extruded = GCode $ do
  st <- get
  let newExtruded = st.filamentExtruded + fromMaybe 0 extruded
  put $ st {filamentExtruded = newExtruded}

setExtruded :: Maybe Double -> GCode ()
setExtruded extruded = GCode $ do
  st <- get
  for_ extruded $ \e -> do
    put $ st {filamentExtruded = e}

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

class IsGCode a where
  toGCode :: a -> GCode ()

class (HasX a, HasY a) => HasXY a where
  setXY :: V2 Double -> a -> a
  setXY (V2 x y) obj = obj & setX x & setY y

class (HasX a, HasY a, HasZ a) => HasXYZ a where
  setXYZ :: V3 Double -> a -> a
  setXYZ (V3 x y z) obj = obj & setX x & setY y & setZ z

class HasX a where
  setX :: Double -> a -> a

class HasY a where
  setY :: Double -> a -> a

class HasZ a where
  setZ :: Double -> a -> a

class HasExtrude a where
  setExtrude :: Double -> a -> a

class HasSpeed a where
  setSpeed :: Int -> a -> a

class HasTargetTemperature a where
  setTargetTemperature :: Int -> a -> a

class HasSkipIfTrusted a where
  setSkipIfTrusted :: Bool -> a -> a

class HasFrequency a where
  setFrequency :: Int -> a -> a

class HasDuration a where
  setDuration :: Int -> a -> a

-------------------------------------------------------------------------------

linearMove :: LinearMove
linearMove =
  LinearMove
    { _x = Nothing,
      _y = Nothing,
      _z = Nothing,
      _e = Nothing,
      _f = Nothing
    }

instance IsGCode LinearMove where
  toGCode val = do
    updatePos (V3 val._x val._y val._z)
    updateExtruded val._e
    gCodeFromCmd $ GLinearMove val

instance HasX LinearMove where
  setX x' obj = obj {_x = Just x'}

instance HasY LinearMove where
  setY y' obj = obj {_y = Just y'}

instance HasZ LinearMove where
  setZ z' obj = obj {_z = Just z'}

instance HasExtrude LinearMove where
  setExtrude e' obj = obj {_e = Just e'}

instance HasSpeed LinearMove where
  setSpeed f' obj = obj {_f = Just f'}

instance HasXY LinearMove

instance HasXYZ LinearMove

-------------------------------------------------------------------------------

data Units = Millimeter | Inche

setUnits :: Units -> GCode ()
setUnits u = gCodeFromCmd $ case u of
  Millimeter -> GMillimeterUnits
  Inche -> GInchUnits

-------------------------------------------------------------------------------

setBedTemperature :: SetBedTemperature
setBedTemperature = SetBedTemperature {_temperature = Nothing}

instance IsGCode SetBedTemperature where
  toGCode = gCodeFromCmd . MSetBedTemperature

instance HasTargetTemperature SetBedTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

waitForBedTemperature :: WaitForBedTemperature
waitForBedTemperature = WaitForBedTemperature {_temperature = Nothing}

instance IsGCode WaitForBedTemperature where
  toGCode = gCodeFromCmd . MWaitForBedTemperature

instance HasTargetTemperature WaitForBedTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

setHotendTemperature :: SetHotendTemperature
setHotendTemperature = SetHotendTemperature {_temperature = Nothing}

instance IsGCode SetHotendTemperature where
  toGCode = gCodeFromCmd . MSSetHotendTemperature

instance HasTargetTemperature SetHotendTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

waitForHotendTemperature :: WaitForHotendTemperature
waitForHotendTemperature = WaitForHotendTemperature {_temperature = Nothing}

instance IsGCode WaitForHotendTemperature where
  toGCode = gCodeFromCmd . MWaitForHotendTemperature

instance HasTargetTemperature WaitForHotendTemperature where
  setTargetTemperature t obj = obj {_temperature = Just t}

-------------------------------------------------------------------------------

autoHome :: AutoHome
autoHome = AutoHome {_skipIfTrusted = False}

autoHome_ :: GCode ()
autoHome_ = toGCode autoHome

instance IsGCode AutoHome where
  toGCode val = do
    env <- ask
    gCodeFromCmd $ GAutoHome val
    updatePos (fmap Just env.parkingPosition)

instance HasSkipIfTrusted AutoHome where
  setSkipIfTrusted skip obj = obj {_skipIfTrusted = skip}

-------------------------------------------------------------------------------

playTone :: PlayTone
playTone = PlayTone {_frequency = Nothing, _duration = Nothing}

instance IsGCode PlayTone where
  toGCode = gCodeFromCmd . MPlayTone

instance HasFrequency PlayTone where
  setFrequency f obj = obj {_frequency = Just f}

instance HasDuration PlayTone where
  setDuration d obj = obj {_duration = Just d}

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
pause seconds = gCodeFromCmd (GPause seconds)

-------------------------------------------------------------------------------
--- Utils
-------------------------------------------------------------------------------

gCodeFromCmd :: GCodeCmd -> GCode ()
gCodeFromCmd cmd =
  GCode
    $ tell
    $ pure
    $ GCodeLine
      { cmd = (Just cmd),
        rawExtra = "",
        comment = (Just (gcodeToComment cmd))
      }

-------------------------------------------------------------------------------

setPosition :: SetPosition
setPosition = SetPosition Nothing Nothing Nothing Nothing

instance IsGCode SetPosition where
  toGCode sp = do
    updatePos (V3 sp._x sp._y sp._z)
    setExtruded sp._e
    gCodeFromCmd (GSetPosition sp)

instance HasX SetPosition where
  setX x' obj = obj {_x = Just x'}

instance HasY SetPosition where
  setY y' obj = obj {_y = Just y'}

instance HasZ SetPosition where
  setZ z' obj = obj {_z = Just z'}

instance HasExtrude SetPosition where
  setExtrude e' obj = obj {_e = Just e'}

instance HasXY SetPosition

instance HasXYZ SetPosition
