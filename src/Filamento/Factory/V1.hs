module Filamento.Factory.V1 (
  printFilament,
  printFilament_,
  ConfigSrc (..),
)
where

import Data.List ((!!))
import qualified Data.Text as T
import Filamento
import Filamento.Lib
import Filamento.Math (addX, addY, justX, justY, subX)
import Linear (V3 (..))
import Linear.V2 (V2 (..), _x, _y)
import Relude
import Relude.Extra (un, wrap)

newtype Coord a b c = Coord a
  deriving (Show, Eq, Num)

type V2D = V2 Double

type V3D = V3 Double

type D3 = V3 Double

type X = Double

type Y = Double

type Z = Double

data Abs

data Rel

data Tube

data World

---

---

data ConfigSrc = ConfigSrc
  {
  }

data ConfigDrv = ConfigDrv
  {
  }

data Config = Config
  { src :: ConfigSrc
  , drv :: ConfigDrv
  }

drvConfig :: ConfigSrc -> Config
drvConfig = undefined

printFilament_ :: GCode ()
printFilament_ = undefined

printFilament :: (ConfigSrc -> ConfigSrc) -> GCode ()
printFilament = undefined