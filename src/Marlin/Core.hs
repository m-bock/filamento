{-# LANGUAGE TupleSections #-}

module Marlin.Core where

import qualified Data.Map.Strict as Map
import Data.Semigroup (Arg)
import Marlin.Syntax
import Relude

data LinearMove = LinearMove
  { _x :: Maybe Double,
    _y :: Maybe Double,
    _z :: Maybe Double,
    _e :: Maybe Double,
    _f :: Maybe Int
  }
  deriving (Show, Eq)

data SetBedTemperature = SetBedTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data WaitForBedTemperature = WaitForBedTemperature
  { _temperature :: Maybe Int
  }
  deriving (Show, Eq)

data GCode
  = GMillimeterUnits
  | GInchUnits
  | -- | SetAbsolutePositioning
    -- | SetExtruderToAbsolute
    GSetBedTemperature SetBedTemperature
  | GWaitForBedTemperature WaitForBedTemperature
  | -- | SetHotendTemperature (Maybe Int)
    -- | WaitForHotendTemperature (Maybe Int)
    -- | AutoHome
    GLinearMove LinearMove
  deriving
    ( -- | SetPosition (Maybe Double)
      -- | ArcMove (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int)
      -- | SetHotendTemperatureOff
      -- | SetBedTemperatureOff
      -- | DisableSteppers
      -- | Comment String
      -- | PlayTone (Maybe Int) (Maybe Int)  -- ^ PlayTone: frequency (Hz), duration (ms)
      Show,
      Eq
    )

toGCodeLine :: GCode -> GCodeLine
toGCodeLine c = define $ case c of
  GMillimeterUnits ->
    ( G,
      21,
      [],
      "MillimeterUnits"
    )
  GInchUnits ->
    ( G,
      20,
      [],
      "InchUnits"
    )
  GSetBedTemperature (SetBedTemperature t) ->
    ( M,
      140,
      [j S ArgInt t],
      "SetBedTemperature"
    )
  GWaitForBedTemperature (WaitForBedTemperature t) ->
    ( M,
      190,
      [j S ArgInt t],
      "WaitForBedTemperature"
    )
  GLinearMove (LinearMove x y z e f) ->
    ( G,
      1,
      [ j X ArgDouble x,
        j Y ArgDouble y,
        j Z ArgDouble z,
        j E ArgDouble e,
        j F ArgInt f
      ],
      "LinearMove"
    )

j :: Argument -> (a -> ArgValue) -> Maybe a -> Maybe (Argument, ArgValue)
j arg mkVal val = case val of
  Just v -> Just (arg, mkVal v)
  Nothing -> Nothing

data CmdCode = G | M

cmdToChar :: CmdCode -> Char
cmdToChar = \case
  G -> 'G'
  M -> 'M'

data Argument = X | Y | Z | E | F | S
  deriving (Show, Eq)

renderArg :: Argument -> Char
renderArg = \case
  X -> 'X'
  Y -> 'Y'
  Z -> 'Z'
  E -> 'E'
  F -> 'F'
  S -> 'S'

define :: (CmdCode, Int, [Maybe (Argument, ArgValue)], Text) -> GCodeLine
define (ch, i, args, com) =
  GCodeLine
    { cmd = Just $ Cmd (cmdToChar ch) i argMap,
      comment = Just com
    }
  where
    argMap = Map.fromList $ map (\(arg, val) -> (renderArg arg, val)) $ catMaybes args
