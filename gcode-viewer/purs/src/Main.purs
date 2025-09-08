module Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Console (log)
import TsBridge as TSB
import DTS as DTS
import GCodeViewer.State as GCodeViewer.State
import GCodeViewer.Lib as GCodeViewer.Lib

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ GCodeViewer.State.tsExports
    , GCodeViewer.Lib.tsExports
    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram