module Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Console (log)
import TsBridge as TSB
import DTS as DTS
import GCodeViewer.Lib as GCodeViewer.Lib
import GCodeViewer.StateMachine as GCodeViewer.StateMachine

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ GCodeViewer.StateMachine.tsExports
    , GCodeViewer.Lib.tsExports
    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram