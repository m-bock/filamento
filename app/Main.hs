module Main where

import Control.Monad.Writer
import Marlin.Core
import Marlin.DSL
import Relude

sampleProgram :: GCode
sampleProgram = do
  waitForBedTemperature & setTargetTemperature 60 & toGCode

main :: IO ()
main = putTextLn (toText sampleProgram)