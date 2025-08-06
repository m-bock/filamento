{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Filament
import qualified Filament2
import qualified Filament3
import qualified Filament4
import qualified Filament5
import Relude
import qualified Sketch03

main :: IO ()
main = do
  -- Filament.main

  Filament5.main

-- testEqualDistances

-- Sketch03.main