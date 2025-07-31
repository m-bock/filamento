{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Filament
import qualified Filament2
import qualified Filament3
import Relude
import qualified Sketch03

main :: IO ()
main = do
  -- Filament.main

  Filament3.main

-- Sketch03.main