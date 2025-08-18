module Main (main) where

import Test.Hspec
import Relude

import qualified Filamento.Types.DeltaSpec as DeltaSpec

main :: IO ()
main = hspec $ do
  describe "Filamento.Types.Delta" DeltaSpec.spec
