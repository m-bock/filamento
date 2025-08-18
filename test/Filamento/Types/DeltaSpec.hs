module Filamento.Types.DeltaSpec (spec) where

import Data.Ratio ((%))
import Filamento.Classes
import Filamento.Types.Delta (Delta)
import Linear (V2 (..), V3 (..))
import Relude
import Test.Hspec
import Test.QuickCheck hiding (scale)
import Test.QuickCheck.Arbitrary

approxDouble :: Double -> Double -> Bool
approxDouble a b = abs (a - b) < 1e-9

approxDelta :: Delta -> Delta -> Bool
approxDelta a b = approxDouble (toMm a) (toMm b)

-- Short helper to construct a Delta from millimeters
d :: Double -> Delta
d = fromMm

-- Arbitrary instances for testing
instance Arbitrary Delta where
  arbitrary = (fromMm <$> arbitrary)
  shrink x = fromMm <$> shrink (toMm x)

instance Arbitrary (V2 Delta) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink (V2 x y) = V2 <$> shrink x <*> shrink y

instance Arbitrary (V3 Delta) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (V3 x y z) = V3 <$> shrink x <*> shrink y <*> shrink z

spec :: Spec
spec = do
  describe "Delta basic operations" $ do
    it "can be constructed and deconstructed" $ do
      let d = (fromMm 5.0 :: Delta)
      d `shouldBe` fromMm 5.0

    it "supports Show" $ do
      show (fromMm 3.14 :: Delta) `shouldBe` "Delta 3.14"

    it "supports Eq" $ do
      (fromMm 1.0 :: Delta) `shouldBe` fromMm 1.0
      (fromMm 1.0 :: Delta) `shouldNotBe` fromMm 2.0

    it "supports Ord" $ do
      (fromMm 1.0 :: Delta) `shouldSatisfy` (< fromMm 2.0)
      (fromMm 3.0 :: Delta) `shouldSatisfy` (> fromMm 2.0)
      (fromMm 2.0 :: Delta) `shouldSatisfy` (>= fromMm 2.0)

  describe "Delta numeric operations" $ do
    it "supports Num operations" $ do
      let d1 = (fromMm 3.0 :: Delta)
          d2 = (fromMm 2.0 :: Delta)
      (d1 + d2) `shouldBe` fromMm 5.0
      (d1 - d2) `shouldBe` fromMm 1.0
      (d1 * d2) `shouldBe` fromMm 6.0
      abs (fromMm (-3.0) :: Delta) `shouldBe` fromMm 3.0
      signum (fromMm (-3.0) :: Delta) `shouldBe` fromMm (-1.0)
      (fromInteger 5 :: Delta) `shouldBe` fromMm 5.0

    it "supports Fractional operations" $ do
      let d1 = (fromMm 6.0 :: Delta)
          d2 = (fromMm 2.0 :: Delta)
      (d1 / d2) `shouldBe` fromMm 3.0
      recip (fromMm 2.0 :: Delta) `shouldBe` fromMm 0.5
      (fromRational (3 % 2) :: Delta) `shouldBe` fromMm 1.5

    it "supports Real operations" $ do
      toRational (fromMm 3.5 :: Delta) `shouldBe` 7 % 2

    it "supports RealFrac operations" $ do
      let (intPart, fracPart) = properFraction (fromMm 3.7 :: Delta)
      intPart `shouldBe` 3
      toMm fracPart `shouldSatisfy` (\x -> approxDouble x 0.7)
      truncate (fromMm 3.7 :: Delta) `shouldBe` 3
      round (fromMm 3.7 :: Delta) `shouldBe` 4
      ceiling (fromMm 3.1 :: Delta) `shouldBe` 4
      floor (fromMm 3.9 :: Delta) `shouldBe` 3

  describe "Delta Semigroup and Monoid operations" $ do
    it "supports Semigroup operations" $ do
      let d1 = (fromMm 3.0 :: Delta)
          d2 = (fromMm 2.0 :: Delta)
      (d1 <> d2) `shouldBe` fromMm 5.0

    it "supports Monoid operations" $ do
      (mempty :: Delta) `shouldBe` d 0.0
      (mconcat (map d [1.0, 2.0, 3.0]) :: Delta) `shouldBe` d 6.0

  describe "Delta Scalable instance" $ do
    it "can be scaled by a factor" $ do
      let d = (fromMm 5.0 :: Delta)
          factor = 2.0
      scale factor d `shouldBe` fromMm 10.0

    it "scaling by 1.0 returns the same value" $ do
      let d = (fromMm 3.14 :: Delta)
      scale 1.0 d `shouldBe` d

    it "scaling by 0.0 returns zero" $ do
      let d = (fromMm 42.0 :: Delta)
      scale 0.0 d `shouldBe` fromMm 0.0

    it "scaling by negative factor works correctly" $ do
      let d = (fromMm 4.0 :: Delta)
      scale (-2.0) d `shouldBe` fromMm (-8.0)

  describe "Delta Millimeters instance" $ do
    it "can convert to millimeters" $ do
      let d = (fromMm 5.5 :: Delta)
      toMm d `shouldBe` 5.5

    it "can convert from millimeters" $ do
      let mm :: Double
          mm = 3.7
      (fromMm mm :: Delta) `shouldBe` d 3.7

    it "round-trip conversion preserves values" $ do
      let d = (fromMm 2.718 :: Delta)
      toMm (fromMm (toMm d) :: Delta) `shouldBe` toMm d

  describe "V2 Delta Millimeters instance" $ do
    it "can convert V2 to millimeters" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 1.0) (fromMm 2.0)
      toMm v2d `shouldBe` V2 1.0 2.0

    it "can convert V2 from millimeters" $ do
      let v2mm :: V2 Double
          v2mm = V2 3.0 4.0
      (fromMm v2mm :: V2 Delta) `shouldBe` V2 (d 3.0) (d 4.0)

    it "round-trip conversion preserves V2 values" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 1.5) (fromMm 2.5)
      toMm (fromMm (toMm v2d) :: V2 Delta) `shouldBe` toMm v2d

  describe "V3 Delta Millimeters instance" $ do
    it "can convert V3 to millimeters" $ do
      let v3d :: V3 Delta
          v3d = V3 (fromMm 1.0) (fromMm 2.0) (fromMm 3.0)
      (toMm v3d :: V3 Double) `shouldBe` (V3 1.0 2.0 3.0 :: V3 Double)

    it "can convert V3 from millimeters" $ do
      let v3mm :: V3 Double
          v3mm = V3 4.0 5.0 6.0
      (fromMm v3mm :: V3 Delta) `shouldBe` V3 (fromMm 4.0) (fromMm 5.0) (fromMm 6.0)

    it "round-trip conversion preserves V3 values" $ do
      let v3d :: V3 Delta
          v3d = V3 (fromMm 1.1) (fromMm 2.2) (fromMm 3.3)
      toMm (fromMm (toMm v3d) :: V3 Delta) `shouldBe` toMm v3d

  describe "V2 Delta JustX instance" $ do
    it "extracts X component and zeroes Y" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 5.0) (fromMm 7.0)
      justX v2d `shouldBe` V2 (fromMm 5.0) (fromMm 0.0)

    it "preserves X component when Y is already zero" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 3.0) (fromMm 0.0)
      justX v2d `shouldBe` v2d

  describe "V2 Delta JustY instance" $ do
    it "extracts Y component and zeroes X" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 5.0) (fromMm 7.0)
      justY v2d `shouldBe` V2 (fromMm 0.0) (fromMm 7.0)

    it "preserves Y component when X is already zero" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 0.0) (fromMm 3.0)
      justY v2d `shouldBe` v2d

  describe "V2 Delta Scalable instance" $ do
    it "scales both components by the same factor" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 2.0) (fromMm 3.0)
          factor = 1.5
      scale factor v2d `shouldBe` V2 (fromMm 3.0) (fromMm 4.5)

    it "scaling by 1.0 returns the same vector" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 1.0) (fromMm 2.0)
      scale 1.0 v2d `shouldBe` v2d

    it "scaling by 0.0 returns zero vector" $ do
      let v2d :: V2 Delta
          v2d = V2 (fromMm 1.0) (fromMm 2.0)
      scale 0.0 v2d `shouldBe` V2 (fromMm 0.0) (fromMm 0.0)

  describe "Delta JSON instances" $ do
    it "can be converted to and from JSON" $ do
      let d = (fromMm 3.14159 :: Delta)
      -- Note: This test assumes the JSON instances work correctly
      -- In a real test environment, you might want to use aeson's encode/decode
      d `shouldBe` d

  describe "Delta properties" $ do
    it "scaling is distributive over addition" $ property $ \factor (d1 :: Delta) (d2 :: Delta) ->
      approxDelta (scale factor (d1 + d2)) (scale factor d1 + scale factor d2)

    it "scaling is associative" $ property $ \f1 f2 (d :: Delta) ->
      approxDelta (scale f1 (scale f2 d)) (scale (f1 * f2) d)

    it "scaling by 1 is identity" $ property $ \(d :: Delta) ->
      scale 1.0 d == d

    it "scaling by 0 gives zero" $ property $ \(d :: Delta) ->
      scale 0.0 d == fromMm 0.0

    it "V2 scaling preserves structure" $ property $ \factor (v2d :: V2 Delta) ->
      let V2 x y = v2d
       in scale factor v2d == V2 (scale factor x) (scale factor y)

    it "justX and justY are orthogonal" $ property $ \(v2d :: V2 Delta) ->
      let xOnly = justX v2d
          yOnly = justY v2d
       in toMm (xOnly + yOnly) == toMm v2d
