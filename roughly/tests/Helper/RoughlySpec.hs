{-# LANGUAGE BlockArguments #-}

module Helper.RoughlySpec where

import Hedgehog (Gen, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Roughly (isRoughly)
import Linear (V3)
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

genVector :: Gen (V3 Float)
genVector = sequence $ pure do
  Gen.float (Range.linearFrac 0 1000)

spec :: Spec
spec = do
  it "commutative float addition" $ hedgehog do
    x <- forAll genVector
    y <- forAll genVector

    (x + y) `isRoughly` (y + x)

  it "associative float addition" $ hedgehog do
    x <- forAll genVector
    y <- forAll genVector
    z <- forAll genVector

    (x + (y + z)) `isRoughly` ((x + y) + z)

  it "commutative float multiplication" $ hedgehog do
    x <- forAll genVector
    y <- forAll genVector

    (x * y) `isRoughly` (y * x)

  it "associative float multiplication" $ hedgehog do
    x <- forAll genVector
    y <- forAll genVector
    z <- forAll genVector

    (x * (y * z)) `isRoughly` ((x * y) * z)
