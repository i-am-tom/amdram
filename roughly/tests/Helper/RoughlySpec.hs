{-# LANGUAGE BlockArguments #-}

module Helper.RoughlySpec where

import Hedgehog (Gen, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Roughly (isRoughly)
import Linear (V3)
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  it "commutative double addition" $ hedgehog do
    x <- forAll $ Gen.double (Range.linearFrac 0 1000)
    y <- forAll $ Gen.double (Range.linearFrac 0 1000)

    (x + y) `isRoughly` (y + x)

  it "associative double addition" $ hedgehog do
    x <- forAll $ Gen.double (Range.linearFrac 0 1000)
    y <- forAll $ Gen.double (Range.linearFrac 0 1000)
    z <- forAll $ Gen.double (Range.linearFrac 0 1000)

    (x + (y + z)) `isRoughly` ((x + y) + z)

  it "commutative double multiplication" $ hedgehog do
    x <- forAll $ Gen.double (Range.linearFrac 0 1000)
    y <- forAll $ Gen.double (Range.linearFrac 0 1000)

    (x * y) `isRoughly` (y * x)

  it "associative double multiplication" $ hedgehog do
    x <- forAll $ Gen.double (Range.linearFrac 0 1000)
    y <- forAll $ Gen.double (Range.linearFrac 0 1000)
    z <- forAll $ Gen.double (Range.linearFrac 0 1000)

    (x * (y * z)) `isRoughly` ((x * y) * z)

  it "commutative float addition" $ hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 1000)
    y <- forAll $ Gen.float (Range.linearFrac 0 1000)

    (x + y) `isRoughly` (y + x)

  it "associative float addition" $ hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 1000)
    y <- forAll $ Gen.float (Range.linearFrac 0 1000)
    z <- forAll $ Gen.float (Range.linearFrac 0 1000)

    (x + (y + z)) `isRoughly` ((x + y) + z)

  it "commutative float multiplication" $ hedgehog do
    x <- forAll $ Gen.float (Range.linearFrac 0 1000)
    y <- forAll $ Gen.float (Range.linearFrac 0 1000)

    (x * y) `isRoughly` (y * x)

  let vector :: Gen (V3 Float)
      vector = sequence $ pure do
        Gen.float (Range.linearFrac 0 1000)

  it "associative float multiplication" $ hedgehog do
    x <- forAll vector
    y <- forAll vector
    z <- forAll vector

    (x * (y * z)) `isRoughly` ((x * y) * z)

  it "commutative vector addition" $ hedgehog do
    x <- forAll vector
    y <- forAll vector

    (x + y) `isRoughly` (y + x)

  it "associative vector addition" $ hedgehog do
    x <- forAll vector
    y <- forAll vector
    z <- forAll vector

    (x + (y + z)) `isRoughly` ((x + y) + z)

  it "commutative vector multiplication" $ hedgehog do
    x <- forAll vector
    y <- forAll vector

    (x * y) `isRoughly` (y * x)

  it "associative vector multiplication" $ hedgehog do
    x <- forAll vector
    y <- forAll vector
    z <- forAll vector

    (x * (y * z)) `isRoughly` ((x * y) * z)
