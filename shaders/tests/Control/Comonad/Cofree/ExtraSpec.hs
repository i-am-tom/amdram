{-# LANGUAGE BlockArguments #-}

module Control.Comonad.Cofree.ExtraSpec where

import Control.Comonad.Cofree.Extra (decapitate, decorate)
import Data.Fix (Fix (Fix))
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

genFix :: Gen (Fix [])
genFix = fmap Fix do
  Gen.list (Range.linear 0 10) do
    Gen.small genFix

spec :: Spec
spec = do
  it "decorate / decapitate" $ hedgehog do
    forAll genFix >>= \x ->
      decapitate (decorate length x) === x
