{-# LANGUAGE BlockArguments #-}

module Control.Comonad.Cofree.ExtraSpec (spec) where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Cofree.Extra (hush, note)
import Data.Fix (Fix (Fix))
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude (Int, Maybe, fmap, maybe, succ, ($), (>>=))

genFix :: Gen (Fix Maybe)
genFix = fmap Fix do
  Gen.maybe (Gen.small genFix)

spec :: Spec
spec = do
  let size :: Maybe (Cofree Maybe Int) -> Int
      size = maybe 0 \(n :< _) -> succ n

  it "hush / note" $ hedgehog do
    forAll genFix >>= \x -> hush (note size x) === x
