{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Extra utilities for @barbies@.
module Barbies.ExtraSpec where

import Barbies (AllBF, ApplicativeB (bpure), ConstraintsB, FunctorB, TraversableB)
import Barbies qualified as B
import Barbies.Extra (align, alignC, unalign, unalignC)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Hedgehog (Gen, PropertyT, diff, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)

type CoordB :: (Type -> Type) -> Type
data CoordB f = CoordB
  { x :: f Float,
    y :: f Float,
    z :: f Float
  }
  deriving stock (Generic)
  deriving anyclass (ApplicativeB, ConstraintsB, FunctorB, TraversableB)

deriving instance (AllBF Eq f CoordB) => Eq (CoordB f)

deriving instance (AllBF Show f CoordB) => Show (CoordB f)

genCoordB :: (Applicative f) => Gen (CoordB f)
genCoordB = do
  x <- Gen.float (Range.linearFrac 0 1)
  y <- Gen.float (Range.linearFrac 0 1)
  z <- Gen.float (Range.linearFrac 0 1)

  pure
    CoordB
      { x = pure x,
        y = pure y,
        z = pure z
      }

-- TODO: replace with @isRoughly@ (@isRoughlyB@?)
(=~=) :: CoordB Identity -> CoordB Identity -> PropertyT IO ()
(=~=) as bs = x as =~ x bs *> y as =~ y bs *> z as =~ z bs
  where
    a =~ b = diff (abs (a - b)) (<) (recip 256)

spec :: Spec
spec = do
  it "aligns and unaligns happily" $ hedgehog do
    xyz <- forAll genCoordB

    let clean :: CoordB Maybe -> CoordB Maybe
        clean = unalign (const Nothing) . align \_ -> Just ()

    clean xyz === bpure Nothing

  it "alignCs and unalignCs happily" $ hedgehog do
    xyz <- forAll genCoordB

    let translate :: (Float -> Float) -> CoordB Identity -> CoordB Identity
        translate f = unalignC @((~) Float) id . fmap f . alignC @((~) Float) id

    translate pred (translate succ xyz) =~= xyz
