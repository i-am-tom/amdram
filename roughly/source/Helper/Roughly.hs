{-# LANGUAGE DerivingVia #-}

module Helper.Roughly where

import Control.Monad.Zip (MonadZip (mzipWith))
import Data.Function (on)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint, Type)
import Hedgehog (MonadTest, (===))
import Linear (V1, V2, V3, V4)
import Test.Hspec (Expectation, shouldBe)

-- | A "rough equality" check on two vectors. We're doing a lot of floating
-- point arithmetic, so we're going to end up with some sort of error.
isRoughly :: (MonadTest m, Approximate x, Show x) => x -> x -> m ()
isRoughly = (===) `on` Roughly

-- | Like 'isRoughly', but specifically for Hspec expectations.
shouldRoughlyBe :: (Approximate x, Show x) => x -> x -> Expectation
shouldRoughlyBe = shouldBe `on` Roughly

-- | A type that implements "rough" equality, where "rough" precisely means
-- that two values are equal if no absolute component-wise difference is more
-- than a 256th of the total.
type Roughly :: Type -> Type
newtype Roughly x = Roughly {precisely :: x}
  deriving stock (Foldable, Show, Traversable)
  deriving (Functor, Applicative) via Identity

instance (Approximate x) => Eq (Roughly x) where
  Roughly x == Roughly y = x =~ y

-- | A class for rough equality. The laws are pretty loose here, but we'd at
-- least expect the following to hold:
--
-- prop> x =~ y === y =~ x
-- prop> x == y ==> x =~ y
-- prop> not (x == y) ==> x /= y
type Approximate :: Type -> Constraint
class Approximate x where
  -- | Are these two values roughly equal?
  (=~) :: x -> x -> Bool

instance Approximate Float where
  x =~ y = abs (x - y) <= maximum [1, abs x, abs y] / 256

instance Approximate Double where
  x =~ y = abs (x - y) <= maximum [1, abs x, abs y] / 256

instance (Approximate x) => Approximate (V1 x) where
  xs =~ ys = and (mzipWith (=~) xs ys)

instance (Approximate x) => Approximate (V2 x) where
  xs =~ ys = and (mzipWith (=~) xs ys)

instance (Approximate x) => Approximate (V3 x) where
  xs =~ ys = and (mzipWith (=~) xs ys)

instance (Approximate x) => Approximate (V4 x) where
  xs =~ ys = and (mzipWith (=~) xs ys)
