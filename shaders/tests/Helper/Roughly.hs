{-# LANGUAGE DerivingStrategies #-}

module Helper.Roughly where

import Control.Monad.Zip (MonadZip (mzipWith))
import Data.Function (on)
import Data.Kind (Type)
import Hedgehog ((===), MonadTest)

-- | A "rough equality" check on two vectors. We're doing a lot of floating
-- point arithmetic, so we're going to end up with some sort of error.
isRoughly :: (MonadTest m, MonadZip f, Foldable f, Fractional x, Ord x, Show (f x)) => f x -> f x -> m ()
isRoughly = (===) `on` Roughly

-- | A type that implements "rough" equality, where "rough" precisely means
-- that two values are equal if no absolute component-wise difference is more
-- than a 256th of the total.
type Roughly :: (Type -> Type) -> Type -> Type
newtype Roughly f x = Roughly (f x)
  deriving stock (Foldable, Traversable)
  deriving newtype (Show, Functor, Applicative)

instance (MonadZip f, Foldable f, Fractional x, Ord x) => Eq (Roughly f x) where
  Roughly xs == Roughly ys = and (mzipWith isCloseEnough xs ys)
    where
      isCloseEnough :: x -> x -> Bool
      isCloseEnough x y = abs (x - y) <= maximum [ 1, abs x, abs y ] / 256
