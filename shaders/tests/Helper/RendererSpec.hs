{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.RendererSpec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Zip (MonadZip (mzipWith))
import Data.Function (on)
import Data.Kind (Constraint, Type)
import Data.String.Interpolate (__i)
import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog ((===), Gen, MonadTest, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderSource, withRenderer)
import Linear (V4 (V4))
import Test.Hspec (Spec, SpecWith, around, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

gen_zero_to_one :: Gen Float
gen_zero_to_one = Gen.float (Range.linearFrac 0 1)

spec :: Spec
spec = around withRenderer do
  describe "withRenderer" do
    it "Renders a pixel correctly" \renderer ->
      hedgehog do
        input@(V4 x y z w) <- do
          x <- forAll gen_zero_to_one
          y <- forAll gen_zero_to_one
          z <- forAll gen_zero_to_one

          pure (V4 x y z 1)

        output <- liftIO do
          renderSource renderer [__i|
            \#version 410 core

            out vec4 colour;

            void main(void) {
              colour = vec4(
                #{ x },
                #{ y },
                #{ z },
                #{ w }
              );
            }
          |]

        input `isRoughly` output

-- | A "rough equality" check on two vectors. We're doing a lot of floating
-- point arithmetic, so we're going to end up with some sort of error.
isRoughly :: (MonadTest m, MonadZip f, Foldable f, Fractional x, Ord x, Show (f x)) => f x -> f x -> m ()
isRoughly = (===) `on` Roughly

type Roughly :: (Type -> Type) -> Type -> Type
newtype Roughly f x = Roughly (f x)
  deriving stock (Foldable, Traversable)
  deriving newtype (Show, Functor, Applicative)

instance (MonadZip f, Foldable f, Fractional x, Ord x) => Eq (Roughly f x) where
  Roughly xs == Roughly ys = and (mzipWith isCloseEnough xs ys)
    where
      isCloseEnough :: x -> x -> Bool
      isCloseEnough x y = abs (x - y) < recip 256
