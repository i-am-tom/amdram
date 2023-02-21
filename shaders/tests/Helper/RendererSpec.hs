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
import Helper.Roughly (isRoughly)
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
