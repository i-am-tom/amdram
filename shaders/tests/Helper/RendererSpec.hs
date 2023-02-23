{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.RendererSpec where

import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (__i)
import Hedgehog (Gen, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (renderExpr, renderSource, withRenderer)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression (lift)
import Test.Hspec (Spec, around, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

genZeroToOne :: Gen Float
genZeroToOne = Gen.float (Range.linearFrac 0 1)

spec :: Spec
spec = around withRenderer do
  describe "withRenderer" do
    it "Renders a pixel correctly" \renderer ->
      hedgehog do
        input@(V4 x y z w) <- do
          x <- forAll genZeroToOne
          y <- forAll genZeroToOne
          z <- forAll genZeroToOne

          pure (V4 x y z 1)

        output <- liftIO do
          renderSource
            renderer
            [__i|
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

    it "Renders a constant expression" \renderer ->
      hedgehog do
        input <- do
          x <- forAll genZeroToOne
          y <- forAll genZeroToOne
          z <- forAll genZeroToOne

          pure (V4 x y z 1)

        output <- liftIO $ renderExpr renderer (lift input)
        input `isRoughly` output
