{-# LANGUAGE BlockArguments #-}

module Shader.Expression.VectorSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLboolean)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (bvec2, bvec3, bvec4, ivec2, ivec3, ivec4, lift, vec2, vec3, vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

genBoolean :: Gen GLboolean
genBoolean = Gen.element [1, 0]

spec :: SpecWith Renderer
spec = do
  it "bvec2" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      bvec2 (lift x) (lift y)

    V2 x y === output

  it "bvec3" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      bvec3 (lift x) (lift y) (lift z)

    V3 x y z === output

  it "bvec4" \renderer -> hedgehog do
    x <- forAll genBoolean
    y <- forAll genBoolean
    z <- forAll genBoolean

    output <- liftIO $ renderExpr renderer do
      bvec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 === output

  it "ivec2" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      ivec2 (lift x) (lift y)

    V2 x y === output

  it "ivec3" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      ivec3 (lift x) (lift y) (lift z)

    V3 x y z === output

  it "ivec4" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      ivec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 === output

  it "vec2" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec2 (lift x) (lift y)

    V2 x y `isRoughly` output

  it "vec3" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec3 (lift x) (lift y) (lift z)

    V3 x y z `isRoughly` output

  it "vec4" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output
