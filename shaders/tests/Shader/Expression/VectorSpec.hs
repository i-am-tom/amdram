{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shader.Expression.VectorSpec where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.RendererSpec (genZeroToOne)
import Helper.Roughly (isRoughly)
import Linear (V4 (V4))
import Shader.Expression (cast, ivec2, ivec3, ivec4, lift, vec2, vec3, vec4)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding (fromInteger, fromRational)

spec :: SpecWith Renderer
spec = do
  it "ivec2" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.float (Range.linearFrac 0 1)

    output <- liftIO $ renderExpr renderer do
      let partial = ivec2 (lift x) (lift y)
      vec4 (cast partial.x) (cast partial.y) (lift z) (lift 1)

    V4 (fromIntegral x) (fromIntegral y) z 1 `isRoughly` output

  it "ivec3" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      let partial = ivec3 (lift x) (lift y) (lift z)
      vec4 (cast partial.x) (cast partial.y) (cast partial.z) (lift 1)

    fmap fromIntegral (V4 x y z 1) `isRoughly` output

  it "ivec4" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast (ivec4 (lift x) (lift y) (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output
  it "vec2" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial = vec2 (lift x) (lift y)
      vec4 partial.x partial.y (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output

  it "vec3" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      let partial = vec3 (lift x) (lift y) (lift z)
      vec4 partial.x partial.y partial.z (lift 1)

    V4 x y z 1 `isRoughly` output

  it "vec4" \renderer -> hedgehog do
    x <- forAll genZeroToOne
    y <- forAll genZeroToOne
    z <- forAll genZeroToOne

    output <- liftIO $ renderExpr renderer do
      vec4 (lift x) (lift y) (lift z) (lift 1)

    V4 x y z 1 `isRoughly` output
