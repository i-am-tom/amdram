{-# LANGUAGE BlockArguments #-}

module Shader.Expression.CastSpec where

import Control.Monad.IO.Class (liftIO)
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Hedgehog (forAll)
import Hedgehog.Gen qualified as Gen
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (cast, ivec2, ivec3, ivec4, lift)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: SpecWith Renderer
spec = do
  it "int -> float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr renderer do
      cast @GLint @GLfloat (lift x)

    V1 (fromIntegral x) `isRoughly` V1 output

  it "V2 int -> V2 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr @(V2 GLfloat) renderer do
      cast (ivec2 (lift x) (lift y))

    fmap fromIntegral (V2 x y) `isRoughly` output

  it "V3 int -> V3 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr @(V3 GLfloat) renderer do
      cast (ivec3 (lift x) (lift y) (lift z))

    fmap fromIntegral (V3 x y z) `isRoughly` output

  it "V4 int -> V4 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    output <- liftIO $ renderExpr @(V4 GLfloat) renderer do
      cast (ivec4 (lift x) (lift y) (lift z) (lift 1))

    fmap fromIntegral (V4 x y z 1) `isRoughly` output
