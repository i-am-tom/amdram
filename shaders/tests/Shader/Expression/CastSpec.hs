{-# LANGUAGE BlockArguments #-}

module Shader.Expression.CastSpec where

import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Hedgehog (annotateShow, evalIO, forAll)
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

    let expr = cast @GLint @GLfloat (lift x)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 (fromIntegral x) `isRoughly` V1 output

  it "V2 int -> V2 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]

    let expr = cast (ivec2 (lift x) (lift y))
    annotateShow expr

    output <- evalIO (renderExpr @(V2 GLfloat) renderer expr)
    output `isRoughly` fmap fromIntegral (V2 x y)

  it "V3 int -> V3 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    let expr = cast (ivec3 (lift x) (lift y) (lift z))
    annotateShow expr

    output <- evalIO (renderExpr @(V3 GLfloat) renderer expr)
    output `isRoughly` fmap fromIntegral (V3 x y z)

  it "V4 int -> V4 float" \renderer -> hedgehog do
    x <- forAll $ Gen.element [0, 1]
    y <- forAll $ Gen.element [0, 1]
    z <- forAll $ Gen.element [0, 1]

    let expr = cast (ivec4 (lift x) (lift y) (lift z) (lift 1))
    annotateShow expr

    output <- evalIO (renderExpr @(V4 GLfloat) renderer expr)
    output `isRoughly` fmap fromIntegral (V4 x y z 1)
