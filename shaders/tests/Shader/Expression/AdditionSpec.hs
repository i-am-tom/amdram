{-# LANGUAGE BlockArguments #-}

module Shader.Expression.AdditionSpec where

import Graphics.Rendering.OpenGL (GLfloat)
import Hedgehog (Gen, annotateShow, evalIO, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helper.Renderer (Renderer, renderExpr)
import Helper.Roughly (isRoughly)
import Linear (V1 (V1), V2 (V2), V3 (V3), V4 (V4))
import Shader.Expression (lift, vec2, vec3, vec4, (+))
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)
import Prelude hiding ((+))
import Prelude qualified

genZeroToHalf :: Gen GLfloat
genZeroToHalf = Gen.float (Range.linearFrac 0 0.5)

spec :: SpecWith Renderer
spec = do
  it "GLfloat + GLfloat = GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf

    let expr = lift x + lift y
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    V1 output `isRoughly` V1 (x Prelude.+ y)

  it "V2 GLfloat + GLfloat = V2 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    let expr = vec2 (lift x) (lift y) + lift r
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V2 (x Prelude.+ r) (y Prelude.+ r)

  it "GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf

    let expr = lift r + vec2 (lift x) (lift y)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V2 (x Prelude.+ r) (y Prelude.+ r)

  it "V2 GLfloat + V2 GLfloat = V2 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf

    let expr = vec2 (lift a) (lift b) + vec2 (lift c) (lift d)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V2 (a Prelude.+ c) (b Prelude.+ d)

  it "V3 GLfloat + GLfloat = V3 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    let expr = vec3 (lift x) (lift y) (lift z) + lift r
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V3 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r)

  it "GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf

    let expr = lift r + vec3 (lift x) (lift y) (lift z)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V3 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r)

  it "V3 GLfloat + V3 GLfloat = V3 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf
    e <- forAll genZeroToHalf
    f <- forAll genZeroToHalf

    let expr = vec3 (lift a) (lift b) (lift c) + vec3 (lift d) (lift e) (lift f)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V3 (a Prelude.+ d) (b Prelude.+ e) (c Prelude.+ f)

  it "V4 GLfloat + GLfloat = V4 GLfloat" \renderer -> hedgehog do
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf
    r <- forAll genZeroToHalf

    let expr = vec4 (lift x) (lift y) (lift z) (lift (1 - r)) + lift r
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1

  it "GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    r <- forAll genZeroToHalf
    x <- forAll genZeroToHalf
    y <- forAll genZeroToHalf
    z <- forAll genZeroToHalf

    let expr = lift r + vec4 (lift x) (lift y) (lift z) (lift (1 - r))
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V4 (x Prelude.+ r) (y Prelude.+ r) (z Prelude.+ r) 1

  it "V4 GLfloat + V4 GLfloat = V4 GLfloat" \renderer -> hedgehog do
    a <- forAll genZeroToHalf
    b <- forAll genZeroToHalf
    c <- forAll genZeroToHalf
    d <- forAll genZeroToHalf
    e <- forAll genZeroToHalf
    f <- forAll genZeroToHalf

    let expr =
          vec4 (lift a) (lift b) (lift c) (lift 0.5)
            + vec4 (lift d) (lift e) (lift f) (lift 0.5)
    annotateShow expr

    output <- evalIO (renderExpr renderer expr)
    output `isRoughly` V4 (a Prelude.+ d) (b Prelude.+ e) (c Prelude.+ f) 1
